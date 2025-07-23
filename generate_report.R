#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# generate_report.R – scrape, summarise, render PDF, upload to Supabase,
#                     and email a DAILY Twitter report through Mailjet
# ---------------------------------------------------------------------------

# 0 ── PACKAGES ──────────────────────────────────────────────────────────────
required <- c(
  "tidyverse", "lubridate", "httr2", "httr", "jsonlite", "glue",
  "pagedown", "rmarkdown", "RPostgres", "DBI", "base64enc", "tidytext"
)
invisible(lapply(required, \(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, quiet = TRUE)
  library(pkg, character.only = TRUE)
}))

# 1 ── HELPERS ───────────────────────────────────────────────────────────────
trim_env <- \(var, default = "") {
  val <- stringr::str_trim(Sys.getenv(var, unset = default))
  if (identical(val, "")) default else val
}

ask_gpt <- function(prompt, model = "gpt-4o-mini",
                    temperature = 0, max_tokens = 700, retries = 3) {
  for (k in seq_len(retries)) {
    resp <- tryCatch(
      request("https://api.openai.com/v1/chat/completions") |>
        req_method("POST") |>
        req_headers(Authorization = paste("Bearer", OPENAI_KEY)) |>
        req_body_json(list(
          model       = model,
          temperature = temperature,
          max_tokens  = max_tokens,
          messages    = list(
            list(
              role    = "system",
              content = "You are a concise analyst. Summarise only concrete activities, events or product launches, in bullet points."
            ),
            list(role = "user", content = prompt)
          )
        )) |>
        req_retry(max_tries = 3) |>
        req_perform(),
      error = identity
    )
    if (!inherits(resp, "error") && resp_status(resp) == 200) {
      return(
        resp_body_json(resp)$choices[[1]]$message$content |>
          stringr::str_trim()
      )
    }
    Sys.sleep(2 ^ k)
  }
  stop("All OpenAI retries failed")
}

`%||%` <- function(a, b) if (nzchar(a)) a else b

# 2 ── ENVIRONMENT VARIABLES -------------------------------------------------
SB_HOST        <- trim_env("SUPABASE_HOST")
SB_PORT        <- as.integer(trim_env("SUPABASE_PORT", "6543"))
SB_DB          <- trim_env("SUPABASE_DB")
SB_USER        <- trim_env("SUPABASE_USER")
SB_PWD         <- trim_env("SUPABASE_PWD")
SB_URL         <- trim_env("SUPABASE_URL")          # https://<ref>.supabase.co
SB_STORAGE_KEY <- trim_env("SUPABASE_SERVICE_ROLE")
SB_BUCKET      <- trim_env("SB_BUCKET", "daily-reports")

OPENAI_KEY     <- trim_env("OPENAI_API_KEY")

MJ_API_KEY     <- trim_env("MJ_API_KEY")
MJ_API_SECRET  <- trim_env("MJ_API_SECRET")
MAIL_FROM      <- trim_env("MAIL_FROM")
MAIL_TO        <- trim_env("MAIL_TO")

# --- DEBUG – key sanity check ----------------------------------------------
cat("DEBUG SB_KEY length:", nchar(SB_STORAGE_KEY), "\n")
cat("DEBUG SB_KEY first 20:", substr(SB_STORAGE_KEY, 1, 20), "\n")

stopifnot(
  SB_HOST  != "", SB_URL != "", SB_STORAGE_KEY != "",
  OPENAI_KEY != "", MJ_API_KEY != "", MJ_API_SECRET != "",
  MAIL_FROM != "", MAIL_TO != ""
)

# 3 ── LOAD DATA FROM SUPABASE ----------------------------------------------
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = SB_HOST,
  port     = SB_PORT,
  dbname   = SB_DB,
  user     = SB_USER,
  password = SB_PWD,
  sslmode  = "require"
)
on.exit(DBI::dbDisconnect(con), add = TRUE)

twitter_raw <- DBI::dbReadTable(con, "twitter_raw") |> as_tibble()

# ── ACCOUNT → CANONICAL‑ID MAPPINGS ─────────────────────────────────────────
main_ids <- tribble(
  ~username,  ~main_id,
  "weave_db", "1206153294680403968",
  # … add the rest of your handles …
  "ArweaveEco", "892752981736779776"
)

# 4 ── PRE‑PROCESS TWEETS (last 24 h) ----------------------------------------
tweets <- twitter_raw |>
  left_join(main_ids, by = "username") |>
  mutate(
    is_rt_text = str_detect(text, "^RT @"),
    tweet_type = case_when(
      is_rt_text                                                    ~ "retweet",
      user_id == main_id & !is_rt_text & str_detect(text, "https://t.co") ~ "quote",
      user_id == main_id                                            ~ "original",
      TRUE                                                          ~ "other"
    ),
    publish_dt = lubridate::ymd_hms(date, tz = "UTC", quiet = TRUE),
    text       = str_squish(text)
  ) |>
  filter(publish_dt >= Sys.time() - lubridate::ddays(1)) |>
  distinct(tweet_id, .keep_all = TRUE)

df  <- tweets |> filter(tweet_type == "original")  # originals only
df2 <- tweets                                       # all types

# 5 ── DAILY REPORT CONTENT ---------------------------------------------------

## 5.1  Compact tweet lines ---------------------------------------------------
tweet_lines <- df |>
  mutate(
    line = glue(
      "{format(date, '%Y-%m-%d %H:%M')} | ",
      "ER={round(engagement_rate, 4)}% | ",
      "{str_replace_all(str_trunc(text, 200), '\\n', ' ')} | ",
      "{tweet_url}"
    )
  ) |>
  pull(line)

big_text <- paste(tweet_lines, collapse = "\n")

## 5.2  Content‑type performance (Data C) ------------------------------------
content_tbl <- df2 |>
  mutate(post_type = case_when(
    tweet_type == "quote"    ~ "Quote",
    tweet_type == "retweet"  ~ "Retweet",
    tweet_type == "original" ~ "Original",
    TRUE                     ~ "Other"
  )) |>
  group_by(post_type) |>
  summarise(
    avg_ER    = mean(engagement_rate,  na.rm = TRUE),
    avg_views = mean(view_count,       na.rm = TRUE),
    .groups   = "drop"
  )

content_block <- content_tbl |>
  mutate(row_txt = glue("{post_type}: ER={round(avg_ER,3)}%, views={round(avg_views)}")) |>
  pull(row_txt) |>
  glue_collapse(sep = "\n")

## 5.3  Keyword / hashtag block (Data D) -------------------------------------
keyword_tbl <- df |>
  mutate(hashtag = str_extract_all(str_to_lower(text), "#\\w+")) |>
  unnest(hashtag) |>
  group_by(hashtag) |>
  summarise(
    n_tweets = n(),
    avg_ER   = mean(engagement_rate, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  filter(n_tweets >= 2) |>
  arrange(desc(avg_ER)) |>
  slice_head(n = 5)

keyword_block <- keyword_tbl |>
  mutate(row_txt = glue("{hashtag}: ER={round(avg_ER,3)}% (n={n_tweets})")) |>
  pull(row_txt) |>
  glue_collapse(sep = "\n")

## 5.4  Five‑number summaries (Data B) ---------------------------------------
num_cols <- c("like_count", "retweet_count", "reply_count",
              "view_count", "engagement_rate")

five_num <- df |>
  summarise(across(all_of(num_cols), \(x) {
    q <- quantile(x, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)
    glue("min={q[1]}, q1={q[2]}, med={q[3]}, q3={q[4]}, max={q[5]}")
  })) |>
  pivot_longer(everything(), names_to = "metric", values_to = "stats") |>
  glue_collapse(sep = "\n")

## 5.5  Launches / activities headline ---------------------------------------
headline_prompt <- glue(
  "Below is a collection of tweets; each line is\n",
  "`YYYY-MM-DD HH:MM | ER=% | Tweet text | URL`.\n\n",
  "Write ONE concise bullet‑point summary …\n",
  "• **Headline** (≤ 20 words) plus the date (YYYY‑MM‑DD).\n",
  "• Next line (indented two spaces) → first 60 characters of the tweet, then raw URL.\n",
  "  **Do not wrap URL in brackets.**\n\n",
  big_text
)
overall_summary <- ask_gpt(headline_prompt, max_tokens = 700)

## 5.6  Numeric + content insights -------------------------------------------
insight_prompt <- glue(
"
You are an experienced social‑media analyst.

### Tasks for the last 24 h
1. **Key Numeric Insights**  
   • Highest ER, its distance to median, show tweet & date (use Data A/B).  
   • Median ER vs. Twitter median 0.015 %.  
   • Comment on spread/outliers (Data B only).

2. **Content‑type performance** – Use Data C only.  
3. **Keyword / Hashtag trends** – 3‑5 terms with higher ER (Data D only).

### Rules
* Bullet points ≤ 12 words.  
* Dates as `YYYY‑MM‑DD`.  
* Do not invent numbers.

### Data A
{big_text}

### Data B
{five_num}

### Data C
{content_block}

### Data D
{keyword_block}
"
)
overall_summary2 <- ask_gpt(insight_prompt, max_tokens = 900)

## 5.7  Engagement‑tier themes -----------------------------------------------
non_na <- sum(!is.na(df$engagement_rate))

if (non_na >= 3) {   # enough data → tertiles via ntile()
  df_tier <- df |>
    mutate(
      tier_num = dplyr::ntile(engagement_rate, 3),
      tier     = factor(c("Low", "Medium", "High")[tier_num],
                        levels = c("Low", "Medium", "High"))
    )
} else {             # not enough observations
  df_tier <- df |>
    mutate(tier = factor("Medium", levels = c("Low", "Medium", "High")))
}

uni <- df_tier |> select(tier, text) |> unnest_tokens(word, text)
bi  <- df_tier |> select(tier, text) |>
        unnest_tokens(word, text, token = "ngrams", n = 2) |>
        separate_rows(word, sep = " ")

tidy_tokens <- bind_rows(uni, bi) |>
  filter(
    !word %in% c("https", "t.co", "rt"),
    !str_detect(word, "^\\d+$")
  ) |>
  anti_join(tidytext::stop_words, by = "word")

tier_keywords <- tidy_tokens |>
  count(tier, word, sort = TRUE) |>
  bind_tf_idf(word, tier, n) |>
  group_by(tier) |>
  slice_max(tf_idf, n = 12, with_ties = FALSE) |>
  mutate(row = glue("{word} ({n})")) |>
  summarise(keywords = glue_collapse(row, sep = "; "), .groups = "drop")

theme_prompt <- glue(
  "You are a social‑media engagement analyst.\n\n",
  "Below are the 12 most distinctive keywords for each engagement tier:\n",
  glue_collapse(
    sprintf("• %s tier → %s",
            tier_keywords$tier,
            tier_keywords$keywords %||% "No keywords (insufficient data)"),
    sep = "\n"
  ),
  "\n\nTasks\n",
  "1. Summarise the main theme(s) per tier in ≤ 80 words.\n",
  "2. Suggest one content tip to move tweets up one tier.\n",
  "Do not invent numbers."
)
overall_summary3 <- ask_gpt(theme_prompt, max_tokens = 500)

## 5.8  Assemble markdown -----------------------------------------------------
final_report <- paste(
  overall_summary,   # launches / headline
  "\n\n",
  overall_summary2,  # numeric + content insights
  "\n\n",
  overall_summary3,  # themes & strategy
  sep = ""
)

# escape $ so LaTeX (pagedown) is happy
final_report <- str_replace_all(final_report, "\\$", "\\\\$")

writeLines(
  c("# Daily Twitter Report", "", final_report),
  "summary.md"
)

## 5.9  Render PDF ------------------------------------------------------------
pagedown::chrome_print(
  input      = "summary.md",
  output     = "summary_full.pdf",
  extra_args = c("--no-sandbox")
)

# 6 ── VERIFY PDF EXISTS ------------------------------------------------------
if (!file.exists("summary_full.pdf")) {
  stop("❌ PDF not generated – summary_full.pdf missing")
}

# 7 ── UPLOAD TO SUPABASE -----------------------------------------------------
object_path <- sprintf(
  "%s/summary_%s.pdf",
  format(Sys.Date(), "%Yw%V"),                    # folder YYYYwWW
  format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
)

upload_url <- sprintf(
  "%s/storage/v1/object/%s/%s?upload=1",
  SB_URL, SB_BUCKET, object_path
)

resp <- request(upload_url) |>
  req_method("POST") |>
  req_headers(
    Authorization = sprintf("Bearer %s", SB_STORAGE_KEY),
    `x-upsert`     = "true",
    `Content-Type` = "application/pdf"
  ) |>
  req_body_file("summary_full.pdf") |>
  req_error(is_error = \(x) FALSE) |>
  req_perform()

if (resp_status(resp) >= 300) stop("Upload failed – status ", resp_status(resp))
cat("✔ Uploaded to Supabase:", object_path, "\n")

# 8 ── EMAIL VIA MAILJET ------------------------------------------------------
from_email <- if (str_detect(MAIL_FROM, "<.+@.+>"))
  str_remove_all(str_extract(MAIL_FROM, "<.+@.+>"), "[<>]") else MAIL_FROM
from_name  <- if (str_detect(MAIL_FROM, "<.+@.+>"))
  str_trim(str_remove(MAIL_FROM, "<.+@.+>$")) else "Report Bot"

mj_resp <- request("https://api.mailjet.com/v3.1/send") |>
  req_auth_basic(MJ_API_KEY, MJ_API_SECRET) |>
  req_body_json(list(
    Messages = list(list(
      From      = list(Email = from_email, Name = from_name),
      To        = list(list(Email = MAIL_TO)),
      Subject   = "Daily Twitter Report",
      TextPart  = "Attached you'll find the daily report in PDF.",
      Attachments = list(list(
        ContentType   = "application/pdf",
        Filename      = "daily_report.pdf",
        Base64Content = base64enc::base64encode("summary_full.pdf")
      ))
    ))
  )) |>
  req_error(is_error = \(x) FALSE) |>
  req_perform()

if (resp_status(mj_resp) >= 300) {
  cat("\n↪ Mailjet response body:\n",
      resp_body_string(mj_resp, encoding = "UTF-8"), "\n\n")
  stop("Mailjet returned status ", resp_status(mj_resp))
}
cat("📧  Mailjet response OK — report emailed\n")

