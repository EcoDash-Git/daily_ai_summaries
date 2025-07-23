#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# generate_report.R – Scrape, summarise, render PDF, upload to Supabase,
#                     and email a **daily** Twitter report through Mailjet
# ---------------------------------------------------------------------------

# 0 ── PACKAGES ──────────────────────────────────────────────────────────────
required <- c(
  "tidyverse", "lubridate", "httr2", "httr", "jsonlite", "glue", "pagedown",
  "RPostgres", "DBI", "base64enc", "tidytext"
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

`%||%` <- function(a, b) if (nzchar(a)) a else b     # tiny helper

# 2 ── ENVIRONMENT VARIABLES -------------------------------------------------
SB_HOST        <- trim_env("SUPABASE_HOST")
SB_PORT        <- as.integer(trim_env("SUPABASE_PORT", "6543"))
SB_DB          <- trim_env("SUPABASE_DB")
SB_USER        <- trim_env("SUPABASE_USER")
SB_PWD         <- trim_env("SUPABASE_PWD")
SB_URL         <- trim_env("SUPABASE_URL")          # e.g. https://abcxyz.supabase.co
SB_STORAGE_KEY <- trim_env("SUPABASE_SERVICE_ROLE")
SB_BUCKET      <- trim_env("SB_BUCKET", "daily-reports")

OPENAI_KEY     <- trim_env("OPENAI_API_KEY")

MJ_API_KEY     <- trim_env("MJ_API_KEY")
MJ_API_SECRET  <- trim_env("MJ_API_SECRET")
MAIL_FROM      <- trim_env("MAIL_FROM")
MAIL_TO        <- trim_env("MAIL_TO")

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

twitter_raw <- DBI::dbReadTable(con, "twitter_raw") |> as_tibble()

# ── ACCOUNT → CANONICAL‑ID MAPPINGS ─────────────────────────────────────────
main_ids <- tribble(
  ~username,            ~main_id,
  "weave_db",           "1206153294680403968",
  # … (same table as before) …
  "ArweaveEco",         "892752981736779776"
)

# 4 ── PRE‑PROCESS TWEETS (24‑hour window) -----------------------------------
tweets <- twitter_raw |>
  left_join(main_ids, by = "username") |>
  mutate(
    is_rt_text = str_detect(text, "^RT @"),
    tweet_type = case_when(
      is_rt_text                                   ~ "retweet",
      user_id == main_id & !is_rt_text & str_detect(text, "https://t.co") ~ "quote",
      user_id == main_id                           ~ "original",
      TRUE                                         ~ "other"
    ),
    publish_dt = lubridate::ymd_hms(date, tz = "UTC"),
    text       = str_squish(text)
  ) |>
  filter(publish_dt >= Sys.time() - lubridate::ddays(1)) |>
  distinct(tweet_id, .keep_all = TRUE)

df  <- tweets |> filter(tweet_type == "original")
df2 <- tweets

# 5 ── SECTION 1 – DAILY ACTIVITY SUMMARY -----------------------------------
tweet_lines <- df |>
  mutate(line = glue(
    "{format(publish_dt, '%Y-%m-%d %H:%M')} | ",
    "ER={round(engagement_rate, 4)}% | ",
    "{str_replace_all(str_trunc(text, 200), '\\n', ' ')} | ",
    "{tweet_url}"
  )) |>
  pull(line)

big_text <- paste(tweet_lines, collapse = "\n")

prompt1 <- glue(
  "Below is a collection of tweets; each line is ",
  "URL | Date | Engagement Rate | Tweet text.\n\n",
  "Write ONE concise bullet‑point summary of what happened *today*.\n",
  "• **Headline** (≤20 words) plus the tweet’s date (YYYY‑MM‑DD).\n",
  "• Next line (indented two spaces) – copy the first 60 characters of the tweet ",
  "text exactly **and then paste the raw URL** (no brackets, no “Link”).\n\n",
  big_text
)

overall_summary <- ask_gpt(prompt1)

# 6 ── SECTION 2 – NUMERIC INSIGHTS, CONTENT TYPE, HASHTAGS ------------------
content_tbl <- df2 |>
  mutate(post_type = tweet_type |> str_to_title()) |>
  group_by(post_type) |>
  summarise(
    avg_ER    = mean(engagement_rate, na.rm = TRUE),
    avg_views = mean(view_count, na.rm = TRUE),
    .groups   = "drop"
  )

content_block <- content_tbl |>
  mutate(row = glue("{post_type}: ER={round(avg_ER,3)}%, views={round(avg_views)}")) |>
  pull(row) |>
  glue_collapse(sep = "\n")

hashtag_block <- df2 |>
  mutate(hashtag = str_extract_all(str_to_lower(text), "#\\w+")) |>
  unnest(hashtag) |>
  group_by(hashtag) |>
  summarise(
    n_tweets = n(),
    avg_ER   = mean(engagement_rate, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  filter(n_tweets >= 3) |>
  arrange(desc(avg_ER)) |>
  slice_head(n = 5) |>
  mutate(row = glue("{hashtag}: ER={round(avg_ER,3)}% (n={n_tweets})")) |>
  pull(row) |>
  glue_collapse(sep = "\n")

num_cols <- c("like_count","retweet_count","reply_count",
              "view_count","engagement_rate")

five_num <- df |>
  summarise(across(all_of(num_cols), \(x){
    q <- quantile(x, c(0,.25,.5,.75,1), na.rm = TRUE)
    glue("min={q[1]}, q1={q[2]}, med={q[3]}, q3={q[4]}, max={q[5]}")
  })) |>
  pivot_longer(everything()) |>
  glue_collapse(sep = "\n")

day_time <- df |>
  mutate(day  = wday(publish_dt, label = TRUE, abbr = FALSE, week_start = 1, locale = "C"),
         hour = hour(publish_dt)) |>
  group_by(day, hour) |>
  summarise(median_engagement = median(engagement_rate, na.rm = TRUE), .groups="drop") |>
  arrange(desc(median_engagement)) |>
  slice_head(n = 10) |>
  mutate(row = glue("{day}: hour={hour}, med_ER={round(median_engagement,3)}")) |>
  pull(row) |>
  glue_collapse(sep = "\n")

prompt2 <- glue(
"
You are an experienced social‑media analyst.

Each line in **Data A** has `YYYY-MM-DD HH:MM | ER=% | tweet_text`.

**Data B** five‑number summaries; **Data C** content types; **Data D** hashtags; **Data E** best‑time.

### Tasks
1. Key Numeric Insights – highest ER, distance to median, tweet text+link, spread/outliers.  
2. Content‑Type Performance – use Data C (give ER & views).  
3. Keyword / Hashtag Trends – 3‑5 terms with higher ER (use Data D).  
4. Best Times to Post – weekdays & 2‑hr windows (use Data E).

**Rules**: bullet points ≤ 12 words; dates `YYYY‑MM‑DD`; don’t invent numbers.

### Data A
{big_text}

### Data B
{five_num}

### Data C
{content_block}

### Data D
{hashtag_block}

### Data E
{day_time}
"
)

overall_summary2 <- ask_gpt(prompt2, max_tokens = 1200)

# 7 ── THEMES BY ENGAGEMENT TIER (safe against low variation) ---------------
vec <- df$engagement_rate |> na.omit()

if (length(unique(vec)) >= 3) {

  q    <- unique(quantile(vec, c(0, .33, .66, 1), na.rm = TRUE))
  labs <- c("Low", "Medium", "High")[seq_len(length(q) - 1)]

  df_tier <- df |>
    mutate(tier = cut(engagement_rate,
                      breaks = q,
                      labels = labs,
                      include.lowest = TRUE))

  tidy_tokens <- bind_rows(
    df_tier |> unnest_tokens(word, text),
    df_tier |> unnest_tokens(word, text, token = "ngrams", n = 2) |>
      separate_rows(word, sep = " ")
  ) |>
    filter(!word %in% c("https","t.co","rt"), !str_detect(word,"^\\d+$")) |>
    anti_join(stop_words, by = "word")

  tier_keywords <- tidy_tokens |>
    count(tier, word, sort = TRUE) |>
    bind_tf_idf(word, tier, n) |>
    filter(!is.na(tier)) |>
    group_by(tier) |>
    slice_max(tf_idf, n = 12, with_ties = FALSE) |>
    mutate(row = glue("{word} ({n})")) |>
    summarise(keywords = glue_collapse(row, sep = "; "), .groups = "drop")

  prompt3 <- glue(
    "You are a social‑media engagement analyst.\n\n",
    "### Keyword lists\n",
    glue_collapse(sprintf("• %s tier → %s",
                          tier_keywords$tier, tier_keywords$keywords), sep = "\n"),
    "\n\n### Tasks\n",
    "1. For each tier, name the *main theme(s)* in ≤ 100 words.\n",
    "2. Suggest one content strategy to move tweets from Low→Medium and Medium→High."
  )

  overall_summary3 <- ask_gpt(prompt3, temperature = 0.7, max_tokens = 500)
} else {
  overall_summary3 <- "_Not enough unique engagement data today to build tiered insights._"
}

# 8 ── DAILY ROUND‑UP --------------------------------------------------------
start_day <- floor_date(Sys.time() - hours(24), "hour")
end_day   <- Sys.time()

day_lines <- df |>
  filter(publish_dt >= start_day) |>
  mutate(
    line = glue("{format(publish_dt, '%Y-%m-%d %H:%M')} | ",
                "ER={round(engagement_rate,4)}% | ",
                "{str_trunc(text, 200)} | {tweet_url}")
  ) |>
  pull(line)

daily_prompt <- glue(
  "You are a social‑media analyst.\n\n",
  "Each line in **Data D** is `YYYY-MM-DD HH:MM | ER | snippet | URL`.\n\n",
  "### Tasks\n",
  "1. What happened in the last 24 hours ({format(start_day,'%Y-%m-%d %H:%M')}–{format(end_day,'%Y-%m-%d %H:%M')}).\n",
  "2. Any hints of tomorrow’s activity.\n",
  "• Bullet points ≤ 15 words; end each point with the URL.\n\n",
  "### Data D\n",
  glue_collapse(day_lines, sep = "\n")
)

overall_summary4 <- ask_gpt(daily_prompt, temperature = 0.4, max_tokens = 450)

# 9 ── LOCATE CHROME / CHROMIUM ---------------------------------------------
suppressMessages({
  chrome <- pagedown::find_chrome()            %||%
            Sys.which("chromium-browser")       %||%
            Sys.which("google-chrome")          %||%
            Sys.which("chromium")
  if (chrome == "") stop("Cannot find Chrome/Chromium on the runner")
  options(pagedown.chromium = chrome)
})

# 10 ── COMBINE, WRITE, RENDER PDF ------------------------------------------
make_md_links <- \(txt) {
  pattern <- "(?<!\\]\\()https?://\\S+"          # only raw URLs
  str_replace_all(txt, pattern, "[Link](\\0)")
}

final_report <- glue(
  "{overall_summary}\n\n{overall_summary2}\n\n{overall_summary3}\n\n",
  "## Daily Round‑up\n\n{overall_summary4}"
) |>
  str_replace_all("\\$", "\\\\$") |>
  make_md_links()

writeLines(c("# Daily Summary", "", final_report), "summary.md")

pagedown::chrome_print(
  "summary.md",
  output     = "summary_full.pdf",
  extra_args = "--no-sandbox"
)
# 11 ── UPLOAD TO SUPABASE ---------------------------------------------------
object_path <- sprintf(
  "%s/summary_%s.pdf",
  format(Sys.Date(), "%Y%m%d"),
  format(Sys.time(),  "%Y-%m-%d_%H-%M-%S")
)

upload_url <- sprintf(
  "%s/storage/v1/object/%s/%s?upload=1",
  SB_URL,
  SB_BUCKET,
  object_path
)

cat("Uploading to:", upload_url, "\n")

resp <- request(upload_url) |>
  req_method("POST") |>
  req_headers(
    Authorization = sprintf("Bearer %s", SB_STORAGE_KEY),
    `x-upsert`     = "true",
    `Content-Type` = "application/pdf"
  ) |>
  req_body_file("summary_full.pdf") |>
  req_error(is_error = \(x) FALSE)   # ← add this line
  |> req_perform()

cat("Status:", resp_status(resp), "\n")
cat("Body  :", resp_body_string(resp, encoding = "UTF-8"), "\n")

if (resp_status(resp) >= 300) {
  stop(sprintf("Upload failed – status %s", resp_status(resp)))
} else {
  cat("✔ Uploaded to Supabase:", object_path, "\n")
}

# 12 ── EMAIL VIA MAILJET ----------------------------------------------------
show_mj_error <- function(resp) {
  cat("\n↪ Mailjet response body:\n",
      resp_body_string(resp, encoding = "UTF-8"), "\n\n")
}

from_email <- if (str_detect(MAIL_FROM, "<.+@.+>")) {
  str_remove_all(str_extract(MAIL_FROM, "<.+@.+>"), "[<>]")
} else {
  MAIL_FROM
}

from_name  <- if (str_detect(MAIL_FROM, "<.+@.+>")) {
  str_trim(str_remove(MAIL_FROM, "<.+@.+>$"))
} else {
  "Report Bot"
}

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
  show_mj_error(mj_resp)
  stop(sprintf("Mailjet returned status %s", resp_status(mj_resp)))
} else {
  cat("📧  Mailjet response OK — report emailed\n")
}
