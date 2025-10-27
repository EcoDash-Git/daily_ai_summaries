#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# generate_report.R – scrape, summarise, render PDF, upload to Supabase,
#                     and (optionally) email a DAILY Twitter report
# ---------------------------------------------------------------------------

# 0 ── PACKAGES ──────────────────────────────────────────────────────────────
required <- c(
  "tidyverse", "lubridate", "httr2", "httr", "jsonlite", "glue",
  "pagedown", "rmarkdown", "RPostgres", "DBI", "base64enc", "tidytext"
)
invisible(lapply(required, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}))

# 1 ── HELPERS ───────────────────────────────────────────────────────────────
trim_env <- function(var, default = "") {
  val <- stringr::str_trim(Sys.getenv(var, unset = default))
  if (identical(val, "")) default else val
}

ask_gpt <- function(prompt, model = "gpt-4o-mini",
                    temperature = 0, max_tokens = 700, retries = 3) {
  for (k in seq_len(retries)) {
    resp <- tryCatch(
      httr2::request("https://api.openai.com/v1/chat/completions") |>
        httr2::req_method("POST") |>
        httr2::req_headers(Authorization = paste("Bearer", OPENAI_KEY)) |>
        httr2::req_body_json(list(
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
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform(),
      error = identity
    )
    if (!inherits(resp, "error") && httr2::resp_status(resp) == 200) {
      return(
        httr2::resp_body_json(resp)$choices[[1]]$message$content |>
          stringr::str_trim()
      )
    }
    Sys.sleep(2 ^ k)
  }
  stop("All OpenAI retries failed")
}

`%||%` <- function(a, b) if (nzchar(a)) a else b

# 2 ── ENVIRONMENT VARIABLES -------------------------------------------------
SEND_EMAIL     <- tolower(trim_env("SEND_EMAIL", "false")) %in% c("1","true","yes")

SB_HOST        <- trim_env("SUPABASE_HOST")
SB_PORT        <- as.integer(trim_env("SUPABASE_PORT", "6543"))
SB_DB          <- trim_env("SUPABASE_DB")
SB_USER        <- trim_env("SUPABASE_USER")
SB_PWD         <- trim_env("SUPABASE_PWD")
SB_URL         <- sub("/+$","", trim_env("SUPABASE_URL"))         # normalize
SB_STORAGE_KEY <- trim_env("SUPABASE_SERVICE_ROLE")
SB_BUCKET      <- trim_env("SB_BUCKET", "daily-reports")

OPENAI_KEY     <- trim_env("OPENAI_API_KEY")

MJ_API_KEY     <- trim_env("MJ_API_KEY")
MJ_API_SECRET  <- trim_env("MJ_API_SECRET")
MAIL_FROM      <- trim_env("MAIL_FROM")
MAIL_TO        <- trim_env("MAIL_TO")

# Require core creds. Mailjet only if emailing is enabled.
stopifnot(SB_HOST != "", SB_URL != "", SB_STORAGE_KEY != "", OPENAI_KEY != "")
if (SEND_EMAIL) stopifnot(MJ_API_KEY != "", MJ_API_SECRET != "", MAIL_FROM != "", MAIL_TO != "")

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

# ── ACCOUNT → CANONICAL-ID MAPPINGS ─────────────────────────────────────────
main_ids <- twitter_raw %>%
  filter(!is.na(user_id) & nzchar(user_id)) %>%
  group_by(username, user_id) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(username, desc(n)) %>%
  group_by(username) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(username, main_id = user_id)

# 4 ── PRE-PROCESS TWEETS (last ~48 h) ---------------------------------------
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
  filter(publish_dt >= Sys.time() - lubridate::ddays(2)) |>
  distinct(tweet_id, .keep_all = TRUE)

df <- tweets |> filter(tweet_type == "original")

# 5 ── DAILY REPORT CONTENT ---------------------------------------------------
## 5.1 compact tweet lines
tweet_lines <- df |>
  mutate(
    line = glue(
      "{format(date, '%Y-%m-%d %H:%M')} | ",
      "@{username} | ",
      "ER={round(engagement_rate, 4)}% | ",
      "{str_replace_all(str_trunc(text, 200), '\\n', ' ')} | ",
      "{tweet_url}"
    )
  ) |>
  pull(line)

big_text <- paste(tweet_lines, collapse = "\n")

## 5.2 GPT summary prompt
headline_prompt <- glue(
  "Below is a collection of tweets. Each line is\n",
  "Date | Account | ER | Tweet text | URL.\n\n",
  "Write bullet-point summaries of concrete product launches, events, ",
  "or other activities **mentioned in the tweets**.\n\n",
  "• Begin every bullet with the date **and the account in parentheses**.\n",
  "  e.g. 2025-08-07 (@redstone_defi): …\n",
  "• Then give a concise summary (≤ 20 words).\n",
  "• End the bullet with the tweet’s URL in parentheses — place it once, ",
  "  after the summary.\n",
  "• Do NOT add any extra words around the URL (no “Link:”, no markdown).\n\n",
  big_text
)

clean_gpt_output <- function(txt) {
  # squash "(url (url))"
  txt <- gsub("\\((https?://[^)\\s]+)\\s*\\(\\1\\)\\)", "(\\1)", txt, perl = TRUE)
  # drop junk lines
  keep <- function(l) {
    l <- trimws(l)
    !(l == "" || grepl("^https?://", l) || grepl("^\\(", l))
  }
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  paste(lines[vapply(lines, keep, logical(1))], collapse = "\n")
}

raw <- ask_gpt(headline_prompt, max_tokens = 700)
launches_summary <- clean_gpt_output(raw)

# wrap URLs in angle brackets so pandoc makes links
launches_summary <- gsub("\\((https?://[^)\\s]+)\\)", "(<\\1>)", launches_summary, perl = TRUE)
launches_summary <- gsub("(?m)^\\s*(https?://\\S+)\\s*$", "(<\\1>)", launches_summary, perl = TRUE)
launches_summary <- gsub("(https?://\\S+)$", "(<\\1>)", launches_summary, perl = TRUE)

# 6 ── Markdown → PDF → Supabase --------------------------------------------
writeLines(c(
  "<style>",
  "  a, a:visited, a.uri { color: #1a0dab !important; text-decoration: underline; }",
  "</style>",
  "",
  "# Daily Twitter Report",
  "",
  "## Launches & Activities",
  launches_summary
), "summary.md")

chrome_path <- Sys.getenv("CHROME_BIN", pagedown::find_chrome())

# Markdown → HTML
html_file <- tempfile(fileext = ".html")
rmarkdown::pandoc_convert(
  "summary.md",
  to     = "html4",
  from   = "markdown+tex_math_single_backslash",
  output = html_file,
  options = c("--standalone", "--section-divs", "--embed-resources",
              "--variable", "bs3=TRUE", "--variable", "theme=bootstrap")
)

# HTML → PDF
pagedown::chrome_print(
  input      = html_file,
  output     = "summary_full.pdf",
  browser    = chrome_path,
  extra_args = c("--headless=new", "--disable-gpu", "--no-sandbox")
)
if (!file.exists("summary_full.pdf"))
  stop("❌ PDF not generated – summary_full.pdf missing")

# Upload to Supabase
object_path <- sprintf("%s/summary_%s.pdf",
                       format(Sys.Date(), "%Yw%V"),
                       format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
upload_url <- sprintf("%s/storage/v1/object/%s/%s?upload=1",
                      SB_URL, SB_BUCKET, object_path)

httr2::request(upload_url) |>
  httr2::req_method("POST") |>
  httr2::req_headers(
    Authorization  = sprintf("Bearer %s", SB_STORAGE_KEY),
    `x-upsert`     = "true",
    `Content-Type` = "application/pdf"
  ) |>
  httr2::req_body_file("summary_full.pdf") |>
  httr2::req_perform() |>
  httr2::resp_check_status()

cat("✔ Uploaded PDF to Supabase:", object_path, "\n")

# 7 ── Email via Mailjet (optional) ------------------------------------------
if (SEND_EMAIL) {
  from_email <- if (stringr::str_detect(MAIL_FROM, "<.+@.+>"))
    stringr::str_remove_all(stringr::str_extract(MAIL_FROM, "<.+@.+>"), "[<>]")
  else MAIL_FROM
  from_name  <- if (stringr::str_detect(MAIL_FROM, "<.+@.+>"))
    stringr::str_trim(stringr::str_remove(MAIL_FROM, "<.+@.+>$"))
  else "Report Bot"

  httr2::request("https://api.mailjet.com/v3.1/send") |>
    httr2::req_auth_basic(MJ_API_KEY, MJ_API_SECRET) |>
    httr2::req_body_json(list(
      Messages = list(list(
        From        = list(Email = from_email, Name = from_name),
        To          = list(list(Email = MAIL_TO)),
        Subject     = "Daily Twitter Report – Launches & Activities",
        TextPart    = "Attached you'll find today's launch/activity summary.",
        Attachments = list(list(
          ContentType   = "application/pdf",
          Filename      = "daily_report.pdf",
          Base64Content = base64enc::base64encode("summary_full.pdf")
        ))
      ))
    )) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  cat("📧  Report emailed via Mailjet\n")
} else {
  cat("↪ Skipping email step (SEND_EMAIL=false). Report generated & uploaded only.\n")
}

