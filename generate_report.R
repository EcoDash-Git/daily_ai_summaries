#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# generate_report.R – scrape, summarise, render PDF, upload to Supabase,
#                     and email a DAILY Twitter report through Mailjet
# ---------------------------------------------------------------------------

# 0 ── PACKAGES ──────────────────────────────────────────────────────────────
required <- c(
  "tidyverse", "lubridate", "httr2", "httr", "jsonlite", "glue", "pagedown",
  "rmarkdown", "RPostgres", "DBI", "base64enc", "tidytext"
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
  # … (same table as before) …
  "ArweaveEco", "892752981736779776"
)

# 4 ── PRE‑PROCESS TWEETS (24‑hour window) -----------------------------------
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
    ## QUIET = TRUE avoids the warning spam & keeps bad rows as NA
    publish_dt = lubridate::ymd_hms(date, tz = "UTC", quiet = TRUE),
    text       = str_squish(text)
  ) |>
  filter(publish_dt >= Sys.time() - lubridate::ddays(1)) |>
  distinct(tweet_id, .keep_all = TRUE)

df  <- tweets |> filter(tweet_type == "original")
df2 <- tweets

# 5 ── ANALYSIS & MARKDOWN GENERATION ----------------------------------------
# (replace this with your existing GPT + numeric‑insight code)
# For illustration we just write a stub markdown file:
summary_md <- "summary.md"
writeLines(c(
  "# Daily Twitter digest",
  "",
  paste("*Tweets analysed:*", nrow(df2)),
  "",
  paste("*Original tweets (last 24 h):*", nrow(df))
), summary_md)

# 6 ── RENDER HTML VIA R Markdown -------------------------------------------
html_out <- "summary.html"
rmarkdown::render(
  input  = summary_md,
  output_file = html_out,
  quiet  = TRUE
)

# 7 ── PRINT TO PDF (pagedown) -----------------------------------------------
# Make sure pagedown finds Chrome; append --no-sandbox for CI
chrome_path <- pagedown::find_chrome()
cat("DEBUG Chrome binary:", chrome_path, "\n")

tryCatch({
  pagedown::chrome_print(
    input       = html_out,
    output      = "summary_full.pdf",
    extra_args  = c("--no-sandbox")
  )
}, error = function(e) {
  message("chrome_print() failed: ", e$message)
  quit(status = 1)
})

# 8 ── VERIFY PDF EXISTS ------------------------------------------------------
if (!file.exists("summary_full.pdf")) {
  stop("❌ PDF not generated – summary_full.pdf missing")
}

# 9 ── UPLOAD TO SUPABASE -----------------------------------------------------
object_path <- sprintf(
  "%s/summary_%s.pdf",
  format(Sys.Date(), "%Yw%V"),         # folder YYYYwWW
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

# 10 ── EMAIL VIA MAILJET -----------------------------------------------------
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
