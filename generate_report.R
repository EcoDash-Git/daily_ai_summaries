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

main_ids <- tibble::tribble(
  ~username,            ~main_id,
  "weave_db",           "1206153294680403968",
  "OdyseeTeam",         "1280241715987660801",
  "ardriveapp",         "1293193263579635712",
  "redstone_defi",      "1294053547630362630",
  "everpay_io",         "1334504432973848577",
  "decentlandlabs",     "1352388512788656136",
  "KYVENetwork",        "136377177683878784",
  "onlyarweave",        "1393171138436534272",
  "ar_io_network",      "1468980765211955205",
  "Permaswap",          "1496714415231717380",
  "communitylabs",      "1548502833401516032",
  "usewander",          "1559946771115163651",
  "apus_network",       "1569621659468054528",
  "fwdresearch",        "1573616135651545088",
  "perma_dao",          "1595075970309857280",
  "Copus_io",           "1610731228130312194",
  "basejumpxyz",        "1612781645588742145",
  "AnyoneFDN",          "1626376419268784130",
  "arweaveindia",       "1670147900033343489",
  "useload",            "1734941279379759105",
  "protocolland",       "1737805485326401536",
  "aoTheComputer",      "1750584639385939968",
  "ArweaveOasis",       "1750723327315030016",
  "aox_xyz",            "1751903735318720512",
  "astrousd",           "1761104764899606528",
  "PerplexFi",          "1775862139980226560",
  "autonomous_af",      "1777500373378322432",
  "Liquid_Ops",         "1795772412396507136",
  "ar_aostore",         "1797632049202794496",
  "FusionFiPro",        "1865790600462921728",
  "vela_ventures",      "1869466343000444928",
  "beaconwallet",       "1879152602681585664",
  "VentoSwap",          "1889714966321893376",
  "permawebjournal",    "1901592191065300993",
  "Botega_AF",          "1902521779161292800",
  "samecwilliams",      "409642632",
  "TateBerenbaum",      "801518825690824707",
  "ArweaveEco",         "892752981736779776"
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
  filter(publish_dt >= Sys.time() - lubridate::ddays(2)) |>
  distinct(tweet_id, .keep_all = TRUE)

df  <- tweets |> filter(tweet_type == "original")  # originals only

# 5 ── DAILY REPORT CONTENT ---------------------------------------------------

## 5.1  compact tweet lines
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

## 5.2  GPT summary prompt
headline_prompt <- glue(
  "Below is a collection of tweets, each on one line as\n",
  "Date | ER | Tweet text | URL.\n\n",

  "Write a bullet-point summary of concrete product launches, events, ",
  "or other activities **mentioned in the tweets**.\n\n",

 "• Begin every bullet with the date (`YYYY-MM-DD:`).\n",
"• Then give a concise summary (≤ 20 words).\n",
"• Finish with **exactly ONE** raw tweet URL in parentheses, no line-breaks,\n",
"  e.g. `2025-08-02: Beacon Wallet … (https://twitter.com/…)`.\n",
  "• Do NOT add any extra words around the URL (no “Link:”, no markdown).\n\n",

  big_text
)

dedup_bullets <- function(txt) {
  # split, trim, drop blank lines
  ln <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1]])
  ln <- ln[nzchar(ln)]

  # drop lines that are *only* "(url)"
  ln <- ln[ !grepl("^\\(https?://[^)]*\\)$", ln) ]

  # rebuild each bullet
  ln <- vapply(
    ln,
    \(b) {
      # collect all urls in the line
      urls <- stringr::str_extract_all(b, "https?://[^)\\s]+")[[1]]
      if (length(urls) == 0) return(b)          # no url → return as-is

      # keep only the very first url
      url1 <- urls[1]

      # strip every (…) block, squeeze whitespace
      b <- stringr::str_replace_all(b, "\\([^)]*\\)", "")
      b <- stringr::str_squish(b)

      sprintf("%s (%s)", b, url1)
    },
    character(1)
  )

  paste(ln, collapse = "\n")
}

## ------------------------------------------------------------------
## REPLACE your entire clean-up chain with just this one function
## ------------------------------------------------------------------
launches_summary <- ask_gpt(headline_prompt, max_tokens = 700) |>
                    dedup_bullets()


# -----------------------------------------------------------------------------
# Markdown → PDF → Supabase → Mailjet (steps identical, only the markdown
# content changed)
# -----------------------------------------------------------------------------
# Assemble final report (single section now)
writeLines(c(
  "# Daily Twitter Report",
  "",
  "## Launches & Activities",
  launches_summary
), "summary.md")

# Render PDF (pagedown)
chrome_path <- Sys.getenv("CHROME_BIN", pagedown::find_chrome())

pagedown::chrome_print(
  input   = "summary.md",
  output  = "summary_full.pdf",
  browser = chrome_path,
  extra_args = c("--headless=new", "--disable-gpu", "--no-sandbox")
)


if (!file.exists("summary_full.pdf"))
  stop("❌ PDF not generated – summary_full.pdf missing")

# Upload to Supabase – unchanged
object_path <- sprintf(
  "%s/summary_%s.pdf",
  format(Sys.Date(), "%Yw%V"),
  format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
)
upload_url <- sprintf("%s/storage/v1/object/%s/%s?upload=1",
                      SB_URL, SB_BUCKET, object_path)

request(upload_url) |>
  req_method("POST") |>
  req_headers(
    Authorization = sprintf("Bearer %s", SB_STORAGE_KEY),
    `x-upsert`     = "true",
    `Content-Type` = "application/pdf"
  ) |>
  req_body_file("summary_full.pdf") |>
  req_perform() |>
  resp_check_status()

cat("✔ Uploaded PDF to Supabase:", object_path, "\n")

# Email via Mailjet – unchanged
from_email <- if (str_detect(MAIL_FROM, "<.+@.+>"))
  str_remove_all(str_extract(MAIL_FROM, "<.+@.+>"), "[<>]") else MAIL_FROM
from_name  <- if (str_detect(MAIL_FROM, "<.+@.+>"))
  str_trim(str_remove(MAIL_FROM, "<.+@.+>$")) else "Report Bot"

request("https://api.mailjet.com/v3.1/send") |>
  req_auth_basic(MJ_API_KEY, MJ_API_SECRET) |>
  req_body_json(list(
    Messages = list(list(
      From      = list(Email = from_email, Name = from_name),
      To        = list(list(Email = MAIL_TO)),
      Subject   = "Daily Twitter Report – Launches & Activities",
      TextPart  = "Attached you'll find today's launch/activity summary.",
      Attachments = list(list(
        ContentType   = "application/pdf",
        Filename      = "daily_report.pdf",
        Base64Content = base64enc::base64encode("summary_full.pdf")
      ))
    ))
  )) |>
  req_perform() |>
  resp_check_status()

cat("📧  Report emailed via Mailjet\n")
