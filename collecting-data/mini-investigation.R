# =============================================================================
# Mini-investigation: Wikipedia API + Browser Automation
# Collecting Data: Web, APIs, Open Sources — E1493, Data Journalism, Simon Munzert
# =============================================================================

library(httr2)
library(selenider)
library(rvest)
library(tidyverse)

# -----------------------------------------------------------------------------
# Step 1: Use the Wikipedia search API to find articles
# -----------------------------------------------------------------------------

search_results <-
  request("https://en.wikipedia.org/w/rest.php/v1/search/page") |>
  req_url_query(q = "public policy school Europe", limit = 20) |>
  req_headers(`User-Agent`      = "DataJournalismCourse/1.0",
              `Accept-Encoding` = "gzip, deflate") |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  pluck("pages") |>
  select(key, title, description, excerpt)

# -----------------------------------------------------------------------------
# Step 2: Use browser automation to retrieve infobox data from article pages
# -----------------------------------------------------------------------------

session <- selenider_session("chromote", options = chromote_options(headless = TRUE, width = 1280, height = 900))

get_infobox <- function(article_key) {
  open_url(paste0("https://en.wikipedia.org/wiki/", article_key))
  Sys.sleep(2)

  html <- get_page_source()

  labels <- html |>
    html_elements(".infobox th") |>
    html_text(trim = TRUE)

  values <- html |>
    html_elements(".infobox td") |>
    html_text(trim = TRUE)

  if (length(labels) == 0) return(NULL)

  tibble(
    article = article_key,
    label   = labels[seq_len(min(length(labels), length(values)))],
    value   = values[seq_len(min(length(labels), length(values)))]
  )
}

# Collect infoboxes for the top 20 results
infoboxes <- map(
  head(search_results$key, 20),
  \(key) {
    message("Fetching: ", key)
    Sys.sleep(1)
    get_infobox(key)
  }
) |>
  compact() |>
  list_rbind()

close_session(session)

# -----------------------------------------------------------------------------
# Step 3: Analyse
# -----------------------------------------------------------------------------

infoboxes |>
  filter(str_detect(label, regex("established|founded|endow", ignore_case = TRUE))) |>
  pivot_wider(names_from = label, values_from = value) |>
  print(n = 20)
