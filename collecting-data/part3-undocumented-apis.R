# =============================================================================
# Part 3: Undocumented APIs — Wikipedia Search
# Collecting Data: Web, APIs, Open Sources — E1493, Data Journalism, Simon Munzert
# =============================================================================

library(httr2)
library(jsonlite)
library(tidyverse)

# -----------------------------------------------------------------------------
# Step 4: Raw curlconverter output (as generated — before cleanup)
# -----------------------------------------------------------------------------

wiki_out <-
  request("https://en.wikipedia.org/w/rest.php/v1/search/title") |>
  req_url_query(
    q     = "Hertie",
    limit = "10"
  ) |>
  req_headers(
    `User-Agent`      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:147.0) Gecko/20100101 Firefox/147.0",
    Accept            = "application/json",
    `Accept-Language` = "en-US,en;q=0.9",
    `Accept-Encoding` = "gzip, deflate",   # ← remove br/zstd: libcurl can't decode them
    Referer           = "https://en.wikipedia.org/wiki/Main_Page",
    `Sec-GPC`         = "1",
    Connection        = "keep-alive",
    # Cookie          = "..."  ← remove: cookies are session-specific and unnecessary
    `Sec-Fetch-Dest`  = "empty",
    `Sec-Fetch-Mode`  = "cors",
    `Sec-Fetch-Site`  = "same-origin",
    DNT               = "1"
  ) |>
  req_perform()

# -----------------------------------------------------------------------------
# Step 5: Minimal working request (stripped down)
# -----------------------------------------------------------------------------

wiki_out <-
  request("https://en.wikipedia.org/w/rest.php/v1/search/title") |>
  req_url_query(q = "Hertie", limit = 10) |>
  req_headers(
    `User-Agent`      = "DataJournalismCourse/1.0 (hertie-school.org)",
    Accept            = "application/json",
    `Accept-Encoding` = "gzip, deflate"
  ) |>
  req_perform()

resp_status(wiki_out)  # 200 = success

# -----------------------------------------------------------------------------
# Step 6: Parse the response
# -----------------------------------------------------------------------------

# Option A: manual with jsonlite — gives full control
wiki_out |>
  resp_body_string() |>
  fromJSON()

# Option B: simplifyVector = TRUE — auto-converts to data frame
wiki_df <-
  wiki_out |>
  resp_body_json(simplifyVector = TRUE) |>
  pluck("pages")

wiki_df

# Flatten the nested thumbnail column
wiki_df |>
  unnest_wider(thumbnail, names_sep = "_") |>
  select(id, title, description, thumbnail_url, thumbnail_width, thumbnail_height)

# -----------------------------------------------------------------------------
# Step 7: Explore the API further
# -----------------------------------------------------------------------------

# Full-text search (searches article bodies, not just titles)
request("https://en.wikipedia.org/w/rest.php/v1/search/page") |>
  req_url_query(q = "Hertie School", limit = 5) |>
  req_headers(`User-Agent`      = "DataJournalismCourse/1.0",
              `Accept-Encoding` = "gzip, deflate") |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  pluck("pages") |>
  select(title, description, excerpt)

# Paginate: use the `offset` parameter to retrieve the next page of results
page2 <-
  request("https://en.wikipedia.org/w/rest.php/v1/search/page") |>
  req_url_query(q = "Hertie School", limit = 5, offset = 5) |>
  req_headers(`User-Agent`      = "DataJournalismCourse/1.0",
              `Accept-Encoding` = "gzip, deflate") |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  pluck("pages")
