# =============================================================================
# Part 4: Browser Automation — YouTube Search Audits
# Collecting Data: Web, APIs, Open Sources — E1493, Data Journalism, Simon Munzert
# =============================================================================

library(tidyverse)
library(rvest)
library(selenider)

# -----------------------------------------------------------------------------
# Step 1: Start a browser session
# -----------------------------------------------------------------------------

session <- selenider_session(
  "chromote",
  options = chromote_options(headless = FALSE, width = 1280, height = 900)
)

# -----------------------------------------------------------------------------
# Step 2: Why rvest alone fails on YouTube
# -----------------------------------------------------------------------------

# This returns almost no useful content — just a shell of HTML
# with no video titles or metadata
page <- read_html("https://www.youtube.com/results?search_query=climate+change")
page |> html_elements("h3") |> html_text()
# → character(0)   ← nothing there

# Now with browser automation:
query <- "climate change"

open_url(
  paste0("https://www.youtube.com/results?search_query=",
         URLencode(query, reserved = TRUE))
)

# Give the page time to load and the cookie popup to appear
Sys.sleep(3)

# Handle the cookie consent popup — click "Reject all" if it appears
reject_btn <- s('#content > div.body.style-scope.ytd-consent-bump-v2-lightbox > div.eom-buttons.style-scope.ytd-consent-bump-v2-lightbox > div:nth-child(1) > ytd-button-renderer:nth-child(1) > yt-button-shape > button > yt-touch-feedback-shape > div.yt-spec-touch-feedback-shape__fill')
if (is_present(reject_btn)) {
  elem_click(reject_btn)
  Sys.sleep(2)  # wait for popup to close and page to settle
}

Sys.sleep(2)

# -----------------------------------------------------------------------------
# Step 3: Find elements on the rendered page
# -----------------------------------------------------------------------------

# Each search result is wrapped in a ytd-video-renderer element
result_cards <- ss("ytd-video-renderer")  # ss() = find ALL matching elements
length(result_cards)  # how many results loaded?

# -----------------------------------------------------------------------------
# Step 4: Extract data from rendered HTML
# -----------------------------------------------------------------------------

html <- get_page_source()  # already returns an xml_document; no read_html() needed

titles <- html |>
  html_elements("ytd-video-renderer #video-title") |>
  html_text(trim = TRUE)

urls <- html |>
  html_elements("ytd-video-renderer #video-title") |>
  html_attr("href") |>
  (\(x) paste0("https://www.youtube.com", x))()

# The channel name node contains two yt-formatted-string elements (visible + screen-reader copy).
# We take every second element starting from the first to get one name per video.
channels_raw <- html |>
  html_elements("ytd-video-renderer ytd-channel-name yt-formatted-string") |>
  html_text(trim = TRUE)
channels <- channels_raw[seq(1, length(channels_raw), by = 2)]

metadata <- html |>
  html_elements("ytd-video-renderer #metadata-line span") |>
  html_text(trim = TRUE)

# Metadata comes in pairs: (view count, upload date) per video
view_counts  <- metadata[seq(1, length(metadata), by = 2)]
upload_dates <- metadata[seq(2, length(metadata), by = 2)]

results <- tibble(
  title    = titles,
  channel  = channels,
  views    = view_counts,
  uploaded = upload_dates,
  url      = urls
)

results

# -----------------------------------------------------------------------------
# Step 5: Collect results at scale with scrolling
# -----------------------------------------------------------------------------

# YouTube's search page uses infinite scroll: only ~20 results are rendered
# initially, and more are injected into the DOM as you scroll down. This
# function automates that process — it navigates to a query, handles the cookie
# popup, scrolls n_scrolls times (each scroll triggers a new batch of ~20
# results), then extracts all visible results from the fully rendered page.
# Running the same function on multiple queries lets you compare what YouTube
# surfaces for different search terms — the basis of an algorithmic audit.

collect_search_results <- function(query, n_scrolls = 3) {
  open_url(
    paste0("https://www.youtube.com/results?search_query=",
           URLencode(query, reserved = TRUE))
  )
  Sys.sleep(3)

  # Dismiss cookie popup if present
  reject_btn <- s('#content > div.body.style-scope.ytd-consent-bump-v2-lightbox > div.eom-buttons.style-scope.ytd-consent-bump-v2-lightbox > div:nth-child(1) > ytd-button-renderer:nth-child(1) > yt-button-shape > button > yt-touch-feedback-shape > div.yt-spec-touch-feedback-shape__fill')
  if (is_present(reject_btn)) {
    elem_click(reject_btn)
    Sys.sleep(2)
  }

  # Scroll to the last visible result card to trigger lazy-loading of the next batch.
  # YouTube's infinite scroll fires when the last card enters the viewport,
  # so scrolling to it is more reliable than a raw JS window.scrollBy().
  for (i in seq_len(n_scrolls)) {
    cards <- ss("ytd-video-renderer")
    elem_scroll_to(cards[[length(cards)]])
    Sys.sleep(2)  # wait for new cards to render before scrolling again
  }

  html <- get_page_source()

  titles   <- html |> html_elements("ytd-video-renderer #video-title") |> html_text(trim = TRUE)
  channels_raw <- html |> html_elements("ytd-video-renderer ytd-channel-name yt-formatted-string") |> html_text(trim = TRUE)
  channels <- channels_raw[seq(1, length(channels_raw), by = 2)]
  metadata <- html |> html_elements("ytd-video-renderer #metadata-line span") |> html_text(trim = TRUE)

  tibble(
    query   = query,
    rank    = seq_along(titles),
    title   = titles,
    channel = channels,
    views   = metadata[seq(1, length(metadata), by = 2)],
    url     = html |>
                html_elements("ytd-video-renderer #video-title") |>
                html_attr("href") |>
                (\(x) paste0("https://www.youtube.com", x))()
  )
}

# Compare two related queries — what does YouTube surface differently?
queries <- c("Klimawandel", "climate change")

all_results <- map_dfr(queries, \(q) {
  message("Searching: ", q)
  Sys.sleep(1)
  collect_search_results(q, n_scrolls = 3)
})

# Which channels appear for both queries vs. only one?
all_results |>
  count(query, channel, sort = TRUE) |>
  pivot_wider(names_from = query, values_from = n, values_fill = 0)

# -----------------------------------------------------------------------------
# Step 6: Close the session
# -----------------------------------------------------------------------------

close_session(session)
