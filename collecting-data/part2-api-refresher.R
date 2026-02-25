# =============================================================================
# Part 2: API Refresher — JSON in R and Genderize.io
# Collecting Data: Web, APIs, Open Sources — E1493, Data Journalism, Simon Munzert
# =============================================================================

library(httr2)
library(jsonlite)
library(tidyverse)

# -----------------------------------------------------------------------------
# JSON basics
# -----------------------------------------------------------------------------

# Parse a JSON string
fromJSON('{"name": "Anna", "gender": "female", "probability": 0.97}')

# Convert R objects to JSON
toJSON(list(name = "Anna", country = "DE"), auto_unbox = TRUE)

# -----------------------------------------------------------------------------
# Single name lookup
# -----------------------------------------------------------------------------

resp <- request("https://api.genderize.io") |>
  req_url_query(name = "Anna") |>
  req_perform()

resp |>
  resp_body_string() |>
  fromJSON()
# $name        $gender    $probability  $count
# "Anna"       "female"   0.97          99847

# -----------------------------------------------------------------------------
# Checking your rate limit (from response headers)
# -----------------------------------------------------------------------------

list(
  limit     = resp |> resp_header("X-Rate-Limit-Limit"),
  remaining = resp |> resp_header("X-Rate-Limit-Remaining"),
  reset_in  = resp |> resp_header("X-Rate-Limit-Reset") |> as.integer() |> (\(s) paste(s, "seconds"))()
)

# -----------------------------------------------------------------------------
# Batch requests and a helper function
# -----------------------------------------------------------------------------

genderize <- function(names) {
  # Split into batches of 10 (API limit)
  batches <- split(names, ceiling(seq_along(names) / 10))

  map(batches, \(batch) {
    resp <- request("https://api.genderize.io") |>
      req_url_query(!!!setNames(as.list(batch), rep("name[]", length(batch)))) |>
      req_throttle(10 / 60) |>   # stay well within limits
      req_perform()

    resp |>
      resp_body_string() |>
      fromJSON()
  }) |>
    list_rbind()
}

# Try it
genderize(c("James", "Sophie", "Amara")) |>
  select(name, gender, probability, count)

# -----------------------------------------------------------------------------
# Mini-audit: prediction confidence by name origin
# -----------------------------------------------------------------------------

names_df <- tibble(
  name   = c("James", "Sophie", "Ingrid",   # W. European
             "Amara", "Chioma", "Kwame",     # Sub-Saharan African
             "Wei",   "Yuki",   "Ji-ho"),    # East Asian
  region = c(rep("W. European", 3),
             rep("Sub-Saharan African", 3),
             rep("East Asian", 3))
)

results <- genderize(names_df$name) |>
  select(name, gender, probability, count) |>
  left_join(names_df, by = "name")

ggplot(results, aes(x = reorder(name, probability), y = probability, fill = region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Genderize.io: Prediction confidence by name origin",
    x = NULL, y = "Probability", fill = "Region"
  ) +
  theme_minimal()
