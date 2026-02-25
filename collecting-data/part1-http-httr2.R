# =============================================================================
# Part 1: HTTP and httr2 Refresher
# Collecting Data: Web, APIs, Open Sources — E1493, Data Journalism, Simon Munzert
# =============================================================================

library(httr2)

# -----------------------------------------------------------------------------
# httr2 basics: build → modify → perform → extract
# -----------------------------------------------------------------------------

resp <- request("https://api.genderize.io") |>  # base URL
  req_url_query(name = "Anna") |>               # add ?name=Anna
  req_headers(Accept = "application/json") |>   # set a header
  req_timeout(10) |>                            # fail after 10s
  req_perform()                                 # fire the request

# Inspect the response
resp_status(resp)       # 200
resp_content_type(resp) # "application/json"
resp_headers(resp)      # all response headers

# Extract the body
resp_body_string(resp)  # raw JSON string
resp_body_json(resp)    # parsed into R list

# -----------------------------------------------------------------------------
# Reading response headers
# -----------------------------------------------------------------------------

# Single header
resp |> resp_header("Content-Type")

# All headers as a named list
resp |> resp_headers()
