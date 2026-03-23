# =============================================================================
# Part 4: Geocoding
# Working with Geodata in R — E1493 Data Journalism, Simon Munzert
# =============================================================================

library(tidyverse)
library(sf)
library(tidygeocoder)
library(nominatimlite)

# -----------------------------------------------------------------------------
# Option 1: tidygeocoder — geocode a data frame of addresses
# -----------------------------------------------------------------------------

cities <- tibble(
  name = c("München", "Hamburg", "Köln", "Frankfurt am Main", "Stuttgart")
)

# method = "osm" uses Nominatim (OpenStreetMap) — free, no API key needed.
# Rate limit: 1 request/second; tidygeocoder handles this automatically.
cities_geocoded <- cities |>
  geocode(
    address = name,
    method = "osm"
  )

cities_geocoded

# Available methods:
# "osm"     — Nominatim/OpenStreetMap. Free, global, 1 req/s limit.
# "arcgis"  — ArcGIS. Free, good international coverage.
# "census"  — US Census. US addresses only.
# "google"  — Google Maps. $5/1000 requests.
# "mapbox"  — Mapbox. 100k free/month.
# "here"    — HERE. 250k free/month.

# -----------------------------------------------------------------------------
# Option 2: nominatimlite — returns sf objects directly
# -----------------------------------------------------------------------------

munich_sf <- geo_lite_sf("München, Germany", limit = 1)
munich_sf

brewery_cities <- c("Bamberg, Germany", "Kulmbach, Germany", "Bayreuth, Germany")
brewery_cities_sf <- geo_lite_sf(brewery_cities, limit = 1)
brewery_cities_sf

# -----------------------------------------------------------------------------
# Geocoding a batch (template — do not run without the source data)
# -----------------------------------------------------------------------------

# unique_cities <- unique(cities_from_scrape)
#
# geocoded <- tibble(city = unique_cities) |>
#   geocode(
#     address = city,
#     method = "osm",
#     custom_query = list(countrycodes = "de")  # restrict to Germany
#   )

# -----------------------------------------------------------------------------
# Reverse geocoding — coordinates to addresses
# -----------------------------------------------------------------------------

coords <- tibble(
  lat = c(52.5128, 49.4263, 40.6892),
  lon = c(13.3866, 11.1208, -74.0471)
)

coords |>
  reverse_geocode(
    lat = lat, long = lon,
    method = "osm",
    address = "address_found"
  )
