# =============================================================================
# Part 3: Transformations and Spatial Overlay
# Working with Geodata in R — E1493 Data Journalism, Simon Munzert
# =============================================================================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Re-create base objects (or source part1-importing-spatial-data.R)
germany        <- ne_countries(scale = "medium", country = "Germany", returnclass = "sf")
germany_states <- ne_states(country = "Germany", returnclass = "sf") |>
  mutate(area_km2 = as.numeric(st_area(geometry) / 1e6))

# -----------------------------------------------------------------------------
# Loading brewery data
# -----------------------------------------------------------------------------

load("data/coords_breweries.RData")

breweries <- coords_df |>
  filter(
    !is.na(lon), !is.na(lat),
    lon >= 5.5, lon <= 15.5,
    lat >= 47, lat <= 55.5
  )

nrow(breweries)

# -----------------------------------------------------------------------------
# Converting a data frame to an sf object
# -----------------------------------------------------------------------------

breweries_sf <- st_as_sf(
  breweries,
  coords = c("lon", "lat"),
  crs = 4326  # WGS 84
)

class(breweries_sf)
head(breweries_sf)

# Quick map with projected CRS
ggplot() +
  geom_sf(data = st_transform(germany, 25832), fill = "gray95") +
  geom_sf(data = st_transform(breweries_sf, 25832), color = "red", size = 0.4, alpha = 0.5) +
  coord_sf(crs = 25832) +
  theme_minimal(base_size = 14) +
  labs(title = "Breweries in Germany")

# -----------------------------------------------------------------------------
# Spatial join: which state is each brewery in?
# -----------------------------------------------------------------------------

breweries_with_state <- st_join(breweries_sf, germany_states["name"])
head(breweries_with_state)

# Count per state
breweries_per_state <- breweries_with_state |>
  st_drop_geometry() |>
  count(name, sort = TRUE) |>
  rename(state = name, n_breweries = n)

breweries_per_state

# Merge back and map with discretised bins
states_with_breweries <- germany_states |>
  left_join(breweries_per_state, by = c("name" = "state")) |>
  mutate(
    n_breweries = replace_na(n_breweries, 0),
    brewery_bin = cut(
      n_breweries,
      breaks = c(-Inf, 10, 50, 100, 200, Inf),
      labels = c("0-10", "11-50", "51-100", "101-200", "201+"),
      right = TRUE
    )
  )

ggplot(states_with_breweries) +
  geom_sf(aes(fill = brewery_bin), color = "white") +
  scale_fill_brewer(palette = "YlOrRd", name = "Breweries", na.value = "gray90") +
  coord_sf(crs = 25832) +
  theme_minimal(base_size = 14) +
  labs(title = "Number of breweries by German state", caption = "Source: biermap24.de")

# -----------------------------------------------------------------------------
# Buffering, intersection, and other geometric operations
# Note: reproject to a metric CRS before any distance/area operation!
# dist = 50000 means 50 km only when coordinates are in metres.
# -----------------------------------------------------------------------------

breweries_proj    <- st_transform(breweries_sf, 25832)
germany_states_proj <- st_transform(germany_states, 25832)

# 50 km buffer around Berlin
berlin_point  <- st_sfc(st_point(c(13.405, 52.52)), crs = 4326) |> st_transform(25832)
berlin_buffer <- st_buffer(berlin_point, dist = 50000)

# Breweries within 50 km of Berlin
breweries_near_berlin <- st_intersection(breweries_proj, berlin_buffer)
cat("Breweries within 50 km of Berlin:", nrow(breweries_near_berlin), "\n")

# Map the buffer
ggplot() +
  geom_sf(data = germany_states_proj, fill = "gray95", color = "gray60") +
  geom_sf(data = berlin_buffer, fill = "steelblue", alpha = 0.2, color = "steelblue") +
  geom_sf(data = breweries_proj, color = "red", size = 0.3, alpha = 0.3) +
  geom_sf(data = breweries_near_berlin, color = "darkred", size = 1.5) +
  coord_sf(
    crs = 25832,
    xlim = st_bbox(berlin_buffer)[c(1, 3)] + c(-100000, 100000),
    ylim = st_bbox(berlin_buffer)[c(2, 4)] + c(-100000, 100000)
  ) +
  theme_minimal(base_size = 14) +
  labs(title = "Breweries within 50 km of Berlin", subtitle = "Blue circle = 50 km buffer")

# -----------------------------------------------------------------------------
# Key sf transformation functions (reference — not meant to be run as-is)
# -----------------------------------------------------------------------------

# Predicates
# st_intersects(x, y)              # do geometries intersect?
# st_within(x, y)                  # is x fully inside y?
# st_contains(x, y)                # does x fully contain y?
# st_is_within_distance(x, y, dist)

# Operations (return new geometries)
# st_intersection(x, y)            # clip x to y
# st_difference(x, y)              # x minus y
# st_union(x)                      # dissolve all into one
# st_buffer(x, dist)               # expand by distance (needs metric CRS)
# st_centroid(x)                   # centre of polygon

# Measurements
# st_distance(x, y)                # pairwise distances
# st_area(x)                       # polygon areas
# st_length(x)                     # line lengths

# Joins
# st_join(x, y)                    # spatial left join
# st_nearest_feature(x, y)         # nearest neighbour index
