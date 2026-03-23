# =============================================================================
# Part 1: Importing Spatial Data
# Working with Geodata in R — E1493 Data Journalism, Simon Munzert
# =============================================================================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# -----------------------------------------------------------------------------
# Vector data with sf
# -----------------------------------------------------------------------------

# Country boundary as an sf object
germany <- ne_countries(
  scale = "medium",
  country = "Germany",
  returnclass = "sf"
)

class(germany)

# The geometry column
st_geometry(germany)

# CRS (coordinate reference system)
st_crs(germany)

# -----------------------------------------------------------------------------
# Reading files from disk
# -----------------------------------------------------------------------------

# Reading a shapefile
# districts <- st_read("data/german_districts.shp")

# Reading GeoJSON
# districts <- st_read("data/german_districts.geojson")

# Reading GeoPackage (modern, recommended format)
# districts <- st_read("data/german_districts.gpkg")

# -----------------------------------------------------------------------------
# German state boundaries
# -----------------------------------------------------------------------------

# German federal states (Bundesländer)
germany_states <- ne_states(country = "Germany", returnclass = "sf")

ggplot(germany_states) +
  geom_sf(fill = "gray95", color = "gray40") +
  theme_minimal(base_size = 14) +
  labs(title = "German federal states (Bundesländer)")

# -----------------------------------------------------------------------------
# Modifying and enriching sf objects
# -----------------------------------------------------------------------------

# (a) mutate() works just like on a plain data frame
germany_states <- germany_states |>
  mutate(name_upper = toupper(name))

# (b) Compute area of each state with st_area()
germany_states <- germany_states |>
  mutate(area_km2 = as.numeric(st_area(geometry) / 1e6))

germany_states |>
  st_drop_geometry() |>
  select(name, area_km2) |>
  arrange(desc(area_km2))

# (c) Join with an external non-spatial data frame
# Approximate 2023 population figures for German federal states
state_population <- tibble(
  name = c(
    "Bayern", "Niedersachsen", "Baden-Württemberg", "Nordrhein-Westfalen",
    "Brandenburg", "Mecklenburg-Vorpommern", "Sachsen-Anhalt",
    "Thüringen", "Sachsen", "Hessen", "Rheinland-Pfalz",
    "Schleswig-Holstein", "Saarland", "Hamburg", "Berlin",
    "Bremen"
  ),
  population = c(
    13.4, 8.1, 11.2, 18.1,
    2.5, 1.6, 2.2,
    2.1, 4.1, 6.4, 4.2,
    3.0, 1.0, 1.9, 3.7,
    0.7
  )
)

# Left-join: geometry stays in germany_states, population comes in
germany_states <- germany_states |>
  left_join(state_population, by = "name")

# (d) Note: the geometry column comes along automatically — no special handling needed
germany_states |>
  st_drop_geometry() |>
  select(name, area_km2, population) |>
  head(5)
