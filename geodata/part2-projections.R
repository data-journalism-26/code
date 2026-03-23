# =============================================================================
# Part 2: Projections
# Working with Geodata in R — E1493 Data Journalism, Simon Munzert
# =============================================================================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

# Run part1 first, or re-create the objects needed:
germany        <- ne_countries(scale = "medium", country = "Germany", returnclass = "sf")
germany_states <- ne_states(country = "Germany", returnclass = "sf")
world          <- ne_countries(scale = "medium", returnclass = "sf")

# -----------------------------------------------------------------------------
# Mercator vs Equal Earth: global distortion comparison
# -----------------------------------------------------------------------------

p_mercator <- ggplot(world) +
  geom_sf(fill = "gray80", color = "white", linewidth = 0.1) +
  coord_sf(crs = "+proj=merc", ylim = c(-15000000, 18000000)) +
  theme_void(base_size = 11) +
  labs(title = "Mercator (EPSG:3857)",
       subtitle = "Distorts area")

p_eqearth <- ggplot(world) +
  geom_sf(fill = "gray80", color = "white", linewidth = 0.1) +
  coord_sf(crs = "+proj=eqearth") +
  theme_void(base_size = 11) +
  labs(title = "Equal Earth",
       subtitle = "Preserves relative areas faithfully")

wrap_elements(p_mercator) + wrap_elements(p_eqearth)

# -----------------------------------------------------------------------------
# Seeing the world as a globe (orthographic projection)
# -----------------------------------------------------------------------------

ortho_crs <- "+proj=ortho +lat_0=48 +lon_0=10"

globe_disc <- st_sfc(st_point(c(0, 0)), crs = ortho_crs) |>
  st_buffer(6370000)  # mean Earth radius in metres

ggplot() +
  geom_sf(data = globe_disc, fill = "#c8dff0", color = "grey55", linewidth = 0.6) +
  geom_sf(data = world, fill = "#e0e0e0", color = "white", linewidth = 0.15) +
  coord_sf(crs = ortho_crs) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.title    = element_text(color = "grey20", face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = "grey50", size = 10, hjust = 0.5)
  ) +
  labs(
    title    = "The world as a globe",
    subtitle = "Orthographic projection centred on central Europe"
  )

# -----------------------------------------------------------------------------
# Checking and setting CRS
# -----------------------------------------------------------------------------

st_crs(germany)
st_crs(germany)$epsg

# -----------------------------------------------------------------------------
# Reprojecting data
# -----------------------------------------------------------------------------

# Reproject to ETRS89 / UTM zone 32N (official German reference system)
germany_proj <- st_transform(germany, crs = 25832)

st_bbox(germany)       # in degrees
st_bbox(germany_proj)  # in metres

# Visual comparison: geographic vs. projected
p1 <- ggplot(germany) +
  geom_sf(fill = "gray90") +
  labs(title = "WGS 84 (geographic)") +
  theme_minimal()

p2 <- ggplot(germany_proj) +
  geom_sf(fill = "gray90") +
  labs(title = "ETRS89 / UTM 32N (projected)") +
  theme_minimal()

p1 + p2

# -----------------------------------------------------------------------------
# Area calculations: geographic vs. projected
# -----------------------------------------------------------------------------

st_area(germany) |> units::set_units("km^2")        # spherical calculation
st_area(germany_proj) |> units::set_units("km^2")    # planar calculation
