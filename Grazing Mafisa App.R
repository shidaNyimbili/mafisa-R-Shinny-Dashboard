#################################################
#################################################
### SETUP NEEDED IF ELIGIBILITY OR BLOCK BOUNDARIES ARE CHANGED
#################################################

# ### Perform ineligibility cutouts from grazing block shapefiles
# library(sf)
# 
# #elig = st_read(file.path(getwd(), "Input data", "Eligibility", "VCS4915_10m_Eligibility_boundaries.gpkg"))
# blocks1 = st_read(file.path(getwd(), "Input data", "Grazing spatial data", "20251014_Cohort_0_1_Overlap_Correction", "20251014Cohort01OverlapCorrection.shp"))
# blocks2 = st_read(file.path(getwd(), "Input data", "Grazing spatial data", "20251014_Cohort_2_Overlap_Correction", "20251014Cohort2OverlapCorrection.shp"))
#   
# # Step 1: Make both layers valid
# #elig_valid <- st_make_valid(elig)
# blocks1_valid <- st_make_valid(blocks1)
# blocks2_valid <- st_make_valid(blocks2)
# 
# # Step 2: Union the eligibility polygons to get a single eligible area with holes
# #elig_union <- st_union(elig_valid)
# #saveRDS(elig_union, file=file.path(getwd(), "Input data", "Eligibility", "VCS4915_10m_eligibility_single_polygon.RDS"))
# elig_union = readRDS(file=file.path(getwd(), "Input data", "Eligibility", "VCS4915_10m_eligibility_single_polygon.RDS"))
# 
# # Step 3: Intersect grazing blocks with eligible land
# blocks1_elig <- st_intersection(blocks1_valid, elig_union)
# blocks2_elig <- st_intersection(blocks2_valid, elig_union)
# 
# st_write(blocks1_elig, file.path(getwd(), "Input data", "Grazing spatial data", "20251014Cohort01OverlapCorrection_eligible.gpkg"), delete_dsn = TRUE)
# st_write(blocks2_elig, file.path(getwd(), "Input data", "Grazing spatial data", "20251014Cohort2OverlapCorrection_eligible.gpkg"), delete_dsn = TRUE)

# ### Extract and assign other model input values to the UNZA 
# library(tidyverse)
# library(sf)
# library(geodata)          # Used to get WorldClim variables for each point
# library(raster)
# 
# project_boundary = st_read(file.path(getwd(), "Input data", "Eligibility", "VCS4915_Outer Boundary.shp"))
# UNZA_points = read.csv(file.path(getwd(), "Input data", "UNZA 2025 Data", "Sheet 1-SOIL DATA CARBON+BD+TEXTURE-650 RECORDS_headers_corrected.csv"))
# UNZA_sf = UNZA_points %>%
#   st_as_sf(coords=c("Lon", "Lat"), crs=4326) %>%
#   mutate(Lon = sf::st_coordinates(.)[,1],
#          Lat = sf::st_coordinates(.)[,2])
# 
# ## Load and extract values of specific variables and attribute to pre-sampling points
# # Temperature and precipitation
# monthly_MAT <- worldclim_country("Zambia", var="tavg", path=file.path(getwd(), "MapLayers"))
# annual_MAT <- calc(stack(monthly_MAT), fun = mean, na.rm = TRUE)
# cropped_MAT_raster <- crop(annual_MAT, as(project_boundary, "Spatial"))
# 
# monthly_MAP <- worldclim_country("Zambia", var="prec", path=file.path(getwd(), "MapLayers"))
# annual_MAP <- calc(stack(monthly_MAP), fun = sum, na.rm = TRUE)
# cropped_MAP_raster <- crop(annual_MAP, as(project_boundary, "Spatial"))
# 
# UNZA_sf$MAT <- raster::extract(cropped_MAT_raster, UNZA_sf)
# UNZA_sf$MAP <- raster::extract(cropped_MAP_raster, UNZA_sf)
# 
# # Extract fire frequency data from MODIS baseline period (2014-2023)
# MODIS_burn_frequency_baseline = raster(file.path(getwd(), "Input data", "Fire frequency", "MODIS_fire_freq_counts_2014-2023.tif"))
# UNZA_sf$Fire_frequency <- raster::extract(MODIS_burn_frequency_baseline, UNZA_sf)
# UNZA_sf$Fire_frequency[is.na(UNZA_sf$Fire_frequency)] = 0
# write.csv(st_drop_geometry(UNZA_sf), file=file.path(getwd(), "Input data", "UNZA 2025 Data", "UNZA 2025 Phase 1 data.csv"), row.names = F)

################
################

# Minimalist Shiny app for Zambia grazing communities (2024/2025)
# Uses existing several core objects:
# communities1, communities2, blocks1, blocks1_elig, blocks2, blocks2_elig, reports24, reports25

library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(DT)
library(RColorBrewer)
library(purrr)
library(glue)
library(tibble)
library(scales)

sf_use_s2(FALSE)

communities1 = st_read(file.path(getwd(), "Input data", "Grazing spatial data", "Cohort01_CommunityBoundaries.gpkg"))
communities2 = st_read(file.path(getwd(), "Input data", "Grazing spatial data", "Cohort2_CommunityBoundaries.gpkg"))

reports24 = read.csv(file.path(getwd(), "Input data", "Grazing spatial data", "Master Grazing Reports 2024-April 2025_corrected.csv"))
reports25 = read.csv(file.path(getwd(), "Input data", "Grazing spatial data", "Grazing Coordinates and Data_2025_10_14.csv"))

blocks1 = st_read(file.path(getwd(), "Input data", "Grazing spatial data", "20251014_Cohort_0_1_Overlap_Correction", "20251014Cohort01OverlapCorrection.shp"))
blocks2 = st_read(file.path(getwd(), "Input data", "Grazing spatial data", "20251014_Cohort_2_Overlap_Correction", "20251014Cohort2OverlapCorrection.shp"))

blocks1_elig = st_read(file.path(getwd(), "Input data", "Grazing spatial data", "20251014Cohort01OverlapCorrection_eligible.gpkg"))
blocks2_elig = st_read(file.path(getwd(), "Input data", "Grazing spatial data", "20251014Cohort2OverlapCorrection_eligible.gpkg"))

project_boundary = st_read(file.path(getwd(), "Input data", "Eligibility", "VCS4915_Outer Boundary.shp"))
UNZA_points = read.csv(file.path(getwd(), "Input data", "UNZA 2025 Data", "UNZA 2025 Phase 1 data.csv"))
presample_points = read.csv(file.path(getwd(), "outputs", "Pre-sampling data_processed.csv"))

boreholes = read.csv(file.path(getwd(), "Input data", "Borehole data 2024_and2025.csv"))
PAI_boundaries = st_read(file.path(getwd(), "Input data", "Grazing spatial data", "PAI_ClusterBoundaries.gpkg"))

# Base map group names
BASE_LIGHT <- "Light (Carto)"
BASE_SAT <- "Satellite (Esri)"

# ----------------------------
# 1) Helpers
# ----------------------------

# Normalize community strings for safe text join (case-insensitive, ignore punctuation/space/underscore)
norm_comm <- function(x) {
  x %>%
    tolower() %>%
    stringr::str_trim() %>%
    # normalize underscores, dashes and parentheses to space
    stringr::str_replace_all("[_()\\-]", " ") %>%
    # collapse repeated spaces
    stringr::str_squish() %>%
    # remove all non-alphanumeric (keep spaces first to compare on words)
    (\(s) stringr::str_replace_all(s, "[^a-z0-9 ]", ""))() %>%
    # collapse spaces entirely for strict equality without spacing differences
    stringr::str_replace_all(" ", "")
}

# GPS fix/validation using your specified Zambia ranges with sign-correction where obvious.
# Valid ranges after fix: lat in [-17.8, -16.3], lon in [24.0, 25.5]
fix_and_filter_coords <- function(lat, lon) {
  # vectorized
  lat_adj <- lat
  lon_adj <- lon
  
  # Fix obvious sign errors: if latitude is positive but within plausible magnitude -> flip sign
  need_flip_lat <- !is.na(lat) & lat > 0 & lat >= 16.3 & lat <= 17.8
  lat_adj[need_flip_lat] <- -lat[need_flip_lat]
  
  # For longitude: if negative but within plausible magnitude -> flip sign
  need_flip_lon <- !is.na(lon) & lon < 0 & abs(lon) >= 24.0 & abs(lon) <= 25.5
  lon_adj[need_flip_lon] <- -lon[need_flip_lon]
  
  # Keep only those within final bounding box
  keep <- !is.na(lat_adj) & !is.na(lon_adj) &
    lat_adj >= -17.8 & lat_adj <= -16.3 &
    lon_adj >= 24.0 & lon_adj <= 25.5
  
  list(lat = ifelse(keep, lat_adj, NA_real_),
       lon = ifelse(keep, lon_adj, NA_real_),
       keep = keep)
}

# Compute area (ha) in equal-area CRS
equal_area_crs <- 6933 # World Cylindrical Equal Area (meters)
area_ha <- function(geom) {
  # Expects sf object
  sf::st_area(sf::st_transform(geom, equal_area_crs)) %>%
    units::set_units("m^2") %>%
    as.numeric() / 10000
}

# Ensure geometry column name is "geometry", drop Z/M, make valid
std_geom <- function(x, geom_col_guess = NULL, set_wgs84_if_missing = TRUE) {
  stopifnot(inherits(x, "sf"))
  
  # If a geometry column name was provided, set it
  if (!is.null(geom_col_guess) && geom_col_guess %in% names(x)) {
    sf::st_geometry(x) <- geom_col_guess
  }
  
  # Assign WGS84 if CRS is missing
  if (set_wgs84_if_missing && is.na(sf::st_crs(x))) {
    x <- sf::st_set_crs(x, 4326)
  }
  
  # Drop Z/M and make valid
  x <- x %>% sf::st_zm(drop = TRUE, what = "ZM") %>% sf::st_make_valid()
  
  # Rename geometry column to "geometry"
  geom_col <- attr(x, "sf_column")
  if (!identical(geom_col, "geometry")) {
    names(x)[names(x) == geom_col] <- "geometry"
    sf::st_geometry(x) <- "geometry"
  }
  
  x
}

# Keep only polygonal parts, and cast to MULTIPOLYGON (for Leaflet)
as_polys <- function(x) {
  x %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>%   # drop lines/points in geometry collections
    sf::st_cast("MULTIPOLYGON", warn = FALSE)
}

# Brewer Spectral palette (12) ordered Jan->Dec
month_levels <- month.abb
pal_month <- colorFactor(palette = colorRampPalette(brewer.pal(11, "Spectral"))(12),
                         levels = month_levels, ordered = TRUE)

# Nicely format numeric KPIs, fallback to "NA"
fmt_num <- function(x, digits = 1) {
  out <- ifelse(is.na(x), NA, scales::comma(round(x, digits)))
  out
}

fmt_ha <- function(x) paste0(scales::comma(round(x, 1)), " ha")

fmt_den <- function(x, digits = 3) ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits))

# ---- Depth-weighted average helper (0–30 and 30–100; weights 30 and 70) ----
dw_avg <- function(a, b, w1 = 30, w2 = 70) {
  # Option A for missing values: if one is missing, use the other; if both missing -> NA
  res <- (a * w1 + b * w2) / (w1 + w2)
  only_a <- !is.na(a) & is.na(b)
  only_b <- is.na(a) & !is.na(b)
  both_na <- is.na(a) & is.na(b)
  res[only_a] <- a[only_a]
  res[only_b] <- b[only_b]
  res[both_na] <- NA_real_
  res
}

# Pretty "avg (± sd)" formatter with chosen digits
fmt_mean_sd <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x))) return("0 (± 0)")
  m <- mean(x, na.rm = TRUE)
  s <- ifelse(length(na.omit(x)) > 1, sd(x, na.rm = TRUE), 0)
  paste0(round(m, digits), " (± ", round(s, digits), ")")
}

# TRUE if there is at least one non-empty geometry
non_empty <- function(x) {
  tryCatch(nrow(x) > 0 && any(!sf::st_is_empty(x)), error = function(e) FALSE)
}

# A single bbox for one or many features (4326)
bbox_of <- function(x) {
  sf::st_bbox(sf::st_union(sf::st_transform(x, 4326)))
}

# ----------------------------
# 2) Standardize & Prepare data
# ----------------------------

# --- Communities (one polygon per community) ---
comm24 <- communities1 %>%
  std_geom(geom_col_guess = "geom") %>%       
  sf::st_set_crs(4326) %>%                    
  sf::st_transform(4326) %>%
  transmute(
    cohort   = "2024",
    community = community_,
    geometry = geometry
  )

comm25 <- communities2 %>%
  std_geom(geom_col_guess = "geom") %>%
  sf::st_set_crs(4326) %>%                    
  sf::st_transform(4326) %>%
  transmute(
    cohort   = "2025",
    community = Community,
    geometry = geometry
  )

communities_all <- bind_rows(comm24, comm25) %>%
  group_by(cohort, community) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
  as_polys()  # ensure polygonal

# Sets of communities by cohort
cohort24_set <- communities_all %>%
  dplyr::filter(cohort == "2024") %>%
  dplyr::pull(community) %>%
  unique()

cohort25_set <- communities_all %>%
  dplyr::filter(cohort == "2025") %>%
  dplyr::pull(community) %>%
  unique()

# ---- Merge community boundaries across cohorts (for attribution) ----
communities_merged <- communities_all %>%
  sf::st_transform(4326) %>%
  dplyr::group_by(community) %>%
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
  sf::st_make_valid()

# ---- Project boundary cleaned (outline only on map) ----
project_boundary_clean <- project_boundary %>%
  std_geom(geom_col_guess = "geometry") %>%
  sf::st_transform(4326)

# ---- Project activity instance boundaries cleaned (outline only on map) ----
PAI_boundaries_clean <- PAI_boundaries %>%
  std_geom(geom_col_guess = "geom") %>%
  sf::st_transform(4326)

# ---- Borehole locations (circles coloured by type on map with popups) ----
boreholes_sf <- sf::st_as_sf(
  boreholes,
  coords = c("Easting..DD.WGS.84.", "Southing..DD.WGS.84."),
  crs = 4326,
  remove = FALSE
)

# Color palette by Type (robust to > 8 classes)
bh_types <- sort(unique(boreholes_sf$Type))
pal_bh <- colorFactor(
  palette = colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(length(bh_types)),
  domain = bh_types
)

# Popup HTML
boreholes_sf <- boreholes_sf %>%
  dplyr::mutate(
    popup_html = glue::glue_data(
      .,
      "<b>{Borehole.Code}</b> — {Borehol.Name}
       <br/><b>Type:</b> {Type}
       <br/><b>Area:</b> {Area}
       <br/><b>District:</b> {District}
       <br/><b>Year:</b> {Year}"
    )
  )

# Map each Type to one of leaflet-awesome marker colors
bh_types <- sort(unique(na.omit(boreholes_sf$Type)))
bh_color_pool <- c(
  "blue","green","orange","red","purple","cadetblue","darkgreen",
  "darkblue","darkred","lightblue","lightgray","black","pink"
)
bh_col_map <- setNames(rep(bh_color_pool, length.out = length(bh_types)), bh_types)

# Row-wise marker color (fallback = 'gray' if Type is NA or unseen)
boreholes_sf$markerColor <- unname(bh_col_map[as.character(boreholes_sf$Type)])
boreholes_sf$markerColor[is.na(boreholes_sf$markerColor)] <- "#7f7f7f"

# Create Font Awesome "tint" (droplet) icons with those colors
bh_icons <- leaflet::awesomeIcons(
  icon        = "tint",      # FA droplet icon (available in Font Awesome 4.x)
  library     = "fa",
  iconColor   = "white",
  markerColor = boreholes_sf$markerColor
)

# Approximate hex fill for leaflet-awesome marker colors
awesome_hex <- c(
  blue      = "#2A81CB",
  green     = "#5CB85C",
  orange    = "#F39C12",
  red       = "#D63E2A",
  purple    = "#8E44AD",
  cadetblue = "#4D8FAC",
  darkgreen = "#2C6B2F",
  darkblue  = "#254B7C",
  darkred   = "#A23336",
  lightblue = "#7AB8F5",
  lightgray = "#D3D3D3",
  gray      = "#A0A0A0",
  black     = "#000000",
  pink      = "#FF91EA"
)

# Legend colors by Type (same order as bh_types)
bh_legend_cols <- unname(awesome_hex[ bh_col_map[bh_types] ])
bh_legend_cols[is.na(bh_legend_cols)] <- "#7f7f7f"  # fallback

# ---- Soil sampling points: UNZA_points (data.frame -> sf) ----
# Expected columns: station_id, Lon, Lat,
# Bulk_density_0.30cm, Bulk_density_30.100cm,
# C_stock.tons.ha._0.30cm, C_stock.tons.ha._30.100cm,
# Texture_0.30cm_.Sand/Silt/Clay, Texture_30.100cm_.Sand/Silt/Clay
unza_sf <- sf::st_as_sf(
  UNZA_points,
  coords = c("Lon", "Lat"),
  crs = 4326,
  remove = FALSE
) %>%
  sf::st_make_valid()

# ---- Compute 0–100 cm metrics ----
unza_sf <- unza_sf %>%
  dplyr::mutate(
    # Bulk density (g/cm³), depth-weighted
    BD_0_100   = dw_avg(`Bulk_density_0.30cm`, `Bulk_density_30.100cm`),
    # Sand/Silt/Clay (%), depth-weighted
    Sand_0_100 = dw_avg(`Texture_0.30cm_.Sand`, `Texture_30.100cm_.Sand`),
    Silt_0_100 = dw_avg(`Texture_0.30cm_.Silt`, `Texture_30.100cm_.Silt`),
    Clay_0_100 = dw_avg(`Texture_0.30cm_.Clay`, `Texture_30.100cm_.Clay`),
    # C stock (t/ha), summed (Option A: if one missing, take the other; if both -> NA)
    Cstock_0_100 = {
      a <- `C_stock.tons.ha._0.30cm`
      b <- `C_stock.tons.ha._30.100cm`
      out <- a + b
      out[is.na(a) & !is.na(b)] <- b[is.na(a) & !is.na(b)]
      out[!is.na(a) & is.na(b)] <- a[!is.na(a) & is.na(b)]
      out[is.na(a) & is.na(b)] <- NA_real_
      out
    }
  )

# ---- Attribute sampling points to communities (within) ----
unza_sf_join <- sf::st_join(
  unza_sf,
  communities_merged[, c("community")],
  join = sf::st_within,   # "within" the boundary (points on boundary will be excluded)
  left = TRUE
) %>%
  dplyr::mutate(
    in_community = !is.na(community)
  )

# Pre-split for mapping (constant layers)
soil_pts_in  <- unza_sf_join %>% dplyr::filter(in_community)
soil_pts_out <- unza_sf_join %>% dplyr::filter(!in_community)

# ---- Popup html for sampling points ----
soil_popup <- function(df) {
  glue::glue(
    "<b>Station:</b> {df$station_id}",
    "<br/><b>Community:</b> {ifelse(is.na(df$community), 'Outside', df$community)}",
    "<br/><b>Bulk density</b> (g/cm³) – 0–30: {round(df$`Bulk_density_0.30cm`, 2)}, 30–100: {round(df$`Bulk_density_30.100cm`, 2)}, 0–100: {round(df$BD_0_100, 2)}",
    "<br/><b>C stock</b> (t/ha) – 0–30: {round(df$`C_stock.tons.ha._0.30cm`, 1)}, 30–100: {round(df$`C_stock.tons.ha._30.100cm`, 1)}, 0–100: {round(df$Cstock_0_100, 1)}",
    "<br/><b>Sand</b> (%) – 0–30: {round(df$`Texture_0.30cm_.Sand`, 1)}, 30–100: {round(df$`Texture_30.100cm_.Sand`, 1)}, 0–100: {round(df$Sand_0_100, 1)}"
  )
}

soil_pts_in$popup_html  <- soil_popup(soil_pts_in)
soil_pts_out$popup_html <- soil_popup(soil_pts_out)

# ---- Pre-sample soil points (2022/23): table -> sf ----
# Columns present: lon, lat, MAT, MAP, Fire_frequency, BD30, BD70, SOC_stock30, SOC_stock70, SOC_stock_1m, Sand...
presample_sf <- sf::st_as_sf(
  presample_points,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>% sf::st_make_valid()

# Identify the Sand column (your str() shows 'Sand...' – pick the first column starting with 'Sand')
sand_col_name <- grep("^Sand", names(presample_sf), value = TRUE)[1]

# Compute 0–100 cm metrics
presample_sf <- presample_sf %>%
  dplyr::mutate(
    # Sand is provided as a single measurement representing 0–100 cm
    Sand_0_100 = as.numeric(.data[[sand_col_name]]),
    
    # Depth-weighted bulk density (30/70 rule), Option A for missing
    BD_0_100 = dw_avg(BD30, BD70),
    
    # 0–100 cm C stock: prefer SOC_stock_1m, fallback to sum of 30 + 70 (Option A for missing)
    Cstock_0_100 = {
      a <- SOC_stock_1m
      b <- SOC_stock30
      c <- SOC_stock70
      out <- a
      out[is.na(a) & !is.na(b) & !is.na(c)] <- b[is.na(a) & !is.na(b) & !is.na(c)] + c[is.na(a) & !is.na(b) & !is.na(c)]
      out[is.na(a) & is.na(b) & !is.na(c)]  <- c[is.na(a) & is.na(b) & !is.na(c)]
      out[is.na(a) & !is.na(b) & is.na(c)]  <- b[is.na(a) & !is.na(b) & is.na(c)]
      out
    }
  )

# Attribute to communities (within -> border points effectively excluded, per your preference)
presample_join <- sf::st_join(
  presample_sf,
  communities_merged["community"],
  join = sf::st_within, left = TRUE
) %>%
  dplyr::mutate(in_community = !is.na(community))

# Split for mapping
presample_in  <- presample_join %>% dplyr::filter(in_community)
presample_out <- presample_join %>% dplyr::filter(!in_community)

# Popups (include Site, Year, BD 0–30 / 30–100 / 0–100, SOC stocks, Sand (0–100), MAT/MAP/Fire)
presample_join <- presample_join %>%
  dplyr::mutate(
    popup_html = glue::glue_data(
      .,
      "<b>Site:</b> {Site} &nbsp;&nbsp;<b>Year:</b> {Year}
       <br/><b>Bulk density</b> (g/cm³) – 0–30: {round(BD30, 2)}, 30–100: {round(BD70, 2)}, 0–100: {round(BD_0_100, 2)}
       <br/><b>C stock</b> (t/ha) – 0–30: {round(SOC_stock30, 1)}, 30–100: {round(SOC_stock70, 1)}, 0–100: {round(Cstock_0_100, 1)}
       <br/><b>Sand</b> (%) – 0–100: {round(Sand_0_100, 1)}
       <br/><b>MAT</b> (°C): {round(MAT,1)} &nbsp;&nbsp; <b>MAP</b> (mm): {MAP} &nbsp;&nbsp; <b>Fire freq.:</b> {Fire_frequency}"
    )
  )

# Recreate 'in'/'out' data frames with popup attached
presample_in  <- presample_join %>% dplyr::filter(in_community)
presample_out <- presample_join %>% dplyr::filter(!in_community)

# --- Blocks (full and eligible) ---
blk24_full <- blocks1 %>%
  std_geom(geom_col_guess = "geometry") %>%
  transmute(
    cohort = "2024",
    community = community_,
    block_name = grazing_bl,
    eligible = FALSE,
    geometry = geometry
  )

blk24_elig <- blocks1_elig %>%
  std_geom(geom_col_guess = "geometry") %>%
  transmute(
    cohort = "2024",
    community = community_,
    block_name = grazing_bl,
    eligible = TRUE,
    geometry = geometry
  )

blk25_full <- blocks2 %>%
  std_geom(geom_col_guess = "geometry") %>%
  transmute(
    cohort = "2025",
    community = Community,
    block_name = GB_Name,
    eligible = FALSE,
    geometry = geometry
  )

blk25_elig <- blocks2_elig %>%
  std_geom(geom_col_guess = "geometry") %>%
  transmute(
    cohort = "2025",
    community = Community,
    block_name = GB_Name,
    eligible = TRUE,
    geometry = geometry
  )

blocks_all <- bind_rows(blk24_full, blk24_elig, blk25_full, blk25_elig) %>%
  as_polys()

# --- Areas pre-computation (per community, not per year) ---
# We’ll compute these on demand (fast enough), but here’s a small helper:
community_area_lookup <- communities_all %>%
  mutate(area_ha = area_ha(geometry)) %>%
  st_drop_geometry() %>%
  # If same community appears in both cohorts (rare), keep the first (areas should match)
  group_by(community) %>%
  summarise(community_area_ha = first(area_ha), .groups = "drop")

block_area_lookup <- blocks_all %>%
  mutate(area_ha = area_ha(geometry)) %>%
  st_drop_geometry() %>%
  group_by(community, eligible) %>%
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = eligible, values_from = area_ha,
                     names_prefix = "eligible_", values_fill = 0) %>%
  rename(
    block_area_ha = eligible_FALSE,
    elig_block_area_ha = eligible_TRUE
  )

# --- Community dropdown choices (use canonical names from the community layers) ---
community_choices <- sort(unique(communities_all$community))
norm_map <- tibble(community = community_choices,
                   norm = norm_comm(community))

# --- Reports (standardize, filter/clean GPS, month coloring) ---
prep_reports_2024 <- function(df) {
  df %>%
    mutate(
      date     = suppressWarnings(lubridate::dmy(Record.Date)),
      animals  = as.numeric(Number.of.Animals.on.Block),
      community = as.character(Community),
      lat_raw  = as.numeric(gps_stamp_latitude),
      lon_raw  = as.numeric(gps_stamp_longitude)
    ) %>%
    {
      f <- fix_and_filter_coords(.$lat_raw, .$lon_raw)
      mutate(., lat = f$lat, lon = f$lon, keep = f$keep)
    } %>%
    filter(keep) %>%
    # >>> Keep only genuine 2024 rows <<<
    filter(!is.na(date) & lubridate::year(date) == 2024) %>%
    transmute(
      cohort = "2024",
      community,
      date,
      year  = lubridate::year(date),
      month = factor(lubridate::month(date, label = TRUE, abbr = TRUE),
                     levels = month_levels, ordered = TRUE),
      animals,
      lat, lon
    ) %>%
    filter(!is.na(animals))
}

prep_reports_2025 <- function(df) {
  # 2025 schema:
  # community_name, date (dd/mm/yyyy), total.number.of.animals
  # grazing_report_coordinates.Latitude / .Longitude
  df %>%
    mutate(
      date = suppressWarnings(lubridate::dmy(date)),
      animals = as.numeric(total.number.of.animals),
      community = as.character(community_name),
      lat_raw = as.numeric(`grazing_report_coordinates.Latitude`),
      lon_raw = as.numeric(`grazing_report_coordinates.Longitude`)
    ) %>%
    {
      f <- fix_and_filter_coords(.$lat_raw, .$lon_raw)
      mutate(., lat = f$lat, lon = f$lon, keep = f$keep)
    } %>%
    filter(keep) %>%
    transmute(
      cohort = "2025",
      community,
      date,
      year = lubridate::year(date),
      month = factor(lubridate::month(date, label = TRUE, abbr = TRUE),
                     levels = month_levels, ordered = TRUE),
      animals,
      lat, lon
    ) %>%
    filter(!is.na(date), !is.na(animals))
}

reports24_std <- prep_reports_2024(reports24)
reports25_std <- prep_reports_2025(reports25)

# A helper to filter reports by a vector of communities (handles inconsistent spellings via normalization)
filter_reports_by_comm <- function(reps, selected) {
  keys <- unique(norm_comm(selected))
  reps %>%
    mutate(norm = norm_comm(community)) %>%
    filter(norm %in% keys) %>%
    select(-norm)
}


# ----------------------------
# 3) Shiny UI
# ----------------------------

ui <- fluidPage(
  titlePanel("Mafisa carbon project - Community grazing summary explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectizeInput(
        "community", "Community",
        choices  = c("All", community_choices),
        selected = "Chisu",
        multiple = TRUE,
        options  = list(plugins = list("remove_button"))
      ),
      selectInput("year", "Year", choices = c("All", "2024", "2025"), selected = "All"),
      # helpText("Map: toggle full vs eligible blocks using the layer control on the map."),
      # helpText("Points colored by submission month (Jan–Dec)."),
      hr(),
      # h4("Community summary"),
      uiOutput("areas_title"),
      DTOutput("areas_table", width = "100%")
    ),
    mainPanel(
      width = 8,
      leafletOutput("map", height = 520),
      br(),
      h4("Soil sampling (0–100 cm)"),
      DTOutput("soil_table"),
      br(),
      h4("Reports (side-by-side by year)"),
      DTOutput("kpis_table"),
      br(),
      plotOutput("time_series", height = 240)
    )
  )
)

# ----------------------------
# 4) Shiny Server
# ----------------------------

server <- function(input, output, session) {
  
  # Treat "All" (or no selection) as "all communities"
  selected_comms <- reactive({
    sel <- input$community
    if (is.null(sel) || length(sel) == 0L || "All" %in% sel) community_choices else sel
  })
  
  output$areas_title <- renderUI({
    n <- length(selected_comms())
    h4(glue::glue("Community summary – {n} selected"))
  })
  
  # Program participation flags by year (not report presence)
  has_program_2024 <- reactive(any(selected_comms() %in% cohort24_set))
  has_program_2025 <- reactive(any(selected_comms() %in% cohort25_set))
  
  # Community boundary (if exists in either cohort)
  comm_boundary <- reactive({
    communities_all %>% dplyr::filter(community %in% selected_comms())
  })
  
  # Blocks for this community (both types consolidated)
  comm_blocks <- reactive({
    blocks_all %>% dplyr::filter(community %in% selected_comms())
  })
  
  # Reports filtered to selected year/community
  comm_reports <- reactive({
    y   <- input$year
    sel <- selected_comms()
    r24 <- filter_reports_by_comm(reports24_std, sel)
    r25 <- filter_reports_by_comm(reports25_std, sel)
    if (y == "2024") r24 else if (y == "2025") r25 else dplyr::bind_rows(r24, r25)
  })
  
  # Build Leaflet map
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      # Add both base layers
      addProviderTiles(providers$CartoDB.Positron, group = BASE_LIGHT) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = BASE_SAT) %>%
      setView(lng = 24.75, lat = -17.05, zoom = 8) %>%
      # Add the base layer control (we’ll re-add with overlay groups in the observer)
      addMeasure(
        primaryLengthUnit = "meters",
        primaryAreaUnit   = "hectares",
        position          = "topleft"
      ) %>%
      addLayersControl(
        baseGroups = c(BASE_LIGHT, BASE_SAT),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  observe({
    req(input$community)
    # use leafletProxy to update layers
    proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      removeControl("bh_legend") %>%
      removeControl("rep_legend")
    
    # Draw community outline (merged over cohorts if duplicates)
    cb <- comm_boundary()
    if (nrow(cb) > 0) {
      proxy <- proxy %>%
        addPolygons(
          data = cb %>% sf::st_transform(4326),
          color = "#1b6ca8", weight = 2, fill = FALSE,
          group = "Community boundary",
          label = ~community
        )
      # Fit bounds
      bb <- sf::st_bbox(cb %>% sf::st_transform(4326))
      proxy <- proxy %>% fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    }
    
    # Community name labels (one per selected community)
    cb <- comm_boundary()
    if (nrow(cb) > 0) {
      labels_pts <- cb %>%
        sf::st_transform(4326) %>%
        dplyr::mutate(geometry = sf::st_point_on_surface(geometry))  # point guaranteed inside
      
      proxy <- proxy %>%
        addLabelOnlyMarkers(
          data = labels_pts,
          label = ~community,
          group = "Community labels",
          labelOptions = labelOptions(
            noHide    = TRUE,
            direction = "center",
            textOnly  = TRUE,
            style     = list(
              "color" = "#1b6ca8",
              "font-weight" = "bold",
              "font-size" = "12px",
              "text-shadow" = "0 0 3px #ffffff"
            )
          )
        )
    }
    
    # Blocks layers: full vs eligible (toggle in layer control)
    blks <- comm_blocks()
    if (nrow(blks) > 0) {
      blks_4326 <- blks %>% sf::st_transform(4326)
      # Full
      bl_full <- blks_4326 %>% filter(!eligible)
      if (nrow(bl_full) > 0) {
        proxy <- proxy %>%
          addPolygons(
            data = bl_full,
            color = "#2ca25f", weight = 1, fillColor = "#99d8c9",
            fillOpacity = 0.4, group = "Blocks (full)",
            label = ~block_name
          )
      }
      # Eligible
      bl_elig <- blks_4326 %>% filter(eligible)
      if (nrow(bl_elig) > 0) {
        proxy <- proxy %>%
          addPolygons(
            data = bl_elig,
            color = "#de2d26", weight = 1, fillColor = "#fcae91",
            fillOpacity = 0.45, group = "Blocks (eligible)",
            label = ~block_name
          )
      }
    }
    
    # Reports (points) colored by month
    reps <- comm_reports()
    if (nrow(reps) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(
          data = reps,
          lng = ~lon, lat = ~lat,
          radius = 5,
          color = ~pal_month(as.character(month)),
          stroke = TRUE, weight = 1, opacity = 1,
          fillOpacity = 0.85,
          group = "Grazing reports",
          popup = ~glue("<b>{community}</b><br/>Date: {format(date, '%d %b %Y')}<br/>Animals: {scales::comma(animals)}<br/>Month: {as.character(month)}")
        ) %>%
        addLegend(
          "bottomleft",
          pal = pal_month,
          values = factor(month_levels, levels = month_levels, ordered = TRUE),
          title = "Month",
          group = "Grazing reports",
          opacity = 1,
          layerId  = "rep_legend"
        )
    }
    
    # ---- Project boundary (outline only) ----
    if (nrow(project_boundary_clean) > 0) {
      proxy <- proxy %>%
        addPolylines(
          data = project_boundary_clean,
          color = "#5e3c99", weight = 3, opacity = 1,
          fill = FALSE, fillOpacity = 0,
          group = "Project boundary",
          label = ~Id
        )
    }
    
    # ---- Soil samples (constant layers) ----
    if (nrow(soil_pts_in) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(
          data = soil_pts_in,
          radius = 5,
          color = "black", fillColor = "black",
          stroke = TRUE, weight = 1, fillOpacity = 0.9,
          group = "Soil samples (in community)",
          popup = ~popup_html
        )
    }
    if (nrow(soil_pts_out) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(
          data = soil_pts_out,
          radius = 4,
          color = "#7f7f7f", fillColor = "#7f7f7f",
          stroke = TRUE, weight = 1, fillOpacity = 0.7,
          group = "Soil samples (outside)",
          popup = ~popup_html
        )
    }
    
    # ---- Pre-sample soil points (toggleable; brown tones) ----
    if (nrow(presample_in) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(
          data = presample_in,
          radius = 5,
          color = "#8c510a", fillColor = "#8c510a",
          stroke = TRUE, weight = 1, fillOpacity = 0.9,
          group = "Pre-sample soil (in community)",
          popup = ~popup_html
        )
    }
    if (nrow(presample_out) > 0) {
      proxy <- proxy %>%
        addCircleMarkers(
          data = presample_out,
          radius = 4,
          color = "#d8b365", fillColor = "#d8b365",
          stroke = TRUE, weight = 1, fillOpacity = 0.75,
          group = "Pre-sample soil (outside)",
          popup = ~popup_html
        )
    }
    
    # ---- PAI boundaries (outline only; constant, not toggleable) ----
    if (nrow(PAI_boundaries_clean) > 0) {
      proxy <- proxy %>%
        addPolylines(
          data = PAI_boundaries_clean,
          color = "#6a3d9a", weight = 2, opacity = 0.8,
          fill = FALSE, fillOpacity = 0
          # NOTE: no `group` => not shown in layer control (constant)
        )
    }
    
    # ---- Boreholes (toggleable with legend; Font Awesome markers) ----
    if (nrow(boreholes_sf) > 0) {
      proxy <- proxy %>%
        addAwesomeMarkers(
          data = boreholes_sf,
          icon = bh_icons,
          popup = ~popup_html,
          group = "Boreholes"
        ) %>%
        addLegend(
          position = "bottomleft",
          colors   = bh_legend_cols,
          labels   = bh_types,
          title    = "Borehole type",
          opacity  = 1,
          group = "Boreholes",
          layerId  = "bh_legend"
        )
    }
    
    # Fallbacks if there is no boundary for current selection:
    blks <- comm_blocks()
    if (nrow(blks) > 0) {
      # after you add polygons for blocks
      bb <- bbox_of(blks)                                  # you already have bbox_of()
      proxy <- proxy %>% fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    } else {
      reps <- comm_reports()
      if (nrow(reps) > 0 && all(c("lon","lat") %in% names(reps))) {
        proxy <- proxy %>% fitBounds(
          min(reps$lon, na.rm = TRUE), min(reps$lat, na.rm = TRUE),
          max(reps$lon, na.rm = TRUE), max(reps$lat, na.rm = TRUE)
        )
      } else {
        proxy <- proxy %>% setView(lng = 24.75, lat = -17.05, zoom = 8)
      }
    }
    
    proxy %>%
      addLayersControl(
        baseGroups    = c(BASE_LIGHT, BASE_SAT),  # keep your base groups
        overlayGroups = c("Community boundary",
                          "Blocks (full)", "Blocks (eligible)",
                          "Grazing reports", "Boreholes",
                          "Pre-sample soil (in community)", "Pre-sample soil (outside)",
                          "Soil samples (in community)", "Soil samples (outside)"),
        options       = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Grazing reports") %>%
      hideGroup("Blocks (eligible)") %>%
      hideGroup("Soil samples (outside)") %>%
      hideGroup("Pre-sample soil (outside)")

  })
  
  # Area summary table
  output$areas_table <- renderDT({
    sel <- selected_comms()
    n_sel <- length(sel)
    
    ## --- Community area (sum across selected; unchanged) ---
    comm_area <- community_area_lookup %>%
      dplyr::filter(community %in% sel) %>%
      dplyr::summarise(total = sum(community_area_ha, na.rm = TRUE), .groups = "drop") %>%
      dplyr::pull(total) %>% { ifelse(length(.) == 0, NA_real_, .) }
    
    ## --- Block areas per community (ensure missing communities count as 0) ---
    blk_area_by_comm <- tibble::tibble(community = sel) %>%           # start with *all* selected comms
      dplyr::left_join(block_area_lookup, by = "community") %>%
      tidyr::replace_na(list(block_area_ha = 0, elig_block_area_ha = 0))
    
    total_block_area <- sum(blk_area_by_comm$block_area_ha, na.rm = TRUE)
    elig_block_area  <- sum(blk_area_by_comm$elig_block_area_ha, na.rm = TRUE)
    
    # Averages per selected community (includes zeros for comms with no blocks)
    avg_block_area_per_comm <- mean(blk_area_by_comm$block_area_ha, na.rm = TRUE)
    avg_elig_area_per_comm  <- mean(blk_area_by_comm$elig_block_area_ha, na.rm = TRUE)
    
    ## --- Rotations per community (drop geometry to avoid sf joins) ---
    blk_tbl <- blocks_all %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(community %in% sel)
    
    rot_full_pc <- blk_tbl %>%
      dplyr::filter(!eligible) %>%
      dplyr::group_by(community) %>%
      dplyr::summarise(n_full = dplyr::n_distinct(block_name), .groups = "drop")
    
    rot_elig_pc <- blk_tbl %>%
      dplyr::filter( eligible) %>%
      dplyr::group_by(community) %>%
      dplyr::summarise(n_elig = dplyr::n_distinct(block_name), .groups = "drop")
    
    # Join onto the full list of selected communities so missing ones are 0
    rot_pc <- tibble::tibble(community = sel) %>%
      dplyr::left_join(rot_full_pc, by = "community") %>%
      dplyr::left_join(rot_elig_pc, by = "community") %>%
      tidyr::replace_na(list(n_full = 0L, n_elig = 0L)) %>%
      dplyr::mutate(n = dplyr::if_else(n_full > 0L, n_full, n_elig))
    
    n_rotations_total     <- sum(rot_pc$n, na.rm = TRUE)
    avg_rotations_per_comm <- if (nrow(rot_pc) == 0) NA_real_ else mean(rot_pc$n, na.rm = TRUE)
    
    ## --- Reports & densities (same idea as before) ---
    reps        <- comm_reports()
    animals_ms  <- fmt_mean_sd(reps$animals, digits = 1)
    mean_anim   <- if (nrow(reps) > 0) mean(reps$animals, na.rm = TRUE) else NA_real_
    
    # Denominators: single community -> totals; multiple -> average per community
    denom_full <- if (n_sel > 1) avg_block_area_per_comm else total_block_area
    denom_elig <- if (n_sel > 1) avg_elig_area_per_comm  else elig_block_area
    
    dens_full <- if (!is.na(mean_anim) && is.finite(denom_full) && denom_full > 0) mean_anim / denom_full else NA_real_
    dens_elig <- if (!is.na(mean_anim) && is.finite(denom_elig) && denom_elig > 0) mean_anim / denom_elig else NA_real_
    
    ## --- Climate & fire (unchanged) ---
    pts    <- unza_sf_join %>% dplyr::filter(community %in% sel)
    mat_ms <- fmt_mean_sd(pts$MAT,            digits = 1)
    map_ms <- fmt_mean_sd(pts$MAP,            digits = 0)
    fire_ms<- fmt_mean_sd(pts$Fire_frequency, digits = 1)
    
    ## --- Assemble table (adds 3 extra rows only when multi-select) ---
    base_rows <- tibble::tibble(
      Metric = c(
        "Total community area",
        "Total grazing block area",
        "Eligible grazing block area",
        "Number of rotations",
        glue::glue("Average animals – {input$year} (± SD)"),
        "Cattle density (head/ha; full)",
        "Cattle density (head/ha; eligible)",
        "Mean annual temperature (°C)",
        "Mean annual precipitation (mm)",
        "Average Fire Frequency"
      ),
      Value = c(
        ifelse(is.na(comm_area),        "NA", fmt_ha(comm_area)),
        ifelse(is.na(total_block_area), "NA", fmt_ha(total_block_area)),
        ifelse(is.na(elig_block_area),  "NA", fmt_ha(elig_block_area)),
        as.character(n_rotations_total),
        animals_ms,
        fmt_den(dens_full, 3),
        fmt_den(dens_elig, 3),
        mat_ms,
        map_ms,
        fire_ms
      )
    )
    
    extra_rows <- NULL
    if (n_sel > 1) {
      extra_rows <- tibble::tibble(
        Metric = c(
          "Average grazing block area",
          "Average eligible grazing area",
          "Average number of rotations"
        ),
        Value = c(
          ifelse(is.na(avg_block_area_per_comm), "NA", fmt_ha(avg_block_area_per_comm)),
          ifelse(is.na(avg_elig_area_per_comm),  "NA", fmt_ha(avg_elig_area_per_comm)),
          format(round(avg_rotations_per_comm, 1), nsmall = 1)
        )
      )
    }
    
    # Insert extras after eligible area (3rd row) and after rotations (4th row)
    tbl <- if (is.null(extra_rows)) {
      base_rows
    } else {
      part1 <- base_rows[1:3, ]
      part2 <- base_rows[4, , drop = FALSE]
      rest  <- base_rows[5:nrow(base_rows), ]
      dplyr::bind_rows(part1, extra_rows[1:2, ], part2, extra_rows[3, , drop = FALSE], rest)
    }
    
    datatable(
      tbl,
      rownames = FALSE,
      class = 'compact',
      options = list(paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE, dom = "t")
    )
  })
  
  # ---- Reports KPIs table (rows = metrics, columns = 2024 & 2025) ----
  output$kpis_table <- renderDT({
    sel <- selected_comms()
    
    # Filter reports for the selected community (text-join with normalization)
    r24  <- filter_reports_by_comm(reports24_std, sel)
    r25  <- filter_reports_by_comm(reports25_std, sel)
    rAll <- dplyr::bind_rows(r24, r25)
    
    # Basic stats function
    stats_vec <- function(x) {
      if (length(x) == 0 || all(is.na(x))) {
        c(N = 0, Avg = NA_real_, Min = NA_real_, Max = NA_real_, SD = NA_real_)
      } else {
        c(
          N   = length(x),
          Avg = mean(x, na.rm = TRUE),
          Min = suppressWarnings(min(x, na.rm = TRUE)),
          Max = suppressWarnings(max(x, na.rm = TRUE)),
          SD  = ifelse(length(na.omit(x)) > 1, sd(x, na.rm = TRUE), NA_real_)
        )
      }
    }
    
    s24  <- stats_vec(r24$animals)
    s25  <- stats_vec(r25$animals)
    sAll <- stats_vec(rAll$animals)
    
    # Program flags
    has24 <- any(sel %in% cohort24_set)
    has25 <- any(sel %in% cohort25_set)
    has_any <- has24 || has25
    
    # Counts (needed to decide how to label zeroes)
    n24  <- as.integer(s24["N"]);  n24[is.na(n24)] <- 0L
    n25  <- as.integer(s25["N"]);  n25[is.na(n25)] <- 0L
    nAll <- as.integer(sAll["N"]); nAll[is.na(nAll)] <- 0L
    
    # Formatter with "0 (no program)" logic
    fmt_year_cell <- function(val_num, metric_key, n_reports, has_prog) {
      # If no reports, distinguish "no program" vs simply "no reports"
      if (n_reports == 0) return(if (!has_prog) "0 (no program)" else "0")
      
      # There are reports -> show the numbers (do NOT suppress by has_prog)
      if (is.na(val_num)) return("0")
      if (metric_key %in% c("Avg","SD")) fmt_num(val_num, 1) else fmt_num(val_num, 0)
    }
    
    # --- first-submission date helpers + values ------------------------
    first_date <- function(df) {
      if (nrow(df) == 0 || all(is.na(df$date))) as.Date(NA) else suppressWarnings(min(df$date, na.rm = TRUE))
    }
    fmt_first_cell <- function(d, has_prog) {
      if (is.na(d)) {
        if (!has_prog) "— (no program)" else "—"
      } else {
        format(d, "%d %b %Y")  # e.g., "07 May 2025"
      }
    }
    
    first24 <- first_date(r24)
    first25 <- first_date(r25)
    firstAll <- first_date(rAll)   # overall first date across years
    
    # Table with rows = metrics; columns = 2024, 2025, All
    kpis_long <- tibble::tibble(
      Metric = c("Number of reports",
                 "Average animals/report",
                 "Minimum animals/report",
                 "Maximum animals/report",
                 "Std. deviation"),
      `2024` = c(s24["N"], s24["Avg"], s24["Min"], s24["Max"], s24["SD"]),
      `2025` = c(s25["N"], s25["Avg"], s25["Min"], s25["Max"], s25["SD"]),
      `All`  = c(sAll["N"], sAll["Avg"], sAll["Min"], sAll["Max"], sAll["SD"])
    ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        metric_key = dplyr::case_when(
          grepl("^Number",  Metric) ~ "N",
          grepl("^Average", Metric) ~ "Avg",
          grepl("^Minimum", Metric) ~ "Min",
          grepl("^Maximum", Metric) ~ "Max",
          TRUE ~ "SD"
        ),
        `2024` = fmt_year_cell(as.numeric(`2024`), metric_key, n24,  has24),
        `2025` = fmt_year_cell(as.numeric(`2025`), metric_key, n25,  has25),
        `All`  = fmt_year_cell(as.numeric(`All`),  metric_key, nAll, has_any)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-metric_key)
    
    # --- Append "First submission date" row ----------------------------
    first_row <- tibble::tibble(
      Metric = "First submission date",
      `2024` = fmt_first_cell(first24, has24),
      `2025` = fmt_first_cell(first25, has25),
      `All`  = fmt_first_cell(firstAll, has_any)
    )
    
    datatable(
      dplyr::bind_rows(first_row, kpis_long),
      class = 'compact',
      rownames = FALSE,
      options = list(
        paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE, dom = "t",
        columnDefs = list(list(className = 'dt-center', targets = 1:3))
      )
    )
    
  })
  
  # Soil sample summary table
  output$soil_table <- renderDT({
    sel <- selected_comms()
    
    # UNZA points in community
    pts_unza <- unza_sf_join   %>% dplyr::filter(community %in% sel)
    n_unza   <- nrow(pts_unza)
    
    # Pre-sample points in community
    pts_pre  <- presample_join %>% dplyr::filter(community %in% sel)
    n_pre    <- nrow(pts_pre)
    
    # Helpers: mean(±sd) for numeric vectors, or "-" for unavailable metrics
    ms_or_dash <- function(x, digits = 1, dash = "-") {
      if (is.null(x)) return(dash)
      if (length(x) == 0 || all(is.na(x))) return(dash)
      fmt_mean_sd(x, digits = digits)
    }
    
    # Assemble table rows
    tbl <- tibble::tibble(
      Metric              = c(
        "Number of sampling points",
        "C stock (t/ha, 0–100 cm)",
        "Sand (% , 0–100 cm)",
        "Silt (% , 0–100 cm)",
        "Clay (% , 0–100 cm)"
      ),
      `UNZA (2025)`       = c(
        as.character(n_unza),
        ms_or_dash(pts_unza$Cstock_0_100, digits = 1),
        ms_or_dash(pts_unza$Sand_0_100,   digits = 1),
        ms_or_dash(pts_unza$Silt_0_100,   digits = 1),
        ms_or_dash(pts_unza$Clay_0_100,   digits = 1)
      ),
      `Pre-sample (2022/23)` = c(
        as.character(n_pre),
        ms_or_dash(pts_pre$Cstock_0_100, digits = 1),   # from SOC_stock_1m (with fallback)
        ms_or_dash(pts_pre$Sand_0_100,   digits = 1),   # single measurement (0–100)
        "-",                                            # no silt in pre-sample
        "-"                                             # no clay in pre-sample
      )
    )
    
    datatable(
      tbl,
      rownames = FALSE,
      class = 'compact',
      options = list(
        paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE, dom = "t",
        columnDefs = list(list(className = 'dt-center', targets = 1:2))
      )
    )
  })
  
  # Optional time-series: date vs animals (filtered by community + Year selector)
  output$time_series <- renderPlot({
    req(input$community)
    reps <- comm_reports()
    if (nrow(reps) == 0) return(NULL)
    
    # Color by year for simple readability
    ggplot2::ggplot(reps, ggplot2::aes(x = date, y = animals, color = factor(year))) +
      ggplot2::geom_point(alpha = 0.8) +
      ggplot2::geom_smooth(se = FALSE, method = "loess", span = 0.4, linewidth = 0.8) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::scale_color_brewer(palette = "Set1", name = "Year") +
      ggplot2::labs(x = "Submission date", y = "Number of animals",
                    title = glue("Report trend – {input$community} ({input$year})")) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "top")
  })
}

shinyApp(ui, server)

