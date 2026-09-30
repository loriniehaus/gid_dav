# Author: Lori Niehaus
# M&E Data dictionary version as of 2026-08-20
# Cleaned up / debugged: 2026-08-20

## PURPOSE ###############################################################
## Extract GID M&E Data Dictionary (reference data) from GID MEA KX Document Library
## Select data cleaning and joining (e.g., country shape file data to be matched)
## Save each key separately in clean DATT ADLS data folder (all sheets with 'key' in sheet name)

## Uses source datasets: ME&A Data Dictionary (on Sharepoint site); country shape file ctry_shapes.rds (on DATT ADLS) 

## Outputs in ADLS directory: ddphsis-cgh/gid/gidmea/giddatt
# "ctry_shapes_updated.rds" (retains shapefile class)
# "ref_country.rds" (does not retain shapefile class, but can be re-converted)
# "ref_nofo.rds"
# "ref_gisf.rds"
# "ref_x2025_28_gid_pa.rds" # GID FY25-28 PAs
# etc.

## R SETUP ###########################################################################

rm(list=ls()) # clear

## Install dependencies
required.packages <- c("tidyverse", "AzureStor","Microsoft365R", "readxl", "janitor","scales", "devtools","sf")
packages.to.install <- setdiff(required.packages, rownames(installed.packages()))
if(length(packages.to.install) >0) install.packages(packages.to.install)

# GID PEB SIR Team-built R package - use to read data from ADLS, only (re)install if not already available
# https://github.com/CDCgov/sirfunctions

if (!requireNamespace("sirfunctions", quietly = TRUE)) {
  devtools::install_github("nish-kishore/sirfunctions")
}

# load libraries
library(tidyverse)
library(readxl)
library(AzureStor)
library(Microsoft365R)
library(janitor)
library(scales)
library(sirfunctions)
library(sf)

### IMPORT READ GID REFERENCE DATA #############################################
# Reference data is stored on GID Kx SharePoint site as "GID ME&A Data Dictionary" 

# Define names of sharepoint site (url), document library (drive), and file name (path)
sp_site_url  <- "https://cdc.sharepoint.com/sites/CGH-GID-Knowledge-Exchange/OD"
sp_drive_name <- "ME&A Document Library"
sp_file_path  <- "GID_M&E_Data_Dictionary.xlsx"

# Get SharePoint Site
sp_site <- Microsoft365R::get_sharepoint_site(site_url = sp_site_url) # access site, browser will open for authentication using MS Graph
sp_drive <- sp_site$get_drive(sp_drive_name)

## If troubleshooting needed -
# sp_site$list_drives()  # see available document libraries
# sp_drive_check <- sp_site$get_drive(sp_drive_name) # drive defined above (ME&A Doc Library)
# sp_drive_check$list_items("") # see files within ME&A Document Library

# Download to a temp ref data file, then read every sheet
tmp_xlsx <- tempfile(fileext = ".xlsx")
sp_drive$download_file(sp_file_path, dest = tmp_xlsx, overwrite = TRUE)
sheet_names <- readxl::excel_sheets(tmp_xlsx)

# Read data
gid_ref <- purrr::map(sheet_names, ~ readxl::read_excel(tmp_xlsx, sheet = .x)) %>%
  setNames(sheet_names)

# Clean list element list names and each column name
names(gid_ref) <- janitor::make_clean_names(names(gid_ref))
gid_ref <- purrr::map(gid_ref, janitor::clean_names)


## CLEAN REFERENCE DATA #######################################################
# Attach color hex codes and merge with shape file (to use for map visualizations)

## Country Key ############################
# For country key, attach color hex codes and merge with shape file
gid_ref$country_key <- gid_ref$country_key %>% clean_names()

### Update shape file data ####

## read in shape data

# read in shape data
ctry.sp <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_clean/ctry_shapes.rds")

setdiff(ctry.sp$iso_a3, gid_ref$country_key$iso3_code)
norefdata <- ctry.sp[ctry.sp$iso_a3 %in% setdiff(ctry.sp$iso_a3, gid_ref$country_key$iso3_code), ] # geographies that won't match; -99 missing
norefdata$admin # geometries that don't match country reference

# update iso3 codes for geometries that are "true countries"
# for geographies without official iso3 codes, set to 3-char so no missing
ctry.sp <- ctry.sp %>% mutate(iso_a3 = case_when(
  admin == "Norway" ~ "NOR",
  admin == "France" ~ "FRA",
  admin == "Kosovo" ~ "RKS",
  admin == "Somaliland" ~ "000",
  admin == "Northern Cyprus" ~ "111",
  admin == "Indian Ocean Territories" ~ "222",
  admin == "Ashmore and Cartier Islands" ~ "333",
  admin == "Siachen Glacier" ~ "444",
  TRUE ~ iso_a3
)) %>% rename(iso3_code = iso_a3)

# for non "true countries", update country ref data so no data loss if vaues exist
norefdata <- ctry.sp[ctry.sp$iso3_code %in% setdiff(ctry.sp$iso3_code,gid_ref$country_key$iso3_code), ]
# setdiff(ctry.sp$iso3_code, gid_ref$country_key$iso3_code)

#  Prepare data to integrate with country ref table
new_rows <- norefdata %>%
  select(admin, iso3_code, region_un) %>% # Select relevant columns
  rename(country_name = admin) %>% # Rename for consistency
  mutate(gid_primary_geography = "Excluded", # Fill in default values for other columns
         country_abbrev = iso3_code, # set as same
         who_member = "not",
         gid_region_abbr = NA_character_,
         gid_region_name_primary = NA_character_,
         iso2 = NA_character_,
         gid_critical_countries = "No",
         who_region = NA_character_,
         unicef_region = NA_character_) %>%
  select(gid_primary_geography, iso3_code, country_name, country_abbrev, everything()) # Ensure column order matches

# Where considered part of WHO region, add; otherwise, leave as NA
# others are contested regions or territories w/o perm. populations - not considered as part of WHO region
new_rows <- new_rows %>% mutate(gid_region_abbr = case_when(
  country_name == "Somaliland" ~ "EMR", # part of Somalia
  country_name == "Aland" ~ "EUR", # autonomous region of Finland
  country_name == "Northern Cyprus" ~ "EUR", # recognized only by Turkey; others part of Cyprus
  country_name == "Indian Ocean Territories" ~ "WPR",
  country_name == "Norfolk Island" ~ "WPR",
  TRUE ~ gid_region_abbr # keep others as missing
)
)

new_rows <- new_rows %>% mutate(gid_region_name_primary = case_when(
  country_name == "Somaliland" ~ "Eastern Mediterranean (EMR)", # part of Somalia
  country_name == "Aland" ~ "Europe (EUR)", # autonomous region of Finland
  country_name == "Northern Cyprus" ~ "Europe (EUR)", # recognized only by Turkey; others part of Cyprus
  country_name == "Indian Ocean Territories" ~ "Western Pacific (WPR)",
  country_name == "Norfolk Island" ~ "Western Pacific (WPR)",
  TRUE ~ gid_region_name_primary # keep others as missing
)
)

new_rows <- new_rows %>% mutate(who_region = case_when(
  country_name == "Somaliland" ~ "EMRO", # part of Somalia
  country_name == "Aland" ~ "EURO", # autonomous region of Finland
  country_name == "Northern Cyprus" ~ "EURO", # recognized only by Turkey; others part of Cyprus
  country_name == "Indian Ocean Territories" ~ "WPRO",
  country_name == "Norfolk Island" ~ "WPRO",
  TRUE ~ who_region # keep others as missing
)
)

new_rows <- new_rows %>% mutate(unicef_region = case_when(
  country_name == "Somaliland" ~ "ESARO", # part of Somalia
  country_name == "Aland" ~ "ECARO", # autonomous region of Finland
  country_name == "Northern Cyprus" ~ "ECARO", # recognized only by Turkey; others part of Cyprus
  country_name == "Indian Ocean Territories" ~ "EAPRO",
  country_name == "Norfolk Island" ~ "EAPRO",
  TRUE ~ unicef_region # keep others as missing
)
)

new_rows <- new_rows %>% mutate(iso2 = case_when(
  country_name == "South Georgia and the Islands" ~ "GS",
  country_name == "British Indian Ocean Territory" ~ "IO",
  country_name == "French Southern and Antarctic Lands" ~ "TF",
  country_name == "Aland" ~ "AX",
  country_name == "Heard Island and McDonald Islands" ~ "HM",
  country_name == "Norfolk Island" ~ "NF",
  TRUE ~ iso2 # keep others as missing
)
)

# Bind new rows with existing country ref data
all_country_data <- bind_rows(gid_ref$country_key, new_rows)

## CHECK - NO ISO3 DUPLICATES
#duplicated(all_country_data$iso3_code) %>% unique() # should be FALSE

## CHECK - ALL ROWS RETAINED - should be TRUE
#nrow(gid_ref$country_key) + nrow(norefdata) == nrow(all_country_data)

# Calculate centroids for each geometry & extract coords and add to shapefile
centroids <- suppressWarnings(sf::st_centroid(ctry.sp)) # warning msg that fx assumes attributes are constant over geometries (ok for use case)
centroid_coords <- sf::st_coordinates(centroids)

# Calculate centroids for each geometry & extract coords and add to shapefile
ctry.sp <- ctry.sp %>%
  mutate(centroid_longitude = centroid_coords[, 1],   # Longitude from centroid_coords
         centroid_latitude = centroid_coords[, 2],    # Latitude from centroid_coords
         centroid_geometry = sf::st_geometry(centroids) # centroid point geo
  ) %>% select(-gid_priority) # remove col - avoids duplicate/conflicting column on later join with all_country_data

# attach country names with matching iso3_codes
ctry.sp_updated <- ctry.sp %>% left_join(
  all_country_data, by = "iso3_code"
) %>% select(-admin) # duplicative column with country name

### Color scheme for Countries ####
all_country_data <- all_country_data %>% mutate(country_hex = case_when(
  iso3_code == "AFG" ~ "#a51d42",
  iso3_code == "BRA" ~ "#D5006D",
  iso3_code == "COD" ~ "#00a266",
  iso3_code == "ETH" ~ "#007B7A",
  iso3_code == "IDN" ~ "#582d90",
  iso3_code == "NGA" ~ "#334a54",
  iso3_code == "PAK" ~ "#D76B6E",
  iso3_code == "PHL" ~ "#A76DAB",
  TRUE ~ NA_character_ # no colors assigned yet to other countries
)
)

# Binary color scheme
all_country_data <- all_country_data %>% mutate(cc_hex_bin = case_when(
  gid_critical_countries=="Yes" ~ "#007B7A", # is GID Priority Country
  gid_critical_countries=="No"~ "#afabab", # is NOT GID Priority Country
  TRUE ~ NA_character_ # no colors assigned yet to other countries
  )
)

## resave country shape file 
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", 
                      file_loc = "data_clean/ctry_shapes_updated.rds", obj=ctry.sp_updated)

## Join country reference  & spatial data, not retaining as sf class
all_country_data <- all_country_data %>% left_join(
  ctry.sp, by="iso3_code"
)

## resave country reference data
sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", 
                      file_loc = "data_clean/ref_country.rds", obj=all_country_data)

## VPD Key #######################################################################

### Color scheme ####

# For VPD Key, attach color hex codes and covert variables to factor to order by priority (not alpha)
gid_ref$vpd_key <- gid_ref$vpd_key %>% mutate(
  vpd_hex = case_when(
    vpd_short_name == "Measles" ~ "#a51d42",
    vpd_short_name == "Rubella" ~ "#CC79A7",
    vpd_short_name == "CRS" ~ "#FF9999",
    vpd_short_name == "Polio" ~ "#009E73",
    vpd_short_name == "Cholera" ~ "#0072B2",
    vpd_short_name == "Ebola" ~ "#FF5733",
    vpd_short_name == "Mpox" | vpd_short_name == "Monkeypox" ~ "#C77CFF",
    vpd_short_name == "Meningitis" ~ "#D55E00",
    vpd_short_name == "Diphtheria" ~ "#6A5ACD",
    vpd_short_name == "Yellow fever" ~ "#E69F00",
    vpd_short_name == "Neonatal tetanus" ~ "#86e0b5",
    vpd_short_name == "Tetanus (all)" ~ "#A5C1BA",
    vpd_short_name == "Typhoid" ~ "#8DA0CB",
    vpd_short_name == "COVID-19" ~ "#A3A500",
    vpd_short_name == "Jap encephalitis" ~ "#007B7A",
    vpd_short_name == "Pertussis" ~ "#041c3a",
    vpd_short_name == "Mumps" ~ "#66B3FF",
    vpd_short_name == "Malaria" ~ "#B8860B",
    vpd_short_name == "Tuberculosis" ~ "#8B008B",
    TRUE ~ "#afabab" # Use gray for others/not assigned
  )
)

top_priority_vpds <- c("Polio","cVDPV","WPV","Measles", # GID named-VPD appropriations (PA1, PA2)
                   "Rubella", "CRS", # GID appropriations adjacent
                   "Ebola","Cholera","Yellow fever", "Mpox","Diphtheria", "COVID-19", # large or disruptive outbreaks (PA3)
                   "Typhoid", "Hep B", "Neonatal Tetanus", "Tetanus (all)", "Cervical cancer") # other vaccine priorities

lower_priority_vpds <- setdiff(unique(gid_ref$vpd_key$vpd_short_name), top_priority_vpds)

# Set a standard order to VPDs for visualizations based on priority within GID
std_vpd_order <- c(top_priority_vpds, lower_priority_vpds)

# new column to specify
gid_ref$vpd_key <- gid_ref$vpd_key %>% mutate(gid_priority_bin=case_when(
  vpd_short_name %in% top_priority_vpds ~ "Yes",
  TRUE ~ "No"
))

# convert to factor
gid_ref$vpd_key$vpd_short_name <- factor(gid_ref$vpd_key$vpd_short_name,
                                         levels = std_vpd_order)


## Priority Area Key #######################################################################

### Color scheme ####
gid_ref$x2025_28_gid_pa_key <- gid_ref$x2025_28_gid_pa_key %>% mutate(
  priority_area_hex = case_when(
    str_detect(gid_priority_area_2025_28_short_name, regex("Polio", ignore_case = TRUE)) ~ "#1F5FA8",
    str_detect(gid_priority_area_2025_28_short_name, regex("MR", ignore_case = TRUE)) ~ "#E8752A",
    str_detect(gid_priority_area_2025_28_short_name, regex("Emergencies", ignore_case = TRUE)) ~ "#009B4D",
    str_detect(gid_priority_area_2025_28_short_name, regex("PPC", ignore_case = TRUE)) ~ "#B8975A",
    str_detect(gid_priority_area_2025_28_short_name, regex("M&O", ignore_case = TRUE)) ~ "#4A4A4A",
    TRUE ~ "#afabab" # catch-all / not yet assigned
  )
)

## GISF Priority Key #######################################################################

### Color scheme ####
gid_ref$gisf_key <- gid_ref$gisf_key %>% mutate(
  gisf_goal_hex = case_when(
    str_detect(cdc_gisf_goals_short, regex("Prevent", ignore_case = TRUE)) ~ "#556B2F",
    str_detect(cdc_gisf_goals_short, regex("Detect", ignore_case = TRUE)) ~ "#C1440E",
    str_detect(cdc_gisf_goals_short, regex("Respond", ignore_case = TRUE)) ~ "#7B241C",
    str_detect(cdc_gisf_goals_short, regex("Sustain", ignore_case = TRUE)) ~ "#5D7A8C",
    str_detect(cdc_gisf_goals_short, regex("Innovate", ignore_case = TRUE)) ~ "#4A2545",
    str_detect(cdc_gisf_goals_short, regex("Partnership-based Principle", ignore_case = TRUE)) ~ "#B8975A",
    str_detect(cdc_gisf_goals_short, regex("N/A", ignore_case = TRUE)) ~ "#4A4A4A", # N/A: CDC Internal
    TRUE ~ "#afabab" # catch-all / not yet assigned
  )
)

## SAVE REF DATA TO ADLS ####
## Save key tables as R dataframes to GID DATT ADLS in clean data folder
for (table_name in names(gid_ref) ) {
  
  # if the table is the country key (already updated/saved above) or not a key, skip it
  if ( table_name == "country_key" | (!grepl("key", table_name)) ) { # exclude (skip in loop)
    next
  }
  
  # otherwise, save to ADLS folder
  prefix <- "data_clean/ref_"
  
  # Strip trailing "_key" or "_key_<number>" (e.g. "budget_key_2" -> "budget_2")
  fi_name <- paste0(prefix, sub("_key(_\\d+)?$", "\\1", table_name), ".rds")
  
  # save
  sirfunctions::edav_io(io = "write", default_dir = "GID/GIDMEA/giddatt", 
                        file_loc = fi_name, obj=gid_ref[[table_name]])
  
  cat("Saved:", fi_name, "\n")  # Print confirmation message
  
}


