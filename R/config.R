# vector of sub-watershed names
subws_names <- c("tre", "uql", "wil", "zie", "sfc", "que")

# named list of numerics containing sub-watershed area in hectares
subws_areas <- list("tre" = 14.1, "uql" = 12.5, "wil" = 26.5,
                     "zie" = 25.3, "sfc" = 424, "que" = 394.3)

# SCRIPT 1
dir_in_raw <- "input/raw"

dir_out_processed <- "output"

dir_out_flux <- file.path(dir_out_processed, "flux_files")

# SCRIPT 2
dir_in_periods <- dir_out_flux

period_dates_file <- "C:/Users/sgm/Google Drive/Caspar Creek_sgm/NEW_data/processed/period_dates/period_dates_file_2.csv"

periods <- list("alltime", "fell_and_yard_period", "fell_period", "HY17", "HY18", 
                "HY19", "HY20", "post_fell", "post_yard", "pre_fell", "pre_yard", 
                "yard_period", "wet_years", "dry_years")

dir_out_periods <- "C:/Users/sgm/Google Drive/Caspar Creek_sgm/NEW_data/processed/period_files/"


# SCRIPT 3
dir_in_seasons <- dir_out_periods

season_dates_file <- "C:/Users/sgm/Google Drive/Caspar Creek_sgm/NEW_data/processed/season_dates/season_dates_file_2.csv"

seasons <- list("alltime", "baseflow", "fall_wetup", "rainy", "spring", 
                "storm_events")

dir_out_seasons <- "C:/Users/sgm/Google Drive/Caspar Creek_sgm/NEW_data/processed/season_files/"

psegs <- list("1", "2", "3", "4", "5", "6", "7", "8", "9")

ssegs <- list("1", "2", "3", "4", "5", "6", "7", "8", "9")