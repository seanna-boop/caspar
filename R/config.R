# vector of sub-watershed names
subws_names <- c("tre", "uql", "wil", "zie", "sfc", "que")

# named list of numerics containing sub-watershed area in hectares
subws_areas <- list("tre" = 14.1, "uql" = 12.5, "wil" = 26.5,
                     "zie" = 25.3, "sfc" = 424, "que" = 394.3)

# SCRIPT 1
dir_in_raw <- "input/raw"

dir_out <- "output"

dir_out_flux <- file.path(dir_out_processed, "flux_files")

# SCRIPT 2

cfg.periods <- list("alltime", "fell_and_yard_period", "fell_period", "HY17", "HY18", 
                "HY19", "HY20", "post_fell", "post_yard", "pre_fell", "pre_yard", 
                "yard_period", "wet_years", "dry_years")

cfg.seasons <- list("alltime", "baseflow", "fall_wetup", "rainy", "spring", 
                "storm_events")

psegs <- ssegs <- as.list(as.character(1:9))