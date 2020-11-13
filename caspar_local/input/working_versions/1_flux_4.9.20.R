library(tidyverse)
library(lubridate)
library(measurements)
library(tibbletime)
Sys.setenv(TZ="America/Los_Angeles")


source("R/config.R")

# get input files (flow + chem files for each sub-watershed)
input_files <- list.files(file.path(dir_in, "raw"))

if(!dir.exists(dir_out))
  dir.create(dir_out, recursive = TRUE)

# find flow data
flo_files <- input_files[grepl(input_files, pattern="flo")]
flo_files

# find chem data
chem_files <- input_files[grepl(input_files, pattern="chem")]
chem_files

# find rainfall data
rain_files <- input_files[grepl(input_files, pattern="rnf")]
rain_files
# load the flow files, convert DATE+TIME to POSIXdatetime
tb_flo <- suppressWarnings(lapply(file.path(dir_in, 'raw', flo_files), 
                                   read_csv, col_names=TRUE))
tb_flo <- lapply(tb_flo, mutate, 
                  datetime = as.POSIXct(paste0(DATE, " ", TIME),
                                        format="%m/%d/%Y %H:%M",
                                        tz="Etc/GMT+7"))

# load the chem file(s), convert dt to POSIX datetime (in this case only one file)
tb_chem <- suppressWarnings(lapply(file.path(dir_in, 'raw', chem_files), 
                                   read_csv, col_names=TRUE))
tb_chem <- lapply(tb_chem, mutate, datetime = as.POSIXct(datetime,
                                                         format="%m/%d/%Y %H:%M", 
                                                         tz="Etc/GMT+7"))
tb_chem[[1]]$datetime <- as.POSIXct(tb_chem[[1]]$datetime, 
                                    # format="%m/%d/%Y %H:%M",  ##################################ask why it throws error
                                    # when this format line is included??
                                    tz="Etc/GMT+7")
# View(tb_chem[[1]])

# load the rain file(s), convert dt to POSIX (in this case, again, only one file)
tb_rain <- suppressWarnings(lapply(file.path(dir_in, 'raw', rain_files), 
                                   read_csv, col_names=TRUE))
# View(tb_rain[[1]])
# set rainfall column names
colnames(tb_rain[[1]]) <- c("seconds","rnf_mm","datetime")
# View(tb_rain[[1]])

###########################
# tb_rain[[1]] <- tb_rain[[1]] %>% 
#   mutate(RAIN_ft = conv_unit(tb_rain[[1]]$rnf_mm, "mm", "ft"))
# View(tb_rain[[1]])
####################################

# View(tb_rain[[1]])

tb_rain <- lapply(tb_rain, mutate,  datetime = as.POSIXct(datetime, 
                                                     # note lowercase y in date format
                                                          format="%m/%d/%Y %H:%M", 
                                                          tz="Etc/GMT+7"))
# View(tb_rain[[1]])
tb_rain[[1]]$datetime <- as.POSIXct(paste0("20", as.character(tb_rain[[1]]$datetime)), 
                                    # format="%m/%d/%Y %H:%M",  ##################################ask why it throws error
                                    # when this format line is included??
                                    tz="Etc/GMT+7")
class(tb_rain[[1]]$datetime)

# list of tbl, produced by iterating over sub-watersheds by name
tb_merge <- lapply(as.list(subws_names), function(subws_name) {
  
  # use subws_name as pattern to search for flow file, get index
  idx_flo <- lapply(subws_name, function(f) {
    which(grepl(f, x = flo_files))
  })
  
  # look for individual subws chem files, or file containing all
  idx_chem <- lapply(paste0(subws_name, "|all"), function(f) {
    which(grepl(f, x = chem_files))
  })
  
  # only process if we have a non-NULL index for flow & chem
  if(length(idx_flo[[1]]) & length(idx_chem[[1]])) {
    idx_flo <- idx_flo[[1]]
    idx_chem <- idx_chem[[1]]
    # get a tb.flow element corresponding to idx.flow
    # sort by datetime
    # calculate deltaT between flow readings
    flo <- tb_flo[[as.numeric(idx_flo)]] %>% 
      arrange(datetime) %>% 
      mutate(dlt = lead(datetime) - datetime)
    
    # merge in the rainfall data for matching datetime
    rnf <- tb_rain[[1]]
    flo <- inner_join(flo, rnf, by = "datetime")

    # create attribute with sub-watershed in flow table
    flo$subws <- subws_name
    
    # get chem data from list always 1
    chem <- tb_chem[[as.numeric(idx_chem)]]
    
    # round time
    chem$time_round <- round_date(chem$datetime, unit="10 minutes")
    
    # find subwatershed code in chem file, also check if it is within flow range
    chem_subws_idx <- which(grepl(chem$SUBWS, pattern=subws_name, ignore.case = TRUE) & chem$time_round %in% flo$datetime)
    
    chem_subws <- chem[chem_subws_idx,] %>% 
      arrange(datetime)
    
    dates_between <- chem_subws$time_round
    
    # create a function for assigning date groups,
    # we will apply to both flow and chem
    assign_dategroup <- function(newdates, dates_between) {
      res <- rep(NA, length(newdates))
      for(i in 2:length(dates_between)) {
        res[newdates >= dates_between[i - 1] &
                          newdates <= dates_between[i]] <- i
      }
      # fix for date_group 1
      res[newdates< dates_between[1]] <- 1
      return(res)
    }
    
    flo$date_group <- assign_dategroup(flo$datetime, 
                                        chem_subws$time_round)
    chem_subws$date_group <- assign_dategroup(chem_subws$time_round,
                                            chem_subws$time_round)
    
    ## proof: 
    # plot(flow$date_group) # takes a second # reflects data recorder density
    # plot(chem.sub$date_group, col="RED") # straight line pretty much (two hiccups)

    # here is a for-loop based Q_sum that should work correctly to get Qsum
    # Q_sums <- rep(NA, length(dates_between))
    # date.groups <- unique(res$date_group)
    # for(i in 1:length(date.groups)) {
    #   in.group <- which(res$date_group == date.groups[i])
    #    # note that dlt is inside the sum() -- you want to multiply
    #    # vectors of the same length, and return one value per date group
    #   Q_sums[i] <- sum(res$Q[in.group] * as.numeric(res$dlt, 'secs')[in.group])
    # }
    # chem.sub$Q_sums <- Q_sums
    
    # dplyr: calculate Q summed over interval defined by date_group
    res1 <- flo %>%
      group_by(date_group) %>%
      # conversion of dlt (datetime) will be in _minutes_ unless specified
      summarise(volume_cf = sum(Q * as.numeric(dlt, 'secs')),
                rain_mm = sum(rnf_mm),
                mean_Q_cfs = mean(Q),
                sd_Q_cfs = sd(Q),
                min_Q_cfs = min(Q),
                max_Q_cfs = max(Q)) %>% 
      mutate(volume_liters = volume_cf * 28.316847,
             rain_ft = rain_mm * 0.00328084,
             runoff_ft = volume_cf / (subws_areas[[subws_name]] * 107639),
             runoff_ratio = rain_ft / runoff_ft)
# View(res1)
#   }
# 
#   return(NULL)
# })

# mutate() takes rolling average of every 2 (ec:doc) raw values 
# (concentration, uS/cm, pH, NTU) and creates (ec_ravg:doc_ravg)
    
    roll_avg <- tibbletime::rollify(mean, window = 2)
    res2 <- chem_subws %>%
      mutate_at(vars(ec:doc), list(ravg = roll_avg))
    
    out <- left_join(res2, res1, "date_group")
    
    # shift volumes to correct index for length 2 roll-wise average
    out$vol_shift <- c(NA, out$volume_liters[1:(nrow(out) - 1)])
    
    # define functions (these are _unique_ to out and subws_name)
    grams <- (function(x) (x * out$vol_shift / 1000))
    kg <- (function(x) (x * out$vol_shift / 1000000))
    ha <- (function(x) (x * out$vol_shift / 1000000 / subws_areas[[subws_name]]))
    
    # area correction for rainfall values by watershed -- same units as flow
    #   [rainfall, cm] * [watershed area, cm^2] / [cm^2 per L] (assume density 1)         
    out$rain_liters <- (out$rain_mm / 10) * (subws_areas[[subws_name]] * 1e8) / 1000
    
    # note, using functions that depend on 'out'
    out <- out %>%
      mutate_at(vars(ec:doc), list(~grams(.), ~kg(.),  ~ha(.))) %>% 
      mutate_at(vars(ec_ravg:doc_ravg), list(~grams(.), ~kg(.),  ~ha(.)))
    
    return(out)
    # class(tb_merge) #list
    # class(tb_merge[[1]]) #data.frame
  }
  
  return(NULL)
})


names(tb_merge) <- subws_names

lapply(names(tb_merge), function(subws_name) {
  tb <- tb_merge[[subws_name]] 
  if(length(tb)) {
    # create directories as needed
    if(!dir.exists(paste0(dir_out, "/", subws_name, "/ti")))
      dir.create(paste0(dir_out, "/", subws_name, "/ti"), recursive = TRUE)
    if(!dir.exists(paste0(dir_out, "/flux_files")))
      dir.create(paste0(dir_out, "/flux_files"), recursive = TRUE)
    
    # write files to subws folders and flux file folder
    # write_csv(x = tb, path = file.path(dir_out, subws_name, "ti", paste0(subws_name, "_flux_rr.csv")))
    write_csv(x = tb, path = file.path(dir_out, "flux_files" , paste0(subws_name, "_flux_k.csv")))
  }
})

