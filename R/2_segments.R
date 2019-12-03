# 2_subws-time-slice.R
library(tidyverse)

source("R/config.R")

period_dates <- read_csv(file.path(dir_in, "dates", "period.csv"))
period_dates$start_date <- as.POSIXct(period_dates$start_date, format="%m/%d/%Y")
period_dates$end_date <- as.POSIXct(period_dates$end_date, format="%m/%d/%Y")

# define a function to determine portion of timeseries within a date interval
within_pp <- function(.time, subws, period="alltime") {
  p <- period_dates[which(period_dates$subws == subws & period_dates$period == period), ]
  # only uses first if there are duplicate subws*period combinations
  return(.time <= p$end_date[1] & .time >= p$start_date[1])
}

input_files <- list.files(dir_out_flux, full.names = TRUE)

flux_files <- as.list(input_files[grepl(basename(input_files), pattern="flux")])

lapply(flux_files, function(ff) {
  # read flux file
  flux <- read_csv(ff)

  # get first 3 chars of base file name as subws
  subws <- substr(basename(ff), 0, 3)
  print(subws)

  # loop through list of periods and write to file
  lapply(cfg.periods, function(pp) {
    
    # subset flux file within period
    flux.sub <- flux[within_pp(flux$datetime, subws, pp), ]
  
    # create output dir if needed
    if(!dir.exists(file.path(dir_out, subws ,"ti", pp)))
      dir.create(file.path(dir_out, subws ,"ti", pp), recursive = TRUE)
    if(!dir.exists(file.path(dir_out, "dates", pp)))
      dir.create(file.path(dir_out, "dates", pp), recursive = TRUE)
    
    write_csv(path = file.path(dir_out, subws ,"ti", pp, paste0(subws, "_" , pp, ".csv")), 
              x = flux.sub)
    write_csv(path = file.path(dir_out, "dates", pp, paste0(subws, "_" , pp, ".csv")),
              x = flux.sub)
  })
})

