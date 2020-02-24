# 2_subws-time-slice.R

#this script outputs period files for each subwatershed, "time period", and segment combo into:
#Google Drive\Caspar Creek_sgm\caspar\output\period_files

library(tidyverse)

source("R/config.R")

period_dates <- read_csv(file.path(dir_in, "period", "period.csv"))
period_dates$start_date <- as.POSIXct(period_dates$start_date, format="%m/%d/%Y")
period_dates$end_date <- as.POSIXct(period_dates$end_date, format="%m/%d/%Y")

# define a function to determine portion of timeseries within a date interval
within_interval <- function(.time, subws, period = "alltime", segment = NULL) {
  idx.who <- which(period_dates$subws == subws & period_dates$period == period)
  
  if(!is.null(segment)) {
    idx.who <- which(period_dates$subws == subws & 
                       period_dates$period == period & 
                       period_dates$pseg == segment)
  }
  flag <- rep(FALSE, length(.time))
  if(length(idx.who)) {
    p <- period_dates[idx.who, ]
    
    for(ip in 1:nrow(p)) {
      ingroup <- .time <= p[ip,]$end_date & .time >= p[ip,]$start_date
      flag <- flag | ingroup
    }
  }
  return(flag)
}

input_files <- list.files(file.path(dir_out, "flux_files"), full.names = TRUE)


flux_files <- as.list(input_files[grepl(basename(input_files), pattern="flux")])


res <- lapply(flux_files, function(ff) {
  # read flux file
  flux <- suppressMessages(read_csv(ff))

  # get first 3 chars of base file name as subws
  subws <- substr(basename(ff), 0, 3)
  print(subws)

  # loop through list of periods and write to file
  lapply(cfg.periods, function(pp) {
    
    period.sub <- period_dates[which(period_dates$subws == subws & period_dates$period == pp),]
    
    # subset flux file within period
    flux.sub <- flux[within_interval(flux$datetime, subws, pp), ]
  
    # create output dir if needed
    # if(!dir.exists(file.path(dir_out, subws ,"ti", pp)))
    #   dir.create(file.path(dir_out, subws ,"ti", pp), recursive = TRUE)
    # 
    # # write whole dataset for period to file
    # write_csv(path = file.path(dir_out, subws ,"ti", paste0(subws, "_" , pp, ".csv")), 
    #           x = flux.sub)  
    # 
    #  # create output dir if needed    
    #  if(!dir.exists(file.path(dir_out, "period_files")))
    #   dir.create(file.path(dir_out, "period_files"), recursive = TRUE)
    # 
    # # write whole dataset for period to file    
    # write_csv(path = file.path(dir_out, "period_files", paste0(subws, "_" , pp, ".csv")),
    #           x = flux.sub)   
    # 
    # print(unique(period.sub$pseg))
    
    # iterate through period sedments
    lapply(unique(period.sub$pseg), function(pseg) {
      
      # subset flux file within period
      flux.sub.sub <- flux[within_interval(flux$datetime, subws, pp, pseg), ]
      
      
      if(nrow(flux.sub.sub)) {
        # create output dir if needed
        
      # if(any(!is.na(flux.sub.sub$volume_liters)))
      #   plot(flux.sub.sub$volume_liters)
      
      if(!dir.exists(file.path(dir_out, subws ,"ti", pp, pseg)))
          dir.create(file.path(dir_out, subws ,"ti", pp, pseg), recursive = TRUE)
        
        write_csv(path = file.path(dir_out, subws ,"ti", pp, pseg, 
                                   paste0(subws, "_" , pp, "_", pseg, ".csv")), 
                  x = flux.sub.sub)
        
        if(!dir.exists(file.path(dir_out, "period_files")))
          dir.create(file.path(dir_out, "period_files"), recursive = TRUE)
        
        # write whole dataset for period to file

        write_csv(path = file.path(dir_out, "period_files", paste0(subws, "_" , pp, "_", pseg, ".csv")),
                  x = flux.sub.sub)
      }
    })
  })
})

