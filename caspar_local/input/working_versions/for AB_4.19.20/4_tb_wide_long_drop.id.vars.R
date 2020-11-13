Sys.setenv(TZ="America/Los_Angeles")
source("R/config.R")

library(tidyverse)
library(stringr)

list_of_files <- list.files(path = file.path(dir_out, "period_files"),
                            pattern = ".csv",
                            full.names = TRUE)

# CTURB column type causes error here, so skipped - not ideal
df <- list_of_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(CTURB = col_skip()), col_names = TRUE), .id = "file_name")
View(df)
class(df$file_name)

tb <- df %>% 
  mutate(full_name = str_sub(file_name, 21, -5),
         period_name = str_sub(full_name, 5, -3),
         segment = str_sub(full_name, start = -1),
         subws_name = str_sub(full_name, end = 3))


index_harvest <- c("wil", "tre", "uql", "zie", "sfc") 
percent_red <- c("0", "35", "55", "75", "SFC outlet")
tb$percent_red <- percent_red[match(tb$subws_name, index_harvest)]


index_slope <- c("wil", "tre", "uql", "zie", "sfc", "que")
percent_slope <- c("51", "47", "49", "43", "60", "50")
tb$percent_slope <- percent_slope[match(tb$subws_name, index_slope)]
# View(tb)

index_area <- c("wil", "tre", "uql", "zie", "sfc", "que")
area_ha <- c("26.5", "14.1", "12.5", "25.3", "424", "394.3")
tb$area_ha <- area_ha[match(tb$subws_name, index_area)]
# head(tb)

# View(tb)

tb <- tb %>% 
  select(subws_name, full_name:area_ha, everything()) 
# View(tb)
# str(tb)

###################################################################################
library(lubridate)
tb$date = as.Date(as.POSIXct(tb$datetime, tz="UTC"))
tb$year = as.factor(year(tb$datetime))
tb$month = as.factor(month(tb$datetime))
tb$day = as.factor(day(tb$datetime))
# View(tb)
# str(tb)

index_month <- as.character(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
class(index_month)

season <- as.character(c("winter", "winter", "spring", "spring", "spring", "summer",
            "summer", "summer", "fall", "fall", "fall", "winter"))

tb$season <- season[match(tb$month, index_month)] 
str(tb)
View(tb)

                                                                                     

if(!dir.exists(file.path(dir_out, "tb")))
  dir.create(file.path(dir_out, "tb"), recursive = TRUE)
write_csv(path = file.path(dir_out, "tb", "tb_wide_g.csv"),
          x = tb)

####################################################################################
library(reshape2)
# melt_1 <- melt(tb)


melt_tb <- melt(tb, id.vars = c("subws_name", "full_name", "period_name", "segment",
                "percent_red", "percent_slope", "area_ha", "file_name", "sample", "id",
                "branch", "SUBWS", "date_group.x", "date" , "year", "month", "day", "season",
                "datetime", "time_round"))
# melt_tb <- melt(tb, measure.vars = c())
View(melt_tb)


library(stringr)
# new_tb <- tb %>% 
#   separate(full_name, c("species", "avg", "kg"), "_") 
# View(melt_tb)
########################################################################
# ***************************************** 3/29/20
element <- paste(c("ec", "ph", "turb", "tn", "no3n", "nh4n", "tp", "po4", "doc", "don"), collapse = "|")
class(element)
method <- paste(c("ravg"), collapse = "|")
unit <- paste(c("grams", "kg", "ha"), collapse = "|")

melt_tb <- melt_tb %>% 
  mutate(element = str_extract(melt_tb$variable, element)) %>%
  mutate(method = str_extract(melt_tb$variable, method)) %>%
  mutate(unit = str_extract(melt_tb$variable, unit)) %>%
  select(subws_name, element, method, unit, value, year, month, day, season, period_name, 
                  segment, percent_red, percent_slope, area_ha, datetime, everything())
# str_extract(melt_tb$variable, tofind)
unique(melt_tb$unit)
# View(melt_tb)
class(melt_tb$method)




if(!dir.exists(file.path(dir_out, "tb")))
  dir.create(file.path(dir_out, "tb"), recursive = TRUE)

# write whole dataset for period to file

write_csv(path = file.path(dir_out, "tb", "tb_long_g.csv"),
          x = melt_tb)

# not sure if i should assign factor levels to element column yet 
# melt_tb$element <- factor(tb$element, levels= c("tn", "nh4n", "no3n", "tp", "po4", "doc"))
str(melt_tb)
unique(melt_tb$variable)
