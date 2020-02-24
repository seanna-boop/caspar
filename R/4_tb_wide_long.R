
source("R/config.R")

library(tidyverse)
library(stringr)

list_of_files <- list.files(path = file.path(dir_out, "period_files"),
                            pattern = ".csv",
                            full.names = TRUE)
df <- list_of_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE), .id = "file_name")
 # View(df)
# class(df$file_name)

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
head(tb)

# index_pre <- c("")


# tb$period_name <- as.factor(tb$period_name)
# 
# tb$segment <- factor(tb$segment, levels = c(1:9))
# tb$subws_name <- factor(tb$subws_name, levels = c("wil", "uql", "tre", "zie", "que", "sfc"))
# tb$percent_red <- factor(tb$percent_red, levels= c("0", "35", "55", "75", "SFC outlet"))
# tb$percent_slope <- factor(tb$percent_slope, levels= c("43", "47", "49", "50", "51", "60"))
# tb$area_ha <- factor(tb$area_ha, levels= c("12.5", "14.1", "25.3", "26.5", "424", "394.3"))


# View(tb)

tb <- tb %>% 
  select(subws_name, full_name:area_ha, everything(), -id, -branch, -date_group, -SUBWS) 
# View(tb)
str(tb)

###################################################################################
library(lubridate)
tb$date = as.Date(as.POSIXct(tb$datetime, tz="UTC"))
tb$year = as.factor(year(tb$datetime))
tb$month = as.factor(month(tb$datetime))

tb$day = day(tb$datetime)
# View(tb)
str(tb)

index_month <- as.character(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
class(index_month)

season <- as.character(c("winter", "winter", "spring", "spring", "spring", "summer",
            "summer", "summer", "fall", "fall", "fall", "winter"))

tb$season <- season[match(tb$month, index_month)] 
str(tb)
# View(tb)



if(!dir.exists(file.path(dir_out, "tb")))
  dir.create(file.path(dir_out, "tb"), recursive = TRUE)
write_csv(path = file.path(dir_out, "tb", "tb_wide_2.csv"),
          x = tb)

# index_5 <- c("ph","ec", "tn", "nh4n", "no3n", "tp", "po4", "doc")
# units <- c("ph", "ec", "mg_l", "mg_l", "mg_l", "mg_l", "mg_l", "mg_l")
# tb$units <- units[match(tb$element, index_5)]
# class(melt_tb$units)
####################################################################################
library(reshape2)
melt_1 <- melt(tb)
# View(melt_1)
melt_tb <- melt(tb, id.vars = c("subws_name", "full_name", "period_name", "segment", 
                "percent_red", "percent_slope", "area_ha", "file_name", "year", "month", "season", "datetime", "time_round"))

# View(melt_tb)

library(stringr)
# new_tb <- tb %>% 
#   separate(full_name, c("species", "avg", "kg"), "_") 


# View(melt_tb)
########################################################################
element <- paste(c("ph", "ec", "tn", "no3n", "nh4n", "tp", "po4", "doc"), collapse = "|")
method <- paste(c("avg"), collapse = "|")
unit <- paste(c("grams", "kg", "ha"), collapse = "|")

melt_tb <- melt_tb %>% 
  mutate(element = str_extract(melt_tb$variable, element)) %>% 
  mutate(method = str_extract(melt_tb$variable, method)) %>% 
  mutate(unit = str_extract(melt_tb$variable, unit)) %>% 
  select(subws_name, variable, value, element, method, unit, year, month, season, period_name, 
                  segment, percent_red, percent_slope, area_ha, datetime, everything())
# str_extract(melt_tb$variable, tofind)
unique(melt_tb$unit)
# View(melt_tb)
class(melt_tb$method)


# this splits variable names into strings...Havent figured out how to 
# ignore (print NA) strings that are not followed by a "-" 
# (ie sample, datetime, time_round...etc)
# library(stringr)
# melt_tb <- melt_tb %>%
#   mutate(element = word(melt_tb$variable, 1, sep = "\\_")) %>% 
#   mutate(method = word(melt_tb$variable, 2, sep = "\\_")) %>% 
#   mutate(units = word(melt_tb$variable, 3, sep = "\\_")) %>% 
#   select(year:units, everything())
# View(melt_tb)
# str(melt_tb)
# class(melt_tb$method)
# class(matches)





if(!dir.exists(file.path(dir_out, "tb")))
  dir.create(file.path(dir_out, "tb"), recursive = TRUE)

# write whole dataset for period to file

write_csv(path = file.path(dir_out, "tb", "tb_long.csv"),
          x = melt_tb)

# not sure if i should assign factor levels to element column yet 
# melt_tb$element <- factor(tb$element, levels= c("tn", "nh4n", "no3n", "tp", "po4", "doc"))

# unique(melt_tb$variable)
