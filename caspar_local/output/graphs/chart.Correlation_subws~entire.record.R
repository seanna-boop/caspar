# this script makes ggcorrplots plots for each combination of grouping elements ~line 24

library(tidyverse)
library(devtools)
library(ggcorrplot)
source("R/config.R")

f <- read.csv(file = "/Users/seanna/Documents/caspar_local/output/tb/tb_wide_5.2_Pre.Post.csv", stringsAsFactors=FALSE)

f$subws_name <- factor(f$subws_name)
f$season <- factor(f$season)
f$period_name <- factor(f$period_name)
f$Pre.Post.type <- factor(f$Pre.Post.type)

# View(f)

# Pre.Post_filter <- c("pre.yard", "post.yard")
# only include entries with NA for method and units (filters out ravg, grams, kg, ha)
f.sub <- filter(f, !is.na(Pre.Post.type))
# View(f.sub)
# str(f.sub)

# split so that each list element is one plot
dat <- group_split(f.sub, subws_name)

# loop over each group and plot
n <- length(dat)

plots <- lapply(dat[1:n], function(grp) {
  
    subws.name <- unique(as.character(grp$subws_name))
    
    
    wide_dat <- grp %>%
      #spread(variable, value) %>% 
      select(firstq, lastq, volume_cf, mean_Q_cfs, rain_ft, runoff_ft, 
             runoff_ratio, ec, ph, turb, tn, nh4n, no3n, tp, po4, doc)
   
   # warnings generated for sd of 0
   mtrx <- suppressWarnings(cor(wide_dat, use = "pairwise.complete.obs"))
   
   # use collapse to concatenate by underscore
   file.name  <- paste0(subws.name, ".png")
   ###############################################################################################################
   #*************************************************************************************************************
   png(filename = paste0("output/graphs/p.mat/subws~entire.record/", 
                         file.name), height = 1000, width = 1000)
   
   res1 <- cor.mtest(wide_dat, conf.level = .90)
   
   (corrplot::corrplot.mixed(mtrx, p.mat = res1$p, sig.level = 0.1, insig = "blank", 
                             mar=c(0,0,1,0), title = paste0(file.name, "_conf.level=0.9")))
   
   if(!dir.exists("output/graphs/p.mat/subws~entire.record"))
      dir.create("output/graphs/p.mat/subws~entire.record", recursive = TRUE)
   

   dev.off()
   
   # calling this function again (line 50), renders each plot in the r studio pane
   (corrplot::corrplot.mixed(mtrx, p.mat = res1$p, sig.level = 0.1, insig = "blank", 
                             title = paste0(file.name)))
   
   print(file.name)

})









