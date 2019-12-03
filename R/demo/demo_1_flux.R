#script 1 demo

library(tidyverse)

flx <- read_csv('output/flux_files/tre_flux.csv')

plot(data = flx, volume_liters ~ datetime, type = "l")
axis(4, at = pretty(flx$no3n_avg_ha))

