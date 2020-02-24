library(tseries)
library(forecast)

f <- read.csv(file = "~/Downloads/Seanna/tb_wide.csv")

vars <- c('ec','ph','turb','tn','nh4n','no3n','tp','po4','doc')
post <- c("","_avg","_avg_grams","_avg_kg","_avg_ha")

#hax
f[,unlist(lapply(post[3:5], function(p) paste0(vars[1:3], p)))] <- NA

res <- lapply(split(f, f$subws_name), function(h) {
  lapply(split(g, g$season), function(h) {
    i <- as.list(1:length(post))
    n <- lapply(i, function(j) {
      k <- paste0(vars, post[[j]])
      if(nrow(h) > 3)
        l <- try(apply(h[,k], 2, FUN = function(m) {
          m.n <- m[!is.na(m)]
          if(length(m.n) > 3 & length(unique(m.n)) > 1)
            suppressWarnings(c(
            `shapiro`=shapiro.test(m), 
            `n`=length(m.n), 
            `adf`=adf.test(m.n),
            `adfdif`=adf.test(diff(m.n, 
                                   lag=frequency(m.n),
                                   differences=nsdiffs(m.n)))))
        }))
    })
    names(n) <- c("raw","roll","grams","kilos","area")
    return(n)
  })
})
