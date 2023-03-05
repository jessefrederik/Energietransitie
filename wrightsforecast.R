library(tidyverse)
library(MASS)
library(googlesheets4)
library(scales)

IRENA2022 <- read.csv("~/Documents/GitHub/Energietransitie/datasets.csv/IRENA2022.csv")




cumcap <- IRENA2022$cumcap 
prijs <- IRENA2022$installation_cost
year <- IRENA2022$year

log(prijs)
logprice <- log(prijs)
sl <- diff(logprice)
logz <- log(cumcap)
horizon <-  2040  ##

pd_sl <- fitdistr(sl, "normal")

year_pred <- (last(year)+1):horizon
delta_t <- year_pred - year[length(year)]


ratio <- (cumcap[length(cumcap)] / cumcap[length(cumcap)-10])^(1/10) - 1
z_pred_median <- rep(0, horizon - max(year))
for(i in 1:length(z_pred_median)) {
  z_pred_median[i] <- cumcap[length(cumcap)] * (1 + ratio)^(i)
}



n <- 10000
sl2 <- diff(log(prijs)) / diff(log(cumcap))
pd_sl2 <- fitdistr(sl2, 'normal')


sl2_mc <- matrix(rnorm(n * length(z_pred_median), 
                       mean = pd_sl2$estimate[1], 
                       sd = pd_sl2$estimate[2]), 
                 nrow = n, 
                 ncol = length(z_pred_median))

price_pred_w <- matrix(0, nrow = n, ncol = length(z_pred_median))
for (i in 1:length(z_pred_median)) {
  if (i == 1) {
    price_pred_w[,i] <- prijs[length(prijs)]*(z_pred_median[i]/cumcap[length(cumcap)])^sl2_mc[,i]
  } else {
    price_pred_w[,i] <- price_pred_w[,i-1]*(z_pred_median[i]/z_pred_median[i-1])^sl2_mc[,i]
  }
} 

price_pred_w <- data.frame(price_pred_w)

price_pred_median_w <- sapply(price_pred_w, median)

price_pred_05th_w <- sapply(price_pred_w, quantile, 0.05)
price_pred_25th_w <- sapply(price_pred_w, quantile, 0.25)
price_pred_10th_w <- sapply(price_pred_w, quantile, 0.10)
price_pred_75th_w <- sapply(price_pred_w, quantile, 0.75)
price_pred_50th_w <- sapply(price_pred_w, quantile, 0.50)
price_pred_90th_w <- sapply(price_pred_w, quantile, 0.90)
price_pred_95th_w <- sapply(price_pred_w, quantile, 0.95)


price_all_median_w <- c(prijs, price_pred_median_w)

a<- data.frame(year = year_all, 
               installation_cost = price_all_median_w) %>%
  mutate(prog = "Gerealiseerd")
b <- data.frame(year = year_pred,
                z_pred_median,
           price_pred_05th_w,
           price_pred_10th_w,
           price_pred_25th_w,
           price_pred_50th_w,
           price_pred_75th_w,
           price_pred_90th_w,
           price_pred_95th_w)

wrightsforecast <- merge(a,b, by ="year", all=T) %>%
  dplyr::select(year, 
                cumcap = z_pred_median,
                median_installation_cost = installation_cost,
                conf25_installation_cost = price_pred_25th_w, 
                conf75_installation_cost = price_pred_75th_w, 
                conf5_installation_cost = price_pred_05th_w, 
                conf95_installation_cost = price_pred_95th_w) %>%
  filter(year > 2021) %>%
  mutate(prog = "Wet van Wright") 
