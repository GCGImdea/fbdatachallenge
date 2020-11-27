## Libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)


## Load data ----
dt <- read.csv("../data/estimates-umd-batches/ES/ES_UMD_data.csv", 
                 fileEncoding = "UTF-8")
dt <- subset(dt, select = -c(country_region_numeric,total_responses, b_size_denom) )

dtene <- read.csv("../data/estimates-umd-batches/ES/serology/enecovid-analysis.csv", 
                fileEncoding = "UTF-8")

## Add 'region' code
dt$region[dt$region_agg == "Andalucía"] <- "ESAN"
dt$region[dt$region_agg == "Aragón"] <- "ESAR"
dt$region[dt$region_agg == "Cantabria"] <- "ESCB"
dt$region[dt$region_agg == "Castilla y León"] <- "ESCL"
dt$region[dt$region_agg == "Castilla-La Mancha"] <- "ESCM"
dt$region[dt$region_agg == "Cataluña"] <- "ESCT"
dt$region[dt$region_agg == "Comunidad Foral de Navarra"] <- "ESNC"
dt$region[dt$region_agg == "Comunidad Valenciana"] <- "ESVC"
dt$region[dt$region_agg == "Comunidad de Madrid"] <- "ESMD"
dt$region[dt$region_agg == "Extremadura"] <- "ESEX"
dt$region[dt$region_agg == "Galicia"] <- "ESGA"
dt$region[dt$region_agg == "Islas Baleares"] <- "ESIB"
dt$region[dt$region_agg == "Islas Canarias"] <- "ESCN"
dt$region[dt$region_agg == "La Rioja"] <- "ESRI"
dt$region[dt$region_agg == "País Vasco"] <- "ESPV"
dt$region[dt$region_agg == "Principado de Asturias"] <- "ESAS"
dt$region[dt$region_agg == "Región de Murcia"] <- "ESMC"
dt$region[dt$region_agg == "Ceuta"] <- "ESMC"
dt$region[dt$region_agg == "Melilla"] <- "ESMC"
dt$region[dt$region_agg == "Ceuta y Melilla"] <- "ESCE_ML"

regions <- unique(dt$region)

dt_r1_s1 <- data.frame()
dt_r1_s2 <- data.frame()
dt_r2_s1 <- data.frame()
dt_r2_s2 <- data.frame()
dt_r3_s1 <- data.frame()
dt_r3_s2 <- data.frame()

for (r in regions) {
  dtr <- dt[dt$region == r,]
  dtr$date <- as.Date(dtr$date)
  
  dta <- dtr[(dtr$date >= as.Date("2020-04-27") & dtr$date <= as.Date("2020-05-05")),]
  aux <- colMeans(dta[sapply(dta, is.numeric)], na.rm=TRUE)
  aux$regioncode <- r
  dt_r1_s1 <- rbind(dt_r1_s1, aux)
  
  dta <- dtr[(dtr$date >= as.Date("2020-05-05") & dtr$date <= as.Date("2020-05-11")),]
  aux <- colMeans(dta[sapply(dta, is.numeric)], na.rm=TRUE)
  aux$regioncode <- r
  dt_r1_s2 <- rbind(dt_r1_s2, aux)
  
  dta <- dtr[(dtr$date >= as.Date("2020-05-18") & dtr$date <= as.Date("2020-05-25")),]
  aux <- colMeans(dta[sapply(dta, is.numeric)], na.rm=TRUE)
  aux$regioncode <- r
  dt_r2_s1 <- rbind(dt_r2_s1, aux)
  
  dta <- dtr[(dtr$date >= as.Date("2020-05-25") & dtr$date <= as.Date("2020-06-01")),]
  aux <- colMeans(dta[sapply(dta, is.numeric)], na.rm=TRUE)
  aux$regioncode <- r
  dt_r2_s2 <- rbind(dt_r2_s2, aux)
  
  dta <- dtr[(dtr$date >= as.Date("2020-06-08") & dtr$date <= as.Date("2020-06-15")),]
  aux <- colMeans(dta[sapply(dta, is.numeric)], na.rm=TRUE)
  aux$regioncode <- r
  dt_r3_s1 <- rbind(dt_r3_s1, aux)
  
  dta <- dtr[(dtr$date >= as.Date("2020-06-15") & dtr$date <= as.Date("2020-06-22")),]
  aux <- colMeans(dta[sapply(dta, is.numeric)], na.rm=TRUE)
  aux$regioncode <- r
  dt_r3_s2 <- rbind(dt_r3_s2, aux)

}

dt_r1_s1 <- dt_r1_s1[order(dt_r1_s1$regioncode),]
dt_r1_s2 <- dt_r1_s2[order(dt_r1_s2$regioncode),]
dt_r2_s1 <- dt_r2_s1[order(dt_r2_s1$regioncode),]
dt_r2_s2 <- dt_r2_s2[order(dt_r2_s2$regioncode),]
dt_r3_s1 <- dt_r3_s1[order(dt_r3_s1$regioncode),]
dt_r3_s2 <- dt_r3_s2[order(dt_r3_s2$regioncode),]

write.csv(dt_r1_s1, "../data/estimates-umd-batches/ES/serology/r1_s1_UMD_data.csv", row.names = FALSE)
write.csv(dt_r1_s2, "../data/estimates-umd-batches/ES/serology/r1_s2_UMD_data.csv", row.names = FALSE)
write.csv(dt_r2_s1, "../data/estimates-umd-batches/ES/serology/r2_s1_UMD_data.csv", row.names = FALSE)
write.csv(dt_r2_s2, "../data/estimates-umd-batches/ES/serology/r2_s2_UMD_data.csv", row.names = FALSE)
write.csv(dt_r3_s1, "../data/estimates-umd-batches/ES/serology/r3_s1_UMD_data.csv", row.names = FALSE)
write.csv(dt_r3_s2, "../data/estimates-umd-batches/ES/serology/r3_s2_UMD_data.csv", row.names = FALSE)

dt_r1_s1$week <- "r1_s1"
dt_r1_s2$week <- "r1_s2"
dt_r2_s1$week <- "r2_s1"
dt_r2_s2$week <- "r2_s2"
dt_r3_s1$week <- "r3_s1"
dt_r3_s2$week <- "r3_s2"

dt_r1_s1$pct_ene_cli <- dtene$r1_s1 / dtene$population * 100
dt_r1_s2$pct_ene_cli <- dtene$r1_s2 / dtene$population * 100
dt_r2_s1$pct_ene_cli <- dtene$r2_s1 / dtene$population * 100
dt_r2_s2$pct_ene_cli <- dtene$r2_s2 / dtene$population * 100
dt_r3_s1$pct_ene_cli <- dtene$r3_s1 / dtene$population * 100
dt_r3_s2$pct_ene_cli <- dtene$r3_s2 / dtene$population * 100

# Omitinh the first week because the is few data
# dtall <- dt_r1_s1
# dtall <- rbind(dtall, dt_r1_s2)
dtall <- dt_r1_s2
dtall <- rbind(dtall, dt_r2_s1)
dtall <- rbind(dtall, dt_r2_s2)
dtall <- rbind(dtall, dt_r3_s1)
dtall <- rbind(dtall, dt_r3_s2)

dtall <- dtall[dtall$pct_ene_cli > 0,]

aux <- colMeans(dtall[sapply(dtall, is.numeric)], na.rm=TRUE)
ratio <- aux["pct_ene_cli"] / aux

write.csv(ratio, "../data/estimates-umd-batches/ES/serology/ratios.csv") #, row.names = FALSE)

to_correct <- dtall[sapply(dtall, is.numeric)]
corrected <- dtall

aerror <- data.frame(signal=character(), abs_error=numeric())

for (s in colnames(to_correct)) {
  cor <- to_correct[s] * ratio[s]
  corrected[s] <- cor
  #mean(abs((dtall$pct_ene_cli-cor$pct_direct_contact_with_non_hh_smooth_low)/dtall$pct_ene_cli)) * 100
  
  mape <- sum(abs(cor - to_correct["pct_ene_cli"]) / to_correct["pct_ene_cli"]) / nrow(to_correct)
  aerror <- aerror %>% add_row(signal = s, abs_error = mape)
  cat("mape:", mape)
}

write.csv(aerror, "../data/estimates-umd-batches/ES/serology/abs-error.csv", row.names = FALSE)
write.csv(corrected, "../data/estimates-umd-batches/ES/serology/corrected.csv", row.names = FALSE)
