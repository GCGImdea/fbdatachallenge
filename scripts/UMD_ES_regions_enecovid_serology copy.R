## Libraries
library(dplyr)
library(ggplot2)
library(stringr)


## Load data ----
dt <- read.csv("../data/estimates-umd-batches/ES/ES_UMD_data.csv", 
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

symptoms <- c("pct_cli", "pct_ili", "pct_fever", "pct_cough", "pct_difficulty_breathing", "pct_fatigue", "pct_stuffy_runny_nose", 
              "pct_aches_muscle_pain", "pct_sore_throat", "pct_chest_pain", "pct_nausea", "pct_anosmia_ageusia", "pct_eye_pain", "pct_headache")

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
  aux <- data.frame(
    "regioncode" = r, 
    "pct_cli" = mean(dta$pct_cli), 
    "pct_ili" = mean(dta$pct_ili), 
    "pct_fever" = mean(dta$pct_fever), 
    "pct_cough" = mean(dta$pct_cough), 
    "pct_difficulty_breathing" = mean(dta$pct_difficulty_breathing), 
    "pct_fatigue" = mean(dta$pct_fatigue), 
    "pct_stuffy_runny_nose" = mean(dta$pct_stuffy_runny_nose), 
    "pct_aches_muscle_pain" = mean(dta$pct_aches_muscle_pain), 
    "pct_aches_muscle_pain" = mean(dta$pct_aches_muscle_pain), 
    "pct_chest_pain" = mean(dta$pct_chest_pain), 
    "pct_nausea" = mean(dta$pct_nausea), 
    "pct_anosmia_ageusia" = mean(dta$pct_anosmia_ageusia), 
    "pct_eye_pain" = mean(dta$pct_eye_pain), 
    "pct_headache" = mean(dta$pct_headache)
  )
  dt_r1_s1 <- rbind(dt_r1_s1, aux)
  
  dta <- dtr[(dtr$date >= as.Date("2020-05-05") & dtr$date <= as.Date("2020-05-11")),]
  aux <- data.frame(
    "regioncode" = r, 
    "pct_cli" = mean(dta$pct_cli), 
    "pct_ili" = mean(dta$pct_ili), 
    "pct_fever" = mean(dta$pct_fever), 
    "pct_cough" = mean(dta$pct_cough), 
    "pct_difficulty_breathing" = mean(dta$pct_difficulty_breathing), 
    "pct_fatigue" = mean(dta$pct_fatigue), 
    "pct_stuffy_runny_nose" = mean(dta$pct_stuffy_runny_nose), 
    "pct_aches_muscle_pain" = mean(dta$pct_aches_muscle_pain), 
    "pct_aches_muscle_pain" = mean(dta$pct_aches_muscle_pain), 
    "pct_chest_pain" = mean(dta$pct_chest_pain), 
    "pct_nausea" = mean(dta$pct_nausea), 
    "pct_anosmia_ageusia" = mean(dta$pct_anosmia_ageusia), 
    "pct_eye_pain" = mean(dta$pct_eye_pain), 
    "pct_headache" = mean(dta$pct_headache)
  )
  dt_r1_s2 <- rbind(dt_r1_s2, aux)
  
  dta <- dtr[(dtr$date >= as.Date("2020-05-18") & dtr$date <= as.Date("2020-05-25")),]
  aux <- data.frame(
    "regioncode" = r, 
    "pct_cli" = mean(dta$pct_cli), 
    "pct_ili" = mean(dta$pct_ili), 
    "pct_fever" = mean(dta$pct_fever), 
    "pct_cough" = mean(dta$pct_cough), 
    "pct_difficulty_breathing" = mean(dta$pct_difficulty_breathing), 
    "pct_fatigue" = mean(dta$pct_fatigue), 
    "pct_stuffy_runny_nose" = mean(dta$pct_stuffy_runny_nose), 
    "pct_aches_muscle_pain" = mean(dta$pct_aches_muscle_pain), 
    "pct_chest_pain" = mean(dta$pct_chest_pain), 
    "pct_nausea" = mean(dta$pct_nausea), 
    "pct_anosmia_ageusia" = mean(dta$pct_anosmia_ageusia), 
    "pct_eye_pain" = mean(dta$pct_eye_pain), 
    "pct_headache" = mean(dta$pct_headache)
  )
  dt_r2_s1 <- rbind(dt_r2_s1, aux)
  
  dta <- dtr[(dtr$date >= as.Date("2020-05-25") & dtr$date <= as.Date("2020-06-01")),]
  aux <- data.frame(
    "regioncode" = r, 
    "pct_cli" = mean(dta$pct_cli), 
    "pct_ili" = mean(dta$pct_ili), 
    "pct_fever" = mean(dta$pct_fever), 
    "pct_cough" = mean(dta$pct_cough), 
    "pct_difficulty_breathing" = mean(dta$pct_difficulty_breathing), 
    "pct_fatigue" = mean(dta$pct_fatigue), 
    "pct_stuffy_runny_nose" = mean(dta$pct_stuffy_runny_nose), 
    "pct_aches_muscle_pain" = mean(dta$pct_aches_muscle_pain), 
    "pct_chest_pain" = mean(dta$pct_chest_pain), 
    "pct_nausea" = mean(dta$pct_nausea), 
    "pct_anosmia_ageusia" = mean(dta$pct_anosmia_ageusia), 
    "pct_eye_pain" = mean(dta$pct_eye_pain), 
    "pct_headache" = mean(dta$pct_headache)
  )
  dt_r2_s2 <- rbind(dt_r2_s2, aux)
  
  dta <- dtr[(dtr$date >= as.Date("2020-06-08") & dtr$date <= as.Date("2020-06-15")),]
  aux <- data.frame(
    "regioncode" = r, 
    "pct_cli" = mean(dta$pct_cli), 
    "pct_ili" = mean(dta$pct_ili), 
    "pct_fever" = mean(dta$pct_fever), 
    "pct_cough" = mean(dta$pct_cough), 
    "pct_difficulty_breathing" = mean(dta$pct_difficulty_breathing), 
    "pct_fatigue" = mean(dta$pct_fatigue), 
    "pct_stuffy_runny_nose" = mean(dta$pct_stuffy_runny_nose), 
    "pct_aches_muscle_pain" = mean(dta$pct_aches_muscle_pain), 
    "pct_chest_pain" = mean(dta$pct_chest_pain), 
    "pct_nausea" = mean(dta$pct_nausea), 
    "pct_anosmia_ageusia" = mean(dta$pct_anosmia_ageusia), 
    "pct_eye_pain" = mean(dta$pct_eye_pain), 
    "pct_headache" = mean(dta$pct_headache)
  )
  dt_r3_s1 <- rbind(dt_r3_s1, aux)
  
  dta <- dtr[(dtr$date >= as.Date("2020-06-15") & dtr$date <= as.Date("2020-06-22")),]
  aux <- data.frame(
    "regioncode" = r, 
    "pct_cli" = mean(dta$pct_cli), 
    "pct_ili" = mean(dta$pct_ili), 
    "pct_fever" = mean(dta$pct_fever), 
    "pct_cough" = mean(dta$pct_cough), 
    "pct_difficulty_breathing" = mean(dta$pct_difficulty_breathing), 
    "pct_fatigue" = mean(dta$pct_fatigue), 
    "pct_stuffy_runny_nose" = mean(dta$pct_stuffy_runny_nose), 
    "pct_aches_muscle_pain" = mean(dta$pct_aches_muscle_pain), 
    "pct_chest_pain" = mean(dta$pct_chest_pain), 
    "pct_nausea" = mean(dta$pct_nausea), 
    "pct_anosmia_ageusia" = mean(dta$pct_anosmia_ageusia), 
    "pct_eye_pain" = mean(dta$pct_eye_pain), 
    "pct_headache" = mean(dta$pct_headache)
  )
  dt_r3_s2 <- rbind(dt_r3_s2, aux)

  
}

write.csv(dt_r1_s1, "../data/estimates-umd-batches/ES/serology/r1_s1_UMD_data.csv", row.names = FALSE)
write.csv(dt_r1_s2, "../data/estimates-umd-batches/ES/serology/r1_s2_UMD_data.csv", row.names = FALSE)
write.csv(dt_r2_s1, "../data/estimates-umd-batches/ES/serology/r2_s1_UMD_data.csv", row.names = FALSE)
write.csv(dt_r2_s2, "../data/estimates-umd-batches/ES/serology/r2_s2_UMD_data.csv", row.names = FALSE)
write.csv(dt_r3_s1, "../data/estimates-umd-batches/ES/serology/r3_s1_UMD_data.csv", row.names = FALSE)
write.csv(dt_r3_s2, "../data/estimates-umd-batches/ES/serology/r3_s2_UMD_data.csv", row.names = FALSE)
