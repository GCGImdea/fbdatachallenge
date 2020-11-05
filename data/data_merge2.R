
##### full survey data country ####

library(dplyr)
read_gz <- function(broken_link){
  funny_gunzip <- gzcon(url(broken_link))  
  funny_gunzip_connection <- readLines(funny_gunzip)
  dt <- read.csv(textConnection(funny_gunzip_connection), as.is = T)
  close(funny_gunzip)
  return(dt)
}


dt_apr <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/04_april_country_full.csv.gz")

dt_may <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/05_may_country_full.csv.gz")

dt_jun <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/06_june_country_full.csv.gz")

dt_jul <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/07_july_country_full.csv.gz")

dt_aug <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/08_aug_country_full.csv.gz")

dt_sep <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/09_sept_country_full.csv.gz")

dt_oct <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/10_oct_country_full.csv.gz")

dt_nov <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/11_nov_country_full.csv.gz")


# bind rows
dtfull <- dt_apr %>% bind_rows(dt_may, dt_jun, dt_jul, dt_aug, dt_sep, dt_oct, dt_nov)

## extract for Spain
esp_country_full <- dtfull %>% filter(GID_0 == "ESP")
write.csv(esp_country_full, "./UMD_updated/Full Survey Data/country/esp_country_full.csv")
bra_country_full <- dtfull %>% filter(GID_0 == "BRA")
write.csv(bra_country_full, "./UMD_updated/Full Survey Data/country/bra_country_full.csv")

# Analyse added and removed questions

# dtfull <-  dt_apr %>% bind_rows(dt_may)   
# names(dt_apr)[!(names(dt_apr) %in% names(dt_may))]
## april to may (in april not in may)
# none

# names(dt_may)[!(names(dt_may) %in% names(dt_apr))]
## in may not in april
# [1] "pct_chills"               "pct_chills_weighted_sums"
# [3] "pct_chills_weighted"

# dtfulljune <- dtfull %>% bind_rows(dt_jun) 
# names(dtfull)[!(names(dtfull) %in% names(dt_jun))]
## may to june (in april or may but not in june)
# none 

# names(dt_jun)[!(names(dt_jun) %in% names(dtfull))]
## in june but not in april or may
# [1] "pct_finances_very_worried"                  
# [2] "pct_finances_somewhat_worried"              
# [3] "pct_finances_notToo_worried"                
# [4] "pct_finances_not_worried"                   
# [5] "pct_finances_very_worried_weighted_sums"    
# [6] "pct_finances_somewhat_worried_weighted_sums"
# [7] "pct_finances_notToo_worried_weighted_sums"  
# [8] "pct_finances_not_worried_weighted_sums"     
# [9] "pct_finances_very_worried_weighted"         
# [10] "pct_finances_somewhat_worried_weighted"     
# [11] "pct_finances_notToo_worried_weighted"       
# [12] "pct_finances_not_worried_weighted"  

# dtfulljuly <- dtfulljune %>% bind_rows(dt_jul)
## june to july (in april, may or june but not in july): names(dtfulljune)[!(names(dtfulljune)  %in% names(dt_jul))] 
# none

## in july but not in previous months: names(dt_jul)[!(names(dt_jul)  %in% names(dtfulljune))]
# character(0)


# dtfullaug <- dtfulljuly %>% bind_rows(dt_aug)
## july to aug (in april, may, june or  july but not august): names(dt_fulljuly)[!(names(dt_fulljuly)  %in% names(dt_august))]
# none

## in aug but not in previous months: names(dt_aug)[!(names(dt_aug)  %in% names(dtfulljuly))]
# character(0)

# dtfullsept <- dtfullaug  %>% bind_rows(dt_sep)
## aug to sept (in april, may, june, july or august not in sept): names(dtfullaug)[!(names(dtfullaug)  %in% names(dt_sep))]
# none    

## in sept but not in previous months: names(dt_sep)[!(names(dt_sep)  %in% names(dtfullaug))]
# character(0)
# dtfulloct <- dtfullsept  %>% bind_rows(dt_oct)
# names(dtfullsept)[!(names(dtfullsept)  %in% names(dt_oct))]
# none

# names(dt_oct)[!(names(dt_oct)  %in% names(dtfullsept))]
# none

# dtfullnov <- dtfulloct %>% bind_rows(dt_nov)
# names(dtfulloct)[!(names(dtfulloct)  %in% names(dt_nov))]
# none

# names(dt_nov)[!(names(dt_nov)  %in% names(dtfulloct))]
# none


##### full survey data country smoothed ####

dtfull <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/smooth/04_april_country_full_smooth.csv.gz") %>%
  bind_rows(read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/smooth/05_may_country_full_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/smooth/06_june_country_full_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/smooth/07_july_country_full_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/smooth/08_aug_country_full_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/smooth/09_sept_country_full_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/smooth/10_oct_country_full_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/country/smooth/11_nov_country_full_smooth.csv.gz"))
  

#extract for Spain
esp_country_full_smooth <- dtfull %>% filter(GID_0 == "ESP")
write.csv(esp_country_full_smooth, "./UMD_updated/Full Survey Data/country/smoothed/esp_country_full_smoothed.csv")
bra_country_full_smooth <- dtfull %>% filter(GID_0 == "BRA")
write.csv(bra_country_full_smooth, "./UMD_updated/Full Survey Data/country/smoothed/bra_country_full_smoothed.csv")


##################### Full survey data region ##############

dtfull <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/04_april_reg_full.csv.gz") %>%
  bind_rows(read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/05_may_reg_full.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/06_june_reg_full.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/07_july_reg_full.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/08_aug_reg_full.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/09_sept_reg_full.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/10_oct_reg_full.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/11_nov_reg_full.csv.gz"))



#extract for Spain
esp_region_full <- dtfull %>% filter(GID_0 == "ESP")
write.csv(esp_region_full, "./UMD_updated/Full Survey Data/region/esp_region_full.csv")
bra_region_full <- dtfull %>% filter(GID_0 == "BRA")
write.csv(bra_region_full, "./UMD_updated/Full Survey Data/region/bra_region_full.csv")



##################### Full survey data region smoothed ##############
dtfull <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/smooth/05_may_reg_full_smooth.csv.gz") %>%
  bind_rows(read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/smooth/06_june_reg_full_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/smooth/07_july_reg_full_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/smooth/08_aug_reg_full_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/smooth/09_sept_reg_full_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/smooth/10_oct_reg_full_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/smooth/11_nov_reg_full_smooth.csv.gz"))

#extract for Spain
esp_region_full_smoothed <- dtfull %>% filter(GID_0 == "ESP")
write.csv(esp_region_full_smoothed, "./UMD_updated/Full Survey Data/region/smoothed/esp_region_full_smoothed.csv")
bra_region_full_smoothed <- dtfull %>% filter(GID_0 == "BRA")
write.csv(bra_region_full_smoothed, "./UMD_updated/Full Survey Data/region/smoothed/bra_region_full_smoothed.csv")



################## Part A Survey country################

dtfull <- read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/04_april_country_parta.csv.gz") %>%
  bind_rows(read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/05_may_country_parta.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/06_june_country_parta.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/07_july_country_parta.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/08_aug_country_parta.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/09_sept_country_parta.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/10_oct_country_parta.csv.gz"), 
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/10_oct_country_parta.csv.gz"))


## extract for Spain
esp_country_parta <- dtfull %>% filter(GID_0 == "ESP")
write.csv(esp_country_parta, "./UMD_updated/Part-A Survey Data/country/esp_country_parta.csv")
bra_country_parta <- dtfull %>% filter(GID_0 == "BRA")
write.csv(bra_country_parta, "./UMD_updated/Part-A Survey Data/country/bra_country_parta.csv")


################## Part A Survey country smooothed ################

dtfull <- read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/smooth/04_april_country_parta_smooth.csv.gz") %>%
  bind_rows(read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/smooth/05_may_country_parta_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/smooth/06_june_country_parta_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/smooth/07_july_country_parta_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/smooth/08_aug_country_parta_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/smooth/09_sept_country_parta_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/smooth/10_oct_country_parta_smooth.csv.gz"), 
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/country/smooth/11_nov_country_parta_smooth.csv.gz"))


#extract for Spain
esp_country_parta_smooth <- dtfull %>% filter(GID_0 == "ESP")
write.csv(esp_country_parta_smooth, "./UMD_updated/Part-A Survey Data/country/smoothed/esp_country_parta_smoothed.csv")
bra_country_parta_smooth <- dtfull %>% filter(GID_0 == "BRA")
write.csv(bra_country_parta_smooth, "./UMD_updated/Part-A Survey Data/country/smoothed/bra_country_parta_smoothed.csv")



##################### Part A survey data region ##############

dtfull <- read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/04_april_reg_parta.csv.gz") %>%
  bind_rows(read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/05_may_reg_parta.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/06_june_reg_parta.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/07_july_reg_parta.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/08_aug_reg_parta.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/09_sept_reg_parta.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/10_oct_reg_parta.csv.gz"), 
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/11_nov_reg_parta.csv.gz"))


#extract for Spain
esp_region_parta <- dtfull %>% filter(GID_0 == "ESP")
write.csv(esp_region_parta, "./UMD_updated/Part-A Survey Data/region/esp_region_parta.csv")
bra_region_parta <- dtfull %>% filter(GID_0 == "BRA")
write.csv(bra_region_parta, "./UMD_updated/Part-A Survey Data/region/bra_region_parta.csv")


##################### Part A survey data region smoothed ##############

dtfull <- bind_rows(read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/smooth/05_may_reg_parta_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/smooth/06_june_reg_parta_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/smooth/07_july_reg_parta_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/smooth/08_aug_reg_parta_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/smooth/09_sept_reg_parta_smooth.csv.gz"),
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/smooth/10_oct_reg_parta_smooth.csv.gz"), 
            read_gz("https://covidmap.umd.edu/umdcsvs/PartA_Survey_Data/region/smooth/11_nov_reg_parta_smooth.csv.gz"))


#extract for Spain

esp_region_parta_smoothed <- dtfull %>% filter(GID_0 == "ESP")
write.csv(esp_region_parta_smoothed, "./UMD_updated/Part-A Survey Data/region/smoothed/esp_region_parta_smoothed.csv")
bra_region_parta_smoothed <- dtfull %>% filter(GID_0 == "BRA")
write.csv(bra_region_parta_smoothed, "./UMD_updated/Part-A Survey Data/region/smoothed/bra_region_parta_smoothed.csv")


