
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

#print (dtfull)
countries  <- unique(dtfull$GID_0)
for (country in countries){
  country_full <- dtfull %>% filter(GID_0 == country)
  lowercasecountry=tolower(country)
  write.csv(country_full, paste0("./UMD_updated/Full Survey Data/country/",lowercasecountry,"_country_full.csv"))
}

## extract for Spain
# esp_country_full <- dtfull %>% filter(GID_0 == "ESP")
# write.csv(esp_country_full, "./UMD_updated/Full Survey Data/country/esp_country_full.csv")
#bra_country_full <- dtfull %>% filter(GID_0 == "BRA")
#write.csv(bra_country_full, "./UMD_updated/Full Survey Data/country/bra_country_full.csv")
#prt_country_full <- dtfull %>% filter(GID_0 == "PRT")
#write.csv(prt_country_full, "./UMD_updated/Full Survey Data/country/prt_country_full.csv")
