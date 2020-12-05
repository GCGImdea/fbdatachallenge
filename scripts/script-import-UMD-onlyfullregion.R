
##### full survey data region ####

library(dplyr)
read_gz <- function(broken_link){
  funny_gunzip <- gzcon(url(broken_link))  
  funny_gunzip_connection <- readLines(funny_gunzip)
  dt <- read.csv(textConnection(funny_gunzip_connection), as.is = T)
  close(funny_gunzip)
  return(dt)
}


dt_apr <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/04_april_reg_full.csv.gz")

dt_may <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/05_may_reg_full.csv.gz")

dt_jun <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/06_june_reg_full.csv.gz")

dt_jul <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/07_july_reg_full.csv.gz")

dt_aug <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/08_aug_reg_full.csv.gz")

dt_sep <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/09_sept_reg_full.csv.gz")

dt_oct <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/10_oct_reg_full.csv.gz")

dt_nov <- read_gz("https://covidmap.umd.edu/umdcsvs/Full_Survey_Data/region/11_nov_reg_full.csv.gz")


# bind rows
dtfull <- dt_apr %>% bind_rows(dt_may, dt_jun, dt_jul, dt_aug, dt_sep, dt_oct, dt_nov)

#print (dtfull)
countries  <- unique(dtfull$GID_0)
for (country in countries){
  country_full <- dtfull %>% filter(GID_0 == country)
  lowercasecountry=tolower(country)
  write.csv(country_full, paste0("../data/UMD_updated/Full_Survey_Data/region/",lowercasecountry,"_region_full.csv"))
}

