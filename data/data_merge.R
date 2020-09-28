## Get data full survey data country
library(dplyr)

dt_april <- read.csv("https://covidmap.umd.edu/umdcsvs/Full%20Survey%20Data/country/04_april_country_full.csv",
                     as.is = T)
dt_may <- read.csv("https://covidmap.umd.edu/umdcsvs/Full%20Survey%20Data/country/05_may_country_full.csv",
                   as.is = T)
dt_june <- read.csv("https://covidmap.umd.edu/umdcsvs/Full%20Survey%20Data/country/06_june_country_full.csv",
                    as.is = T)
dt_july <- read.csv("https://covidmap.umd.edu/umdcsvs/Full%20Survey%20Data/country/07_july_country_full.csv",
                    as.is = T)
dt_august <- read.csv("https://covidmap.umd.edu/umdcsvs/Full%20Survey%20Data/country/08_august_country_full.csv",
                      as.is = T)
dt_sept <- read.csv("https://covidmap.umd.edu/umdcsvs/Full%20Survey%20Data/country/09_sept_country_full.csv",
                    as.is = T)

# row_bind

dtfull <-  dt_april %>% bind_rows(dt_may)   
## april to may (in april not in may)
# [1] "pct_wear_mask_all_time"              
# [2] "pct_wear_mask_all_time_weighted_sums"
# [3] "pct_wear_mask_all_time_weighted" 

## in may not in april
# [1] "pct_chills"                  "pct__all_time"              
# [3] "pct_chills_weighted_sums"    "pct__all_time_weighted_sums"
# [5] "pct_chills_weighted"         "pct__all_time_weighted"     

dtfulljune <- dtfull %>% bind_rows(dt_june) 
## may to june (in april or may but not in june)
# [1] "pct_wear_mask_all_time"              
# [2] "pct_wear_mask_all_time_weighted_sums"
# [3] "pct_wear_mask_all_time_weighted" 

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


dt_fulljuly <- dtfulljune %>% bind_rows(dt_july) #, dt_june, dt_july, dt_august, dt_sept)

## june to july (in april, may or june but not in july): names(dtfulljune)[!(names(dtfulljune)  %in% names(dt_july))] 
# [1] "pct_wear_mask_all_time"              
# [2] "pct_wear_mask_all_time_weighted_sums"
# [3] "pct_wear_mask_all_time_weighted"     

## in july but not in previous months: names(dt_july)[!(names(dt_july)  %in% names(dtfulljune))]
# character(0)

dt_fullaug <- dt_fulljuly %>% bind_rows(dt_august)
## july to aug (in april, may, june or  july but not august): names(dt_fulljuly)[!(names(dt_fulljuly)  %in% names(dt_august))]
# [1] "pct_wear_mask_all_time"              
# [2] "pct_wear_mask_all_time_weighted_sums"
# [3] "pct_wear_mask_all_time_weighted"     

## in aug but not in previous months: names(dt_august)[!(names(dt_august)  %in% names(dt_fulljuly))]
# character(0)

dt_final <- dt_fullaug  %>% bind_rows(dt_sept)
## aug to sept (in april, may, june, july or august not in sept): names(dt_fulljuly)[!(names(dt_fulljuly)  %in% names(dt_august))]
# [1] "pct_wear_mask_all_time"              
# [2] "pct_wear_mask_all_time_weighted_sums"
# [3] "pct_wear_mask_all_time_weighted"    

## in sept but not in previous months: nam
character(0)



## extract for Spain
esp_country_full <- dt_final %>% filter(GID_0 == "ESP")
write.csv(esp_country_full, "./UMD/Full Survey Data/country/esp_country_full_new.csv")
bra_country_full <- dt_final %>% filter(GID_0 == "BRA")
write.csv(bra_country_full, "./UMD/Full Survey Data/country/bra_country_full_new")
