## Daily estimates per country country:
library(dplyr)
spain_population <- 46754778 # worldometer

dtspain <- read.csv("../data/UMD/Full_Survey_Data/country/esp_country_full_new.csv") %>% 
  filter(gender == "overall", age_bucket == "overall")

# daily estimate using unweighted data
spain_daily_cli_estimate <- (dtspain$pct_cli/100) * spain_population
spain_daily_ili_estimate <- (dtspain$pct_ili/100) * spain_population

# estimate daily new cases

# cumulative cases

spain_daily_cli_estimate_cumulative <- cumsum(spain_daily_cli_estimate)
spain_daily_ili_estimate_cumulative <- cumsum(spain_daily_ili_estimate)

# how to compute the falities from and total infected from IFR and 





## use weighted data or not
## 
