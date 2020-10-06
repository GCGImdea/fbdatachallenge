library(dplyr)
# for USMA
uscodes <- (read.csv("../data/common_data/regions-tree-population.csv") %>% 
  filter(countrycode == "US"))[, c(2,5,6)] 

fls <- list.files("../data/csse_covid_19_daily_reports_us", full.names = T)
ls_fls <- lapply(fls, function(x){
  # remove diamond and grand princess
  dt <- read.csv(x, as.is = T)
  dt <- left_join(dt, uscodes, by = c("Province_State" = "regionname"))
  dt <- dt[, c(1, 19, 20, 2:18)]
  
  # indd <- which(dt$Province_State %in% c("Diamond Princess", "Grand Princess", "Guam",
  #                                        "Puerto Rico", "American Samoa", "Northern Mariana Islands", 
  #                                        "Virgin Islands"))
  #dt <- dt[-indd, ]
  dt <- dt[!is.na(dt$regioncode),]
  totalcases = dt$Confirmed[dt$regioncode == "USMA"]
  namesplit <- unlist(strsplit(x = x, split = "/"))
  date <- substring(namesplit[length(namesplit)], 1, 10)
  return(data.frame(date = date, 
                    total_cases = totalcases,
                    population = dt$population[dt$regioncode == "USMA"]))
})

df <- bind_rows(ls_fls)
df$cases_jhu <- c(df$total_cases[1], diff(df$total_cases))
df <- df[-1, ]


#total active cases
ac_window <- 18
df$cases_active_jhu <- cumsum(c(df$cases_jhu[1:ac_window],
                            diff(df$cases_jhu, lag = ac_window)))
df$pct_cases_active_jhu <- df$cases_active_jhu/df$population*100

write.csv(df, "../data/US_jhu_active_cases/us_jhu_active_cases_ma.csv")
