library(dplyr)
dt <- read.csv("/home/statimatician/Downloads/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv", as.is = T) %>% 
  filter(state == "MA") %>% 
  select(submission_date, state, new_case) %>% 
  rename(date = submission_date, 
         new_case_cdc = new_case)

dt$date <- as.Date(dt$date, format = "%m/%d/%Y")

#active cases
ac_window <- 18



dt$cases_active_cdc <- cumsum(c(dt$new_case_cdc[1:ac_window],
                                diff(dt$new_case_cdc, lag = ac_window)))
dt$state <- paste0("US", dt$state)
uscodes <- (read.csv("../data/common_data/regions-tree-population.csv") %>% 
              filter(countrycode == "US"))[, c(2,5)] 
dt <- left_join(dt, uscodes, by = c("state" = "regioncode"))

write.csv(dt, "../data/US_cdc_active_cases/us_cdc_active_cases_ma.csv")
