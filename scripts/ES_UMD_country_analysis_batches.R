## Libraries
library(dplyr)
library(ggplot2)

## Smoothing the estimated active cases

smooth_column <- function(df_in, col_s, basis_dim = 15){
  require(mgcv)
  
  # add a number of "day" column:
  to.smooth <- df_in
  to.smooth$day <- 1:nrow(to.smooth)
  
  # change the name of column to be smoothed:
  colnames(to.smooth)[colnames(to.smooth) == col_s] = "y"
  
  # not NA elements to be smoothed:
  ind_not_na <- !is.na(to.smooth$y)
  
  # data to be smoothed:
  to.smooth <- to.smooth[ind_not_na, c("y", "day")]
  
  # Mono-smoothing with scam ----
  b1 <- gam(y ~ s(day, k = basis_dim, bs="ps"), data=to.smooth)
  
  # save to column "xxx_smooth":
  df_in$y_smooth <- NA
  # df_in[frst_n_zero:nrow(df_in) , "y_smooth"] <- b1$fitted.values
  newd <- data.frame(day = 1:nrow(df_in))
  df_in[ , "y_smooth"] <- predict(b1, newd)
  colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, 
                                                           "_smooth")
  return(df_in)
}

## Load data
data <- read.csv("../data/UMD/Full Survey Data/country/esp_country_full_new.csv", 
                 fileEncoding = "UTF-8")

# data_s <- read.csv("../data/UMD/Full Survey Data/region/smoothed/esp_region_full_smoothed.csv", 
#                  fileEncoding = "UTF-8")

## Filter overall >> overall
# unsmoothed
dt <- data %>% 
  filter(gender=="overall", age_bucket == "overall") %>% 
  select(date, total_responses, pct_cli )

## Number of infected in each region
dt$number_cli <- dt$total_responses*dt$pct_cli/100

# # smoothed
# dt_s <- data_s %>% 
#   filter(gender=="overall", age_bucket == "overall") %>% 
#   select(country_agg, region_agg, date, rolling_total_responses, 
#          country_region_numeric, smoothed_pct_cli )
# 
# ## Use smoothed data
# dt <- dt_s
# colnames(dt)[colnames(dt)== "smoothed_pct_cli"] <- "pct_cli"

dt$date <- as.Date(dt$date)

## Consider data starting on 2020-05-08
# dt <- dt %>% filter(date > "2020-05-07")


## Get region population
regions_tree <- read.csv(file = "../data/common_data/regions-tree-population.csv", as.is = T) %>%
  filter(countrycode == "ES") %>% 
  group_by(regioncode) %>% 
  summarise(population = sum(population))

dt$population <- sum(regions_tree$population)

## Batched-estimated-cases

b_size <- dt[1, "population"]/1000

dt$batched_pct_cli <- NA

dt$cum_responses <- cumsum(dt$total_responses)
dt$cum_number_cli <- cumsum(dt$number_cli)

for (i in 1:(nrow(dt)-1)) {
  if (dt[i, "cum_responses"] >= b_size) {
    dt[i, "batched_pct_cli"] <- dt[i, "cum_number_cli"]/dt[i, "cum_responses"]*100
    
    dt[(i+1):nrow(dt), "cum_responses"] <- dt[(i+1):nrow(dt), "cum_responses"] - dt[i, "cum_responses"]
    dt[(i+1):nrow(dt), "cum_number_cli"] <- dt[(i+1):nrow(dt), "cum_number_cli"] - dt[i, "cum_number_cli"]
  } # if-cum_responses-greater-b_size
} # for-rows-dt

dt <- smooth_column(dt, "batched_pct_cli", 
                         basis_dim = min(sum(!is.na(dt$batched_pct_cli)), 25))

dt <- smooth_column(dt, "pct_cli", 
              basis_dim = min(sum(!is.na(dt$batched_pct_cli)), 40))

dt <- dt %>% 
  select(date, population, total_responses, pct_cli, pct_cli_smooth,
         number_cli, batched_pct_cli, batched_pct_cli_smooth) %>% 
  mutate(estimate_cli = population*(batched_pct_cli_smooth/100),
         cum_estimate_cli = cumsum(estimate_cli),
         number_cli_smooth = population*(pct_cli_smooth/100),
         cum_number_cli_smooth = cumsum(number_cli_smooth))

write.csv(dt,
          "../data/estimates-umd-batches/spain/ES_UMD_country_data.csv", 
          row.names = FALSE)

## Some plots 
p1 <- ggplot(data = dt, aes(x = date)) +
  geom_point(aes(y = batched_pct_cli)) +
  geom_line(aes(y = batched_pct_cli_smooth)) + 
  theme_bw()
p1

p2 <- ggplot(data = dt, aes(x = date)) +
  geom_point(aes(y = pct_cli)) +
  geom_line(aes(y = pct_cli_smooth)) + 
  theme_bw()
p2

# I think this is producing a TIME SHIFT!!!
cols <- c("pct_cli_smooth" = "red", "batched_pct_cli_smooth" = "blue")
p3 <- ggplot(data = dt, aes(x = date)) +
  geom_line(aes(y = pct_cli_smooth, colour = "pct_cli_smooth")) + 
  geom_line(aes(y = batched_pct_cli_smooth, colour = "batched_pct_cli_smooth")) +
  theme_bw() + scale_colour_manual(name = "Legend", values = cols)
p3

