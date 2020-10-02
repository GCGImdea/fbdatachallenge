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
# data <- read.csv("../data/UMD/Full Survey Data/region/esp_region_full.csv", 
#                  fileEncoding = "UTF-8")

data_s <- read.csv("../data/UMD/Full Survey Data/region/smoothed/esp_region_full_smoothed.csv",
                 fileEncoding = "UTF-8")

## Filter overall >> overall
# # unsmoothed
# dt <- data %>% 
#   filter(gender=="overall", age_bucket == "overall") %>% 
#   select(country_agg, region_agg, date, total_responses, 
#          country_region_numeric, pct_cli )

## Number of infected in each region
dt$number_cli <- dt$total_responses*dt$pct_cli/100

# smoothed
dt_s <- data_s %>%
  filter(gender=="overall", age_bucket == "overall") %>%
  select(country_agg, region_agg, date, rolling_total_responses,
         country_region_numeric, smoothed_pct_cli )

## Use smoothed data
dt <- dt_s
colnames(dt)[colnames(dt)== "smoothed_pct_cli"] <- "pct_cli"

dt$date <- as.Date(dt$date)

## Consider data starting on 2020-05-08
dt <- dt %>% filter(date > "2020-05-07")

## Get region population
country_geoid = "ES"
unique(dt$region_agg)
dt$region <- NA

summary(dt$region_agg)

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

## Get region population
regions_tree <- read.csv(file = "../data/common_data/regions-tree-population.csv", as.is = T) %>%
  filter(countrycode == country_geoid) %>% 
  group_by(regioncode) %>% 
  summarise(population = sum(population))

# add "Ceuta y Melilla"
regions_tree <- rbind(regions_tree, data.frame(regioncode = "ESCE_ML", population = 171264) )

## Add region population
dt$population <- NA 
for (i in 1:nrow(regions_tree)) {
  dt$population[ dt$region == regions_tree$regioncode[i] ] <- regions_tree$population[i]
}

## Remove La Rioja (just 1 obs.)
# dt <- dt %>% filter(region != "ESRI")

df_out <- data.frame()
## Batched-estimated-cases
for (region_code in unique(dt$region)){
  
  df_temp <- dt %>% filter(region == region_code)
  # df_temp$batched_pct_cli <- NA
  
  # b_size <- as.numeric(regions_tree[regions_tree$regioncode == region_code, "population"]/1000)
  # 
  # df_temp$cum_responses <- cumsum(df_temp$total_responses)
  # df_temp$cum_number_cli <- cumsum(df_temp$number_cli)
  
  df_temp <- smooth_column(df_temp, "pct_cli", 
                           basis_dim = min(sum(df_temp$pct_cli > 0), 25))
  
  df_out <- rbind(df_out, df_temp)
  
}

df_out <- df_out %>% select(date, region, population, rolling_total_responses,
                            pct_cli, pct_cli_smooth)

dt <- data.frame()
for (region_code in unique(df_out$region)){
  
  df_region <- df_out %>% filter(region == region_code)
  
  df_region <- df_region %>% mutate(estimate_cli = population*(pct_cli_smooth/100),
                                    cum_estimate_cli = cumsum(estimate_cli) )
  # write.csv(df_region,
  #           paste0("../data/estimates-umd-batches/spain/", region_code  , "_UMD_data.csv"), 
  #           row.names = FALSE)
  
  dt <- rbind(dt, df_region)
  
}

# write.csv(dt,
#           "../data/estimates-umd-batches/spain/ES_UMD_data.csv", 
#           row.names = FALSE)

## Some plots 
p1 <- ggplot(data = dt, aes(x = date, color = region)) +
  facet_wrap(~ region, scales = "free") +
  geom_point(aes(y = pct_cli)) +
  geom_line(aes(y = pct_cli_smooth)) + 
  theme_bw()
p1

# ## Plot of specific regions ----
# 
# for (region_code in unique(df_out$region)){
# 
# df_region <- df_out %>% filter(region == region_code)
# # b_size <- as.numeric(regions_tree[regions_tree$regioncode == region_code, "population"]/10000)
# 
# p2 <-  ggplot(data = df_region, aes(x = date)) +
#   geom_point(aes(y = batched_pct_cli), size = 1, colour = "red", alpha = 0.5) +
#   geom_line(aes(y = batched_pct_cli_smooth), size = 1, colour = "blue") + 
#   theme_bw() + ggtitle(paste0(region_code, ": Batched percentage covid-like-illness"))
# print(p2)
# 
# }
