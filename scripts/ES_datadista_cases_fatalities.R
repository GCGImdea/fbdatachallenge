## Libraries
library(dplyr)
library(ggplot2)
library(ggpubr)

## Built-in functions ----
smooth_positive_column <- function(df_in, col_s, basis_dim = 15){
  require(mgcv)
  
  # add a number of "day" column:
  to.smooth <- df_in
  to.smooth$day <- 1:nrow(to.smooth)
  
  # change the name of column to be smoothed:
  colnames(to.smooth)[colnames(to.smooth) == col_s] = "y"
  
  # Smoothing with scam ----
  b1 <- gam(y ~ s(day, k = basis_dim, bs="ps"), 
            family=gaussian(link = "log"), 
            data=to.smooth)
  
  # save to column "xxx_smooth":
  df_in$y_smooth <- NA
  df_in$y_smooth <- b1$fitted.values
  colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, 
                                                           "_smooth")
  return(df_in)
}

## Load data from datatista's github ----
df_cases <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_datos_isciii_nueva_serie.csv",
                     encoding = "UTF-8")
df_fatalities <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos_por_fecha_defuncion_nueva_serie_long.csv", 
                          encoding = "UTF-8")

df_cases <- df_cases %>% select(fecha, ccaa, num_casos)
df_fatalities <- df_fatalities %>% select(fecha, CCAA, total)

colnames(df_cases) <- c("date", "ccaa", "cases")
colnames(df_fatalities) <- c("date", "ccaa", "deads")

df_cases$date <- as.Date(df_cases$date)
df_fatalities$date <- as.Date(df_fatalities$date)

summary(df_cases$ccaa)
summary(df_fatalities$ccaa)

## Smoothing the number of cases and fatalities ----
df_out <- data.frame()
for (region_code in unique(df_cases$ccaa)) {
  
  df_cases_in <- df_cases %>% filter(ccaa == region_code)
  df_fatal_in <- df_fatalities %>% filter(ccaa == region_code)
  
  if (min(nrow(df_cases_in), nrow(df_fatal_in)) > 15) {
    
    df_cases_in <- smooth_positive_column(df_cases_in, "cases", 
                                          basis_dim = min(nrow(df_cases_in), 40))
    
    df_fatal_in <- smooth_positive_column(df_fatal_in, "deads", 
                                         basis_dim = min(nrow(df_fatal_in), 40))
    
    
    df_out <-rbind(df_out, full_join(df_cases_in, df_fatal_in, by = "date"))
   
  }
}

df_out <- df_out %>% select(date, ccaa.x, 
                            cases, cases_smooth, 
                            deads, deads_smooth)

colnames(df_out)[2] <- "ccaa"

df_out[is.na(df_out)]  <- 0

df_out <- df_out %>% 
  group_by(ccaa) %>% 
  mutate(cum_cases = cumsum(cases), 
         cum_deads = cumsum(deads))

df_out <- df_out %>% 
  group_by(ccaa) %>% 
  mutate(cum_cases_smooth = cumsum(cases_smooth), 
         cum_deads_smooth = cumsum(deads_smooth))

## Add 'region' code
df_out$region <- NA

df_out$region[df_out$ccaa == "Andalucía"] <- "ESAN"
df_out$region[df_out$ccaa == "Aragón"] <- "ESAR"
df_out$region[df_out$ccaa == "Asturias"] <- "ESAS"
df_out$region[df_out$ccaa == "Cantabria"] <- "ESCB"
df_out$region[df_out$ccaa == "Ceuta"] <- "ESMC"
df_out$region[df_out$ccaa == "Castilla y León"] <- "ESCL"
df_out$region[df_out$ccaa == "Castilla La Mancha"] <- "ESCM"
df_out$region[df_out$ccaa == "Canarias"] <- "ESCN"
df_out$region[df_out$ccaa == "Cataluña"] <- "ESCT"
df_out$region[df_out$ccaa == "Extremadura"] <- "ESEX"
df_out$region[df_out$ccaa == "Galicia"] <- "ESGA"
df_out$region[df_out$ccaa == "Baleares"] <- "ESIB"
df_out$region[df_out$ccaa == "Murcia"] <- "ESMC"
df_out$region[df_out$ccaa == "Madrid"] <- "ESMD"
df_out$region[df_out$ccaa == "Melilla"] <- "ESMC"
df_out$region[df_out$ccaa == "Navarra"] <- "ESNC"
df_out$region[df_out$ccaa == "País Vasco"] <- "ESPV"
df_out$region[df_out$ccaa == "La Rioja"] <- "ESRI"
df_out$region[df_out$ccaa == "C. Valenciana"] <- "ESVC"

## rearrange the columns:
df_out <- df_out %>% group_by(region) %>% 
  select(date, region, cases, cases_smooth, 
                            cum_cases, cum_cases_smooth,
                            deads, deads_smooth,
                            cum_deads, cum_deads_smooth)

write.csv(df_out,
          "../data/datadista_regional/smooth_cases_fatalities.csv", 
          row.names = FALSE)

## Some plots ----
some_plots = F

if (some_plots) {
  ## Raw data
  p1 <- ggplot(data = df_cases, aes(x = date, y = cases, color = region)) +
    facet_wrap(~ region, scales = "free") +
    geom_line() + 
    theme_bw()
  p1
  
  p2 <- ggplot(data = df_fatalities, aes(x = date, y = deads, color = region)) +
    facet_wrap(~ region, scales = "free") +
    geom_line() + 
    theme_bw()
  p2
  
  ## Raw vs. smooth data
  p3 <- ggplot(data = df_out, aes(x = date, color = region)) +
    facet_wrap(~ region, scales = "free") +
    geom_point(aes(y = cases), size = 0.5) +
    geom_line(aes(y = cases_smooth), size = 1) + 
    theme_bw() + ggtitle("Number of cases per day")
  p3
  
  p4 <- ggplot(data = df_out, aes(x = date, color = region)) +
    facet_wrap(~ region, scales = "free") +
    geom_point(aes(y = deads), size = 0.5) +
    geom_line(aes(y = deads_smooth), size = 1) + 
    theme_bw() + ggtitle("Fatalities per day")
  p4
  
  ## Plot of specific regions
  ## check possible values with: unique(df_out$region)
  region_code = "ESMD"
  # region_code = "ESAN" 
  
  df_region <- df_out %>% filter(region == region_code)
  
  p5 <-  ggplot(data = df_region, aes(x = date)) +
    geom_point(aes(y = cases), size = 0.5, colour = "blue") +
    geom_line(aes(y = cases_smooth), size = 1, colour = "blue") + 
    theme_bw() + ggtitle(paste0(region_code, ": cases per day"))
  
  p6 <- ggplot(data = df_region, aes(x = date)) +
    geom_point(aes(y = deads), size = 0.5, colour = "red") +
    geom_line(aes(y = deads_smooth), size = 1, colour = "red") + 
    theme_bw() + ggtitle(paste0(region_code, ": fatalities per day"))
  
  p56 <- ggarrange(p5, p6, ncol = 2, nrow = 1)
  p56
  
  p7 <-  ggplot(data = df_region, aes(x = date)) +
    geom_point(aes(y = cases), size = 0.5, colour = "blue") +
    geom_line(aes(y = cases_smooth), size = 1, colour = "blue") + 
    geom_point(aes(y = deads), size = 0.5, colour = "red") +
    geom_line(aes(y = deads_smooth), size = 1, colour = "red") + 
    theme_bw() + ggtitle(paste0(region_code, ": cases/fatalities per day"))
  p7
  
  ## cumulative (smoothed and unsmoothed)
  p8 <-  ggplot(data = df_region, aes(x = date)) +
    geom_point(aes(y = cum_cases), size = 1, colour = "blue") +
    geom_line(aes(y = cum_cases_smooth), size = 1, colour = "blue", alpha = 0.7) +
    geom_point(aes(y = cum_deads), size = 1, colour = "red") +
    geom_line(aes(y = cum_deads_smooth), size = 1, colour = "red", alpha = 0.7) +
    theme_bw() + ggtitle(paste0(region_code, ": cumulative cases/fatalities"))
  p8
  
  p9 <-  ggplot(data = df_region, aes(x = date)) +
    geom_point(aes(y = cum_cases), size = 1.5, colour = "blue", alpha = 0.2) +
    geom_line(aes(y = cum_cases_smooth), size = 1, colour = "blue") + 
    theme_bw() + ggtitle(paste0(region_code, ": cumulative cases"))
  
  p10 <- ggplot(data = df_region, aes(x = date)) +
    geom_point(aes(y = cum_deads), size = 1.5, colour = "red", alpha = 0.2) +
    geom_line(aes(y = cum_deads_smooth), size = 1, colour = "red") + 
    theme_bw() + ggtitle(paste0(region_code, ": cumulative fatalities"))
  
  p9_10 <- ggarrange(p9, p10, ncol = 2, nrow = 1)
  p9_10
}