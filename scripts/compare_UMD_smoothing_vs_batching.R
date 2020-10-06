library(ggplot2)
library(dplyr)

## Region to study:
region_iso = "ESMD"

## Load data UMD's smoothed data
data_s <- read.csv("../data/UMD/Full Survey Data/region/smoothed/esp_region_full_smoothed.csv",
                 fileEncoding = "UTF-8")

## Filter overall >> overall
# unsmoothed
dt <- data_s %>% 
  filter(gender=="overall", age_bucket == "overall") %>% 
  select(country_agg, region_agg, date, smoothed_pct_cli )

dt$date <- as.Date(dt$date)

## Consider data starting on 2020-05-08
dt <- dt %>% filter(date > "2020-05-07")

## Add 'region' code
dt$region <- NA
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

dt <- dt %>% filter(region == region_iso) %>% select(date, smoothed_pct_cli)

## Get our preprocessed data:

df <- read.csv("../data/estimates-umd-batches/ES/ESMD_UMD_data.csv")
df$date <- as.Date(df$date)

df2plot <- full_join(df, dt, by = "date")

d = df2plot$b_size_denom[1]

p1 <- ggplot(data = df2plot, aes(x = date)) +
  geom_point(aes(y = pct_cli, color = "CSDC CLI (UMD raw)"), alpha = 0.3) +
  geom_line(aes(y = smoothed_pct_cli, color = "CSDC CLI (UMD smooth)"), linetype = "dashed", alpha = 0.6, size = 0.7) +
  geom_point(aes(y = batched_pct_cli, color = "Batched CSDC CLI"), alpha = 0.5) +
  geom_line(aes(y = batched_pct_cli_smooth, color = "Batched CSDC CLI (smooth)"), linetype = "solid", alpha = 0.6, size = 0.7)  +
  theme_bw() + 
  labs(title = "Madrid", subtitle = paste0("batch size = population / ", d),
       x = "Date", y = "% symptomatic cases") +
  scale_colour_manual(name = "", values = c("blue", "blue", "red", "red"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("blank", "solid", "blank", "dashed"),
                        shape = c(1, NA, 1, NA)))) +
  theme(legend.position = "bottom")
p1

ggsave(plot = p1, 
       filename = paste0("../data/all-estimates/PlotData/", region_iso, "-batch-vs-mov-ave.png"), 
       width = 9, height = 6)

write.csv(df2plot , file = paste0("../data/all-estimates/PlotData/", region_iso, "-batch-vs-mov-ave.csv"))
