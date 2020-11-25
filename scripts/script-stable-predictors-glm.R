library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)

## Study the stable signals:

files_model_path <- "../data/estimates-symptom-lags/PlotData/"
files_model <- dir(files_model_path)

coeffs_by_country <- list()
stable_regressors <- c()

for (country_model in files_model) {
  
  temp_coeffs <- read.csv(paste0(files_model_path, country_model)) %>% 
    dplyr::select(starts_with("coef")) %>%
    distinct()
  
  colnames(temp_coeffs) <- str_replace(colnames(temp_coeffs), "coef_", "")
  stable_regressors <- c(stable_regressors, colnames(temp_coeffs))
  
  country_code <- str_sub(country_model, 1, 2)
  
  coeffs_by_country[[country_code]] <- temp_coeffs
  
}

## Dealing with stability ----

p_stable <- ggplot(data.frame(Signal = stable_regressors), aes(forcats::fct_infreq(Signal))) +
  geom_bar(alpha = 0.8 ) +
  theme_light(base_size = 15) +
  xlab("") + ylab("Times selected") +
  labs(title = "Stability") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
p_stable

ggsave(plot = p_stable, 
       filename = paste0(
         "../data/estimates-symptom-lags/stable-regressors.png"
       ), width = 12, height = 10
)


## Dealing with coefficients ----

df_coeffs <- matrix(0, nrow = length(coeffs_by_country), ncol = length(unique(stable_regressors)))
colnames(df_coeffs) <- unique(stable_regressors)
rownames(df_coeffs) <- names(coeffs_by_country)

for (country_code in names(coeffs_by_country)) {
  temp_coeffs <- coeffs_by_country[[country_code]]
  df_coeffs[country_code, colnames(temp_coeffs)] = as.numeric(temp_coeffs)
}


## Standardize:

df_coeffs_std <- as.data.frame(apply(df_coeffs, 2, function(x) (x-min(x))/(max(x)-min(x))))

df_coeffs_std <- pivot_longer(df_coeffs_std, colnames(df_coeffs_std), names_to = "Signal", values_to = "Coefficient")

## Ascending by mean
p_coeffs_by_median <- ggplot(df_coeffs_std, aes(x = reorder(Signal, Coefficient, median, ), y = Coefficient)) +
  geom_boxplot(outlier.alpha = 0.1 ) +
  theme_light(base_size = 15) +
  xlab("") + ylab("Coefficient values (standardized)") +
  labs(title = "Increasing order by median") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
p_coeffs_by_median

ggsave(plot = p_coeffs_by_median, 
       filename = paste0(
         "../data/estimates-symptom-lags/coeffs-boxplot-by-median.png"
       ), width = 12, height = 10
)

# ## By number of observations per group
# p_coeffs_by_stable <- df_coeffs_std %>%
#   mutate(Signal = fct_reorder(Signal, Coefficient, .fun='length' )) %>%
#   ggplot( aes(x = Signal, y = Coefficient)) +
#   geom_boxplot(outlier.alpha = 0.1 ) +
#   theme_light(base_size = 15) +
#   xlab("") + ylab("Coefficient values (standardized)") +
#   labs(title = "Decreasing order by stability") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
# p_coeffs_by_stable
# 
# ggsave(plot = p_coeffs_by_stable, 
#        filename = paste0(
#          "../data/estimates-symptom-lags/coeffs-boxplot-by-stability.png"
#        ), width = 12, height = 10
# )
