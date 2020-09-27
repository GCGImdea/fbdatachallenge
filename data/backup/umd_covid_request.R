library(dplyr)
library(httr)
library(jsonlite)

# get available dates
path_dates <- "https://covidmap.umd.edu/api/datesavail?country=Spain"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
datedata <- fromJSON(response, flatten = TRUE) %>% data.frame()
write.csv(datedata, "spain_available_dates.csv")

# download country data covid and smooth
path <- "https://covidmap.umd.edu/api/resources?indicator=covid&type=smoothed&country=Spain&daterange=20200423-20200924"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
spain_data_covid_smooth <- fromJSON(response, flatten = TRUE) %>% data.frame()
write.csv(spain_data_covid_smooth, "./data/spain_data_covid_smooth.csv")

# download country data covid and daily
path <- "https://covidmap.umd.edu/api/resources?indicator=covid&type=daily&country=Spain&daterange=20200423-20200924"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
spain_data_covid_daily <- fromJSON(response, flatten = TRUE) %>% data.frame()
write.csv(spain_data_covid_daily, "./data/spain_data_covid_daily.csv")


# download country data flu and smooth
path <- "https://covidmap.umd.edu/api/resources?indicator=flu&type=smoothed&country=Spain&daterange=20200423-20200924"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
spain_data_flu_smooth <- fromJSON(response, flatten = TRUE) %>% data.frame()
write.csv(spain_data_flu_smooth, "./data/spain_data_flu_smooth.csv")


# download country data flu and daily
path <- "https://covidmap.umd.edu/api/resources?indicator=flu&type=daily&country=Spain&daterange=20200423-20200924"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
spain_data_flu_daily <- fromJSON(response, flatten = TRUE) %>% data.frame()
write.csv(spain_data_flu_daily, "./data/spain_data_flu_daily.csv")

###### Regional data #############
## smoothed, covid, regional
path <- "https://covidmap.umd.edu/api/resources?indicator=covid&type=smoothed&country=Spain&region=all&daterange=20200423-20200924"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
spain_regional_data_covid_smooth <- fromJSON(response, flatten = TRUE) %>% data.frame()
write.csv(spain_regional_data_covid_smooth, "./data/spain_regional_data_covid_smooth.csv")

## daily, covid, regional
path <- "https://covidmap.umd.edu/api/resources?indicator=covid&type=daily&country=Spain&region=all&daterange=20200423-20200924"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
spain_regional_data_covid_daily <- fromJSON(response, flatten = TRUE) %>% data.frame()
write.csv(spain_regional_data_covid_daily, "./data/spain_regional_data_covid_daily.csv")

## smoothed, flu, regional
path <- "https://covidmap.umd.edu/api/resources?indicator=flu&type=smoothed&country=Spain&region=all&daterange=20200423-20200924"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
spain_regional_data_flu_smooth <- fromJSON(response, flatten = TRUE) %>% data.frame()
write.csv(spain_regional_data_flu_smooth, "./data/spain_regional_data_flu_smooth.csv")

## daily, flu, regional
path <- "https://covidmap.umd.edu/api/resources?indicator=flu&type=daily&country=Spain&region=all&daterange=20200423-20200924"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
spain_regional_data_flu_daily <- fromJSON(response, flatten = TRUE) %>% data.frame()
write.csv(spain_regional_data_flu_daily, "./data/spain_regional_data_flu_daily.csv")



