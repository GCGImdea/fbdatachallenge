dtt <- read.csv("andalucia_data.csv", as.is = T)

## fecha and date column in data are of type character.
# We can convert them to Date type

dtt$fecha2 <- as.Date(dtt$fecha, format = "%Y/%m/%d")
dtt$date2 <- as.Date(dtt$date, format = "%Y-%m-%d")

## plot using base R

plot(dtt$fecha2, dtt$cases)

plot(dtt$fecha2, dtt$cases, type = "l", xlab = "fecha", ylab = "casos")

plot(dtt$date2, dtt$deaths, xlab = "date", ylab = "deaths")


## using ggplot2
library(ggplot2)

ggplot(data = dtt, aes(x = date2, y = deaths)) +
  geom_line() +
  theme_bw()

