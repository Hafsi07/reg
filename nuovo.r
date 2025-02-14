library(ggplot2)
library(lubridate)
library(tseries)
library(forecast)


df <- read.csv("C:\\Users\\hafsi\\Downloads\\Electric_Production.csv", stringsAsFactors = FALSE )

summary(df)
dim(df)
str(df)

df$DATE <- mdy(df$DATE)

interval <- as.numeric(median(diff(df$DATE)))

frequency <- ifelse(interval >= 360, 1,  # Year
             ifelse(interval >= 90, 4,  # Quarter
             ifelse(interval >= 28, 12, # Month
             ifelse(interval >= 7, 52,  # Week
             ifelse(interval >= 1, 365, # Day
             1)))))

print(paste("Detected frequency:", frequency))

ts<- ts(df$IPG2211A2N,start = c(year(min(df$DATE)), month(min(df$DATE))), frequency = 12)

plot(ts)

autoplot(ts_data) +
  ggtitle("Electric Production Over Time") +
  xlab("Year") + ylab("Production") +
  theme_minimal()

adf_test <- adf.test(ts_data)
print(adf_test)

best_arima <- auto.arima(ts)
print(best_arima)

forecast_values <- forecast(best_arima, h=12)

autoplot(forecast_values) +
  ggtitle("Electric Production Forecast") +
  xlab("Year") + ylab("Production") +
  theme_minimal()
