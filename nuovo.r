library(ggplot2)
library(lubridate)
library(tseries)
library(forecast)
library(tidyverse)
library(zoo)
library(randomForest)

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

ts <- diff(ts, differences = 1)  # First differencing
plot(ts, main = "First Order Differenced Time Series")


adf_test <- adf.test(ts)
print(adf_test)

best_arima <- auto.arima(ts)
print(best_arima)

forecast_values <- forecast(best_arima, h=12)

plot(forecast_values)


######
data<- df%>% arrange(DATE)
data$Date <- data $DATE

data$Month <- month(data$Date)
data$Year <- year(data$Date)


data <- data %>%
  mutate(
    Lag_1 = lag(IPG2211A2N, 1),
    Lag_2 = lag(IPG2211A2N, 2),
    Lag_3 = lag(IPG2211A2N, 3),
    Rolling_Mean_3 = rollmean(IPG2211A2N, k = 3, fill = NA, align = "right"),
    Rolling_Mean_6 = rollmean(IPG2211A2N, k = 6, fill = NA, align = "right"),
    Rolling_Mean_12 = rollmean(IPG2211A2N, k = 12, fill = NA, align = "right")
  )

data <- na.omit(data)

train_size <- nrow(data) - 12
train_data <- data[1:train_size,]
test_data <- data[(train_size + 1):nrow(data),]

predictors <- c("Lag_1", "Lag_2", "Lag_3", "Rolling_Mean_3", "Rolling_Mean_6", "Rolling_Mean_12", "Month", "Year")
X_train <- train_data[, predictors]
y_train <- train_data$IPG2211A2N
X_test <- test_data[, predictors]
y_test <- test_data$IPG2211A2N


set.seed(442200)

rf_model <- randomForest(X_train, y_train, ntree = 100, mtry = 3, importance = TRUE)

test_data$Predicted <- predict(rf_model, X_test)

mae <- abs(mean((test_data$Predicted - test_data$IPG2211A2N)))
r_squared <- cor(test_data$Predicted, test_data$IPG2211A2N)^2

print(mae)
print(r_squared)


ggplot(test_data, aes(x = Date)) +
  geom_line(aes(y = IPG2211A2N, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1, linetype = "dashed") +
  ggtitle("Actual vs Predicted Electric Production") +
  ylab("Electric Production") +
  xlab("Date") +
  theme_minimal() +
  scale_color_manual(name = "Legend", values = c("Actual" = "blue", "Predicted" = "red"))


