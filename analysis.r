library(lubridate)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(caret)
library(forecast)
library(tseries)
library(prophet)
library(reticulate)
library(tensorflow)
library(keras)
library(tidyverse)

df <- read.csv("ecommerceData.csv")
colnames(df)<-c('UID','PID','Category','Price','Discount','FP','PM','Date')
  
summary(df)

str(df)

df$Date <- dmy(df$Date)

any(is.na(df))

# unique users
length(unique(df[,1]))

# unique products
length(unique(df[,2]))

# unique categories
unique(df[,3])

# unique payment methods
unique(df[,'PM'])

# unique Discount values
unique(df[,'Discount'])


# Category vs Discount
df$Discount<- as.factor(df$Discount)
df %>% 
  count(Category, Discount) %>% 
  ggplot(aes(x = Category, y = n, fill = Discount)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Discount Distribution by Category", x = "Category", y = "Discount") +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

# payment method vs category 
df %>% 
  count(Category, PM) %>% 
  ggplot(aes(x = Category, y = n, fill = PM)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Payment Method Distribution by Category", x = "Category", y = "Payment Method")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Price by category
ggplot(df, aes(x = Category, y = `Price`, fill = Category)) +
  geom_boxplot() +
  labs(title = "Price by Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Discounted price by category
ggplot(df, aes(x = Category, y = `FP`, fill = Category)) +
  geom_boxplot() +
  labs(title = "Sales' Price by Category", y = "Sales price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# daily price sum
df_summary <- df %>%
  group_by(Date) %>%
  summarize(Total_Price = sum(FP, na.rm = TRUE))

ggplot(df_summary, aes(x = Date, y = Total_Price)) +
  geom_line() +
  labs(title = "Daily Revenue", x = "Date", y = "Total Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# to get a clear view we will select 60 days to examine our data
ggplot(df_summary[1:60, ], aes(x = Date, y = Total_Price)) +
  geom_line() +
  labs(title = "Daily Revenue", x = "Date", y = "Total Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# we notice a weekly trend

df$YearMonth <- format(df$Date, "%Y-%m")

df_monthly_summary <- df %>%
  group_by(YearMonth) %>%
  summarize(Monthly_Price = sum(FP, na.rm = TRUE))

ggplot(df_monthly_summary, aes(x = YearMonth, y = Monthly_Price)) +
  geom_line(group = 1, color = "blue") +
  geom_point(color = "red") +
  labs(title = "Monthly Total of Purchases", x = "Month", y = "Total Revenu") +
  theme_minimal()

df <- df %>% select(-UID)
df <- df %>% select(-PID)


df_summary$Date <- as.Date(df_summary$Date)
ts_data <- ts(df_summary$Total_Price, start = c(year(min(df$Date)), month(min(df$Date))), frequency = 7)

acf(ts_data)
pacf(ts_data)

adf_test <- adf.test(ts_data)
print(adf_test)

kpss_test <- kpss.test(ts_data)
print(kpss_test)

fit <- auto.arima(ts_data)
summary(fit)

ets_model <- ets(ts_data)
summary(ets_model)

sarima_model <- auto.arima(ts_data, seasonal = TRUE)
summary(sarima_model)

tbats_model <- tbats(ts_data)
summary(tbats_model)


df_prophet <- df_summary %>%
  rename(ds = Date, y = Total_Price)

prophet_model <- prophet(df_prophet)
future <- make_future_dataframe(prophet_model, periods = 30)
forecast_prophet <- predict(prophet_model, future)
plot(prophet_model, forecast_prophet)
prophet_plot_components(prophet_model, forecast_prophet)


install_tensorflow()
install_keras()


normalize <- function(x) (x - min(x)) / (max(x) - min(x))
df_summary$Total_Price <- normalize(df_summary$Total_Price)

ts_data <- ts(df_summary$Total_Price, frequency = 365)
train_size <- floor(0.8 * length(ts_data))
train_data <- ts_data[1:train_size]
test_data <- ts_data[(train_size + 1):length(ts_data)]

create_sequences <- function(data, n_steps) {
  x <- NULL
  y <- NULL
  for (i in 1:(length(data) - n_steps)) {
    x <- rbind(x, data[i:(i + n_steps - 1)])
    y <- c(y, data[i + n_steps])
  }
  return(list(x = array(x, dim = c(nrow(x), n_steps, 1)), y = y))
}

train_seq <- create_sequences(train_data, 10)

test_seq <- create_sequences(test_data, 10)

model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(n_steps, 1), return_sequences = FALSE) %>%
  layer_dense(units = 1)
