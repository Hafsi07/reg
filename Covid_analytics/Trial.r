require(tidyverse)
require(magrittr)
require(tscount)
library(zoo)
library(ggplot2)

df <- read.csv("../Downloads/COVIDenh.csv")

str(df)

# removing index column
#df <- df[2:length(df[1, ])]

# renaming columns
df %<>% rename(
  date = data,
  country = stato,
  hospitalized_with_symptoms = ricoverati_con_sintomi,
  intensive_care = terapia_intensiva,
  total_hospitalized = totale_ospedalizzati,
  home_isolation = isolamento_domiciliare,
  total_positive = totale_positivi,
  change_total_positive = variazione_totale_positivi,
  new_positive = nuovi_positivi,
  recovered = dimessi_guariti,
  deceased = deceduti,
  cases_diagnostic_suspicion = casi_da_sospetto_diagnostico,
  cases_screening = casi_da_screening,
  total_cases = totale_casi,
  swabs = tamponi,
  cases_tested = casi_testati,
  notes = note,
  intensive_care_admissions = ingressi_terapia_intensiva,
  test_notes = note_test,
  case_notes = note_casi,
  total_positive_molecular_test = totale_positivi_test_molecolare,
  total_positive_rapid_antigen_test = totale_positivi_test_antigenico_rapido,
  molecular_test_swabs = tamponi_test_molecolare,
  rapid_antigen_test_swabs = tamponi_test_antigenico_rapido
)

# transforming data types
df$date <- as.Date(df$date)

# checking na values
df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variables",
               values_to = "NA_count") %>% 
  filter(NA_count > 0)

# checking for columns' utility
dim(df)
length(unique(df[, "date"]))
unique(df[, "country"])
unique(df[, "notes"])

unique(df[, "test_notes"])
unique(df[, "case_notes"])

# we need to drop the insignificant columns
df %<>%
  select(-c("country", "notes", "cases_diagnostic_suspicion",
            "cases_screening", "test_notes", "case_notes"))

# we verify again if the rest of the columns contain unique or only NA values
df %>%
  select(where(~ sum(is.na(.)) > 0)) %>%
  reframe(across(everything(), ~ list(head(unique(.), 3))))

str(df)

ggplot(df, aes(x = date, y = total_cases)) +
  geom_line(color = "black", size = 1) +
  labs(title = "Total Cases Over Time", x = "Date", y = "Total Cases") +
  theme_minimal()

# distribution of cases 
ggplot(df, aes(x = total_cases)) +
  geom_histogram(binwidth = 1000000, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Total Cases", x = "Total Cases", y = "Frequency") +
  theme_minimal()
  

# density plot showing the rapid growth or spread of corona
# the values in the middle occured wayyy less than on the sides 
# We have to consider the number of tests done as well to confirm
ggplot(df, aes(x = total_cases)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  labs(title = "Density Plot of Total Cases", x = "Total Cases", y = "Density") +
  theme_minimal()

ggplot(df, aes(x = date, y = log(df$total_cases + 1)))+
  labs(title = "Total Cases Over Timess", x = "Date", y = "Total Cases") +
  theme_minimal()


numvars <- df %>% select(where(is.numeric))
numplots <- ncol(numvars)
par(mfrow = c(1, 1), mar = c(3, 3, 2, 1))

for (var in names(numvars)) {
  plot(Date, numvars[[var]], type = "l",
       main = var, xlab = "Date", ylab = "")
}

df %>% arrange(.,by=date) %>% select()

plot(acf(df$new_positive))
plot(pacf(df$new_positive))


# mod mod mod mod mod mod mod mod mod mod mod
ldf <- df[df$date >= as.Date("2023-01-01"), ] %>% arrange(., by = date) # Second wave
ldf$rolling_avg <- rollmean(ldf$new_positive, k = 7, fill = NA, align = "right")
ldf <- ldf[7:length(ldf$new_positive),]

plot(acf(ldf$rolling_avg))
plot(pacf(ldf$rolling_avg))

#days of the week
ldf$DayOfWeek <- weekdays(ldf$date)

# Create dummy variables
ldf <-cbind(ldf, dummy(ldf))



ggplot(ldf, aes(x = date)) +
  geom_line(aes(y = new_positive, color = "Actual Cases"), size = 1) +
  geom_line(aes(y = rolling_avg, color = "7-Day Rolling Avg"), size = 1.2) +
  labs(title = "COVID Cases with Rolling Average",
       x = "Date", y = "Total Cases") +
  scale_color_manual(values = c("Actual Cases" = "blue", "7-Day Rolling Avg" = "red")) +
  theme_minimal()

#preparing model training covariates
covs <- as.matrix(sapply(ldf[,c("DayOfWeek_Tuesday", "DayOfWeek_Wednesday", "DayOfWeek_Thursday",
                         "DayOfWeek_Friday", "DayOfWeek_Saturday", "DayOfWeek_Sunday")], as.numeric))
covs <- cbind(covs, scale(ldf$rolling_avg)+1)

# Fit separate glms based on distribution 
model1 <- tsglm(ldf$new_positive, model = list(past_obs = c(1, 7)), distr = "poisson")
model2 <- tsglm(ldf$new_positive, model = list(past_obs = c(1, 7)), xreg = covs, link = "log",distr = "nbinom")

summary(model1)
summary(model2)
exp(model2$coefficients)

train_size <- round(length(ldf$new_positive)*0.8)

#split
train_data <- ldf[1:train_size,]
test_data <- ldf[(train_size+1):(length(ldf$new_positive)-100),]
train_dates <- ldf$date[1:train_size]
test_dates <- ldf$date[(train_size + 1):(nrow(ldf)-100)]


#models
mod_1 <- tsglm(train_data$new_positive, model = list(past_obs =c(1,7)), distr = "nbinom")
mod_2 <- tsglm(train_data$new_positive, model = list(past_obs =c(1:7)), xreg = covs[1:train_size,], distr = "nbinom")
mod_3 <- tsglm(train_data$new_positive, model = list(past_obs = c(1, 7), past_mean=c(1, 7)), distr = "nbinom")
mod_4 <- tsglm(train_data$new_positive, model = list(past_obs = c(1, 7), past_mean=c(1, 7)), xreg = covs[1:train_size,], distr = "nbinom")
mod_5 <- tsglm(train_data$new_positive, model = list(past_mean=c(1, 7)), link = "log", xreg = covs[1:train_size,], distr = "nbinom")



#forecast
fare <- function(x){
  pred <- predict(x, n.ahead = length(test_data$new_positive))$pred
  print(summary(x))
  
  plot_data <- data.frame(
    Date = test_dates,
    Actual = test_data$new_positive,
    Predicted = pred
  )
  pred[1:4]
  
  gg <- ggplot(plot_data, aes(x = Date)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1.2) +
    geom_line(aes(y = Predicted, color = "Predicted"), size = 1.2, linetype = "dashed") +
    labs(title = "Actual vs Predicted cases",
         x = "Date", y = "Total Cases") +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_minimal()
  print(gg)
}

fare(mod_1)
fare(mod_2)
fare(mod_3)
fare(mod_4)
fare(mod_5)


# what if we put the average transition ration of each day in reference to the previous day
