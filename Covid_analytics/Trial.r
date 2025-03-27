require(tidyverse)
require(magrittr)

df <- read.csv("Downloads\\Covid_data.csv")

str(df)

# removing index column
df <- df[2:length(df[1, ])]

# renaming columns
namescol <- c("Date", "State", "hospitalized+symptoms", "intensive_care",
              "hospitalized", "home_isolation", "positive_cases",
              "variation_of_+_cases", "+_cases", "recovered", "deceased",
              "diagnostic_suspicion_cases", "screening_cases", "total_cases",
              "tests_performed", "tested_individuals", "note",
              "intensive_care_oggi", "test_note", "case_note",
              "molecular_test_tot", "rapid_antigen_test_tot",
              "molecular_test_oggi", "rapid_antigen_oggi", "proportion_new")
colnames(df) <- namescol

# transforming data types
df$Date <- as.Date(df$Date)

# checking na values
df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variables",
               values_to = "NA count") %>% 
  filter(. > 0)

# checking for columns' utility
dim(df)
length(unique(df[, "Date"]))
unique(df[, "State"])
unique(df[, "note"])

unique(df[, "test_note"])
unique(df[, "case_note"])


# these two columns have only 162 values and the rest is missing
sum(is.na(df$diagnostic_suspicion_cases))
sum(is.na(df$screening_cases))

# we need to drop the insignificant columns
df %<>%
  select(-c("State", "note", "diagnostic_suspicion_cases",
            "screening_cases", "test_note", "case_note"))

# we verify again if the rest of the columns contain unique or only NA values
df %>%
  select(where(~ sum(is.na(.)) > 0)) %>%
  reframe(across(everything(), ~ list(head(unique(.), 3))))

str(df)

ggplot(df, aes(x = Date, y = total_cases)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Total Cases Over Time", x = "Date", y = "Total Cases") +
  theme_minimal()

# distribution of cases 
ggplot(df, aes(x = total_cases)) +
  geom_histogram(binwidth = 1000000, fill = "blue", color = "black") +
  labs(title = "Distribution of Total Cases", x = "Total Cases", y = "Frequency") +
  theme_minimal()
  

# density plot showing the rapid growth or spread of corona
# the values in the middle occured wayyy less than on the sides 
# We have to consider the number of tests done as well to confirm
ggplot(df, aes(x = total_cases)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Total Cases", x = "Total Cases", y = "Density") +
  theme_minimal()
