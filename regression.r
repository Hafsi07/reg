library(dplyr)
library(ggcorrplot)
library(corrplot)
library(ggplot2)
library(GGally)

#Loading Data
df<-read.csv("C:\\Users\\hafsi\\Downloads\\Rice.csv")

#Exploring the data
summary(df)
str(df)
dim(df)

#we notice that the categorical variables aren't identified as factor but as character
#so we convert them 
df <- df %>% mutate(across(where(is.character), as.factor))

#double checking
colnames(df)[sapply(df,is.factor)]

#Checking for Null values
any(is.na(df))
any(is.null(df))

str(df)

#Visualizing data

# Scatter plots for each predictor against the target variable
num_cols <- sapply(df, is.numeric)
par(mfrow = c(2, 2))  # Create a 2x2 grid of plots.

for (i in 1..5){

}
for (col in names(df)[num_cols[:4]]) {
  plot(df[[col]], df$goutput, main = paste("Scatterplot of", col, "vs Goutput"), xlab = col, ylab = "Target", col = "blue")
}

#we notice the significance of labour hours, size, urea, phosphate, and seed. maybe pesticide to some extent


cat_cols <- sapply(df, is.factor)
for (col in names(df)[cat_cols]) {
  barplot(table(df[[col]]), main = paste("Barplot of", col), col = "lightgreen", las = 2)
}
# We notice imbalance in most of the categorical variables except the region variable 



# Correlation matrix to view the most directly influential numeric variables
par(mfrow=c(1,1))
cor_matrix <- cor(df[, num_cols], use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 0.8)

# Selection of highly correlated variables with the goutput
cor_goutput <- cor_matrix["goutput", ]
high_corr_cols <- names(cor_goutput[abs(cor_goutput) > 0.75 & names(cor_goutput) != "goutput"])

colnames(df[, high_corr_cols])

ggpairs(df[, colnames(df[, c("size", "urea", "hiredlabor", "totlabor",'goutput')])])



# Modeling

# we have to exclude noutput as it dissipates data to the target variable and ofc id
df <- df %>% select(-noutput)
df <- df %>% select(-id)


set.seed(64587) 
indices <- sample(1:nrow(df), size = 0.7* nrow(df))
train <- df[indices, ]
test <- df[-indices, ]

# model with all the variables 
mod.v1<- lm(goutput~.,data=train)
aicv1<-AIC(mod.v1)
r2v1<-summary(mod.v1)$r.squared
summary(mod.v1)
