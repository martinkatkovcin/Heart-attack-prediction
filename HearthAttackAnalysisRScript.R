# Unnecessary libraries to install before running this R script
# install.packages("tidyverse")
# install.packages("dbplyr")
# install.packages("magrittr")
# install.packages("corrplot")
# install.packages("gmodels")
# install.packages("ggplot2")
# install.packages("ggpubr")
# install.packages("ggiraphExtra")
# rm(list = ls()) # reset all enviroment

# Unnecessary libraries to run this R script
library(tidyverse)
library(dbplyr)
library(magrittr)
library(corrplot)
library(gmodels)
library(ggplot2)
library(ggpubr)
library(ggiraphExtra)

# Hypothesis 0: ,,Predpokladáme, že hodnota cholesterolu ostáva rovnaký so stúpajúcim vekom, čo znamená, 
# že ľudia vo vyššom veku majú rovnakú šancu dostať infarkt."
# Hypothesis 1: ,,Predpokladáme, že hodnota cholesterolu sa zvyšuje so stúpajúcim vekom, čo znamená, 
# že ľudia vo vyššom veku majú vyššiu šancu dostať infarkt."

# Loaded dataset to dataframe
data <- read.csv('heart.csv')

# Show first 3 and last 3 rows (getting to know the Dataset)
show_ht <- function(data)
{
  head_tail = rbind(head(data, 3), tail(data, 3))
  print(head_tail)
}
show_ht(data)

# Function, that can find us, if we have some duplicates in the Dataframe
find_duplicates <- function(data)
{
  rows = dim(data)[1]
  unique = dim(data %>% 
                 group_by_all %>% 
                 count)[1]
  count_duplicates = (rows - unique)
  cat('Found', count_duplicates, 'duplicates')
}
find_duplicates(data)

# We found 2 same rows, so we are deleting a duplicate row
data = distinct(data)
find_duplicates(data)

# There are no NA values, but if we had some, we would replace them by the mean values of the column, or maybe the most 
# frequented values of the variable, if there were just a few, we would just delete them (rows with NA values)
data %>% 
  map_int(~sum(is.na(.x)))

# Getting to know the dataframe (number of rows, number of variables, types of variables, number of NA values by column)
# nrow(data), ncol(data), colnames(data) could be also used
# Every variable is numeric, so we don't have to do some change in the dataset (we need numeric variables, if we want to use models)
data %>% 
  glimpse()

# Checking in Boxplots for outliers
# There are just less than 5 outleirs in the Cholesterol variable
boxplot(data$chol, ylab = 'Cholesterol', col = 'lightblue') + title('Boxplot to indentify outliers')
boxplot(data$age, xlab = 'Age', horizontal = TRUE, col = 'lightblue') + title('Boxplot to indentify outliers')

# Visualization by density plots
data %>% 
  select(-output, age, chol) %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid)) %>% 
  ggplot(aes(x = value)) + geom_density() + facet_wrap(~name, scale = 'free')

# Descriptive statistics for important variables (associated with hypothesis)
data %>% 
  select (age, chol) %>%
  summary()

# Age, cholesterol histograms - getting to know the frequency of the values we have in the Dataframe
hist(data$age, main = 'Histogram of patients by age', xlab = 'Age', col = 'lightblue')
hist(data$chol, main = 'Histogram of patients by cholesterol', xlab = 'Cholesterol', col = 'lightblue')

# Pair analysis between age and cholesterol with attribute Output - Indicates Hearth attack or not (1, 0)
# From the graphs, we can clearly see that with higher age
ggplot(data = data, aes(x = age, group = output, colour = output, fill = output)) + xlab('Age') + ylab('Density') + geom_density(alpha = 0.4) + ggtitle('Age with indicator of Heart attack (output)')
ggplot(data = data, aes(x = chol, group = output, colour = output, fill = output)) + xlab('Cholesterol') + ylab('Density') + geom_density(alpha = 0.4)  + ggtitle('Cholesterol with indicator of Heart attack (output)')

# Pearson correlation coefficient we count, that indicates if there is relationship between cholesterol and age (interval <-1, 1>)
# We can see, we got correlation between our 2 variables, it's positive number, so we can
# say the values are directly proportional, when one value is increasing, other one is also (reversly same ideology)
# We did also correlation matrix, but for better visualization we made the Correlation plot (same functionality if we had done Heatmap)
round(cor(data), 2)
corrplot(cor(data), method = 'circle', order = 'alphabet', title = 'Correlation plot')
cor(data$age, data$chol)

# Visualization of our variables that we chose by scatter plot (linear regression model) - We did 3 possible ways of ploting the LRM
ggscatter(data, x = 'age', y = 'chol', add = 'reg.line', conf.int = TRUE, cor.coef = TRUE, cor.method = 'pearson', xlab = 'Age', ylab = 'Cholesterol')  + ggtitle('Graph of linear regression')
ggplot(data,aes(x = age, y = chol)) + xlab('Age') + ylab('Cholesterol') + geom_point() + geom_smooth(method = 'lm', se = FALSE) + ggtitle('Graph of linear regression')

linear_reg_line <- lm (data$chol ~ data$age)
plot (data$chol ~ data$age,
      main = 'Linear regression with values of cholesterol and age of the patient',
      xlab = 'Age',
      ylab = 'Cholesterol',
      pch = 16)
abline(linear_reg_line, col = 'red')

# We realized from the graph, that there can be linear relationship, but maybe there's some curve that we can't see, so we have to dig more
# Checking if the variables are from normal distribution using Shapiro-Wilk test -> We can use, because we have less then 5000 rows in the Dataframe
# otherwise we would have to use Kolmogorov-Smirnov test - both significance level is 0.05
# We see, taht our p values are less then level of significance, so we are dealing with data, that are not from normal distribution so we are
# REJECTING Hypothesis 0, also we visualzed by Q-Q plots, that's also rejecting Hypothesis 0
shapiro.test(data$age)
shapiro.test(data$chol)
ggqqplot(data$age, ylab = 'Age')
ggqqplot(data$chol, ylab = 'Cholesterol')

# Beta0 (intercept - úrovňová konštanta) and Beta1 (slope - sklon krivky)
# Our predictor (X) represents variable age and response (Y) represents variable chol of the patient
# Y ≈ Beta0 + Beta1*X
# With function summary(), we can see, that we have not that high Standard Error, so that's good sign
# so we continue analysing the model, also we can see that our Intercept is 181,9980 and slope is 
# 1,1863, so our line will be ascending, that's what we predicted
# Significance value is less then 0.05, we can reject Hypothesis 0 in the dataframe we loaded
# we will have a look in our models, if this value will be still less then 0.05
fit <- lm(chol ~ age, data = data)
summary(fit)

# Residuals are the points, which are in the graph, which will help us, to make the average distance from the regression line
residuals(fit)

# Cross validation
# 1.
# Making 150 subsets (we have 303 rows, that should be enough, more than 50% of the source data)
new_subsets <- map(1:150, ~ data[sort(sample(1:dim(data)[1], size = 0.5*dim(data)[1])),])
new_subsets
# 2.
# Fitting linear regression model to each of these newly created subsets
models <- map(new_subsets, ~ lm(.x$chol ~ .x$age))
models <- map(new_subsets, function(x) lm(x$chol ~ x$age))

# 3.
# Extracting Beta coefficient and also residuals for each model we created
# Printing table, if everything went well
# For every model we are saving coefficients Beta0 and Beta1 and residuals
functionsList <- list(coefficients = coef, residuals = residuals)
extract_ceofficients <- function(x) 
{
  sapply(functionsList, function (g) g(x))
}
extractedData <- map(models, ~extract_ceofficients(.x))
extractedData

# 4. 
# Calculating standard deviation of intercept and slope values
# Standard errors are 16,7144 and 0,3162243, that values are really close to our 
# first fit model (linear regression model), and we assumed, that standard errors are 
# such a low values, so the model fit well
# In this we can see also, if we check Beta0 and Beta1 values of coefficients,
# we can see, that we took one model, that has value of significance greater than 0.05
# so we can't conclude, that a significant difference exists, so we can't reject Hypothesis 0
# because of this, so we failed to rejecting it
map(models, ~ coef(.x))
sd(map_dbl(models, ~ coef(.x)[1])) # slope
sd(map_dbl(models, ~ coef(.x)[2])) # intercept

sig_test <- data.frame(new_subsets[143])
test_fit <- lm(chol ~ age, data = sig_test)
summary(test_fit)

# 5.
# Calculate standard error of residual sum of squares (RSS)
# and also residual mean square error (RMSE) for each set we made
# We chose the boxplots for visualizing the standard errors
rss <- map_dbl(models, ~ sum(resid(.x)^2))
rse <- map_dbl(rss, ~ sqrt(.x/(0.5*dim(data)[1]-2)))
boxplot(rss, main = 'Residual sum of squares')
boxplot(rse, main = 'Residual standard error')

# 6. (purrr style)
# Checking if there is relation between response and the predictor
# We can see, there's p value. less then 0.05, so again we rejected Hypothesis 0
cfs <- map_dbl(models, ~ coef(.x)[2])
t.test(cfs, mu = 0)

# Graph with cholesterol and age of the Dataset with indicator output (Heart attack)
# We can also see, that  both are are regression lines and both and also, that with the age
# is value of cholesterol increasing and we can see, that higher age and higher cholesterol 
# patient had, there was higher chance of getting the heart attack
# We are confirming Hypothesis 1 #
fit_output <-  lm(chol ~ age*output, data = data)
ggPredict(fit_output, colorAsFactor = TRUE, ineteractive = TRUE)  + ggtitle('Graph of linear regression with indicator')

