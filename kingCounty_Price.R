#calling the required libraries and packages
library(lattice)
library(move)
library(moments)
library(pacman)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(ggiraphExtra)
library(plyr)
library(caret)
library(corrplot)
library(GGally)
library(ggcorrplot)
library(corrr)

#Fetching the raw data-file
data_raw <- read.csv("C:/Users/91773/Desktop/MSc Data Analytics/DMML/Project/New folder/House_Price/kc_house_dataset.csv")

#Data Exploration
str(data_raw)
dim(data_raw)
head(data_raw)
class(data_raw)
glimpse(data_raw)
data_clean <- data_raw

#Obtaining the Summary Statistics of the dataset
summary(data_clean)

#Performing data cleaning operations. Changing the date format to yymmdd format
#Selecting the sub-string of date column from 1:8
data_clean$date <- substr(data_clean$date, 1, 8)
data_clean$date <- as.numeric(as.character(data_clean$date))
head(data_clean)

#Checking for any NA values
length(which(is.na(data_clean)))

#Removing ID column since it does not add value to our visualization
data_clean$id <- NULL

par(mfrow=c(2,3))
boxplot(data_clean$bedrooms, main = "Bedroom Plot")
boxplot(data_clean$bathrooms, main = "Bathroom Plot")
boxplot(data_clean$floors, main = "Floors Plot")
boxplot(data_clean$sqft_living, main = "Living SqFt Plot")
boxplot(data_clean$sqft_above, main = "Above SqFt Plot")


#Removing Outliers
med <- quantile(data_clean$bedrooms, 0.5)
out1 <- quantile(data_clean$bedrooms, 0.95)
out2 <- quantile(data_clean$bedrooms, 0.05)
data_clean$bedrooms[data_clean$bedrooms > out1 ] <- med
data_clean$bedrooms[data_clean$bedrooms < out2 ] <- med

med <- quantile(data_clean$bathrooms, 0.5)
out1 <- quantile(data_clean$bathrooms, 0.95)
out2 <- quantile(data_clean$bathrooms, 0.05)
data_clean$bathrooms[data_clean$bathrooms > out1 ] <- med
data_clean$bathrooms[data_clean$bathrooms < out2 ] <- med

med <- quantile(data_clean$sqft_living, 0.5)
out1 <- quantile(data_clean$sqft_living, 0.95)
out2 <- quantile(data_clean$sqft_living, 0.05)
data_clean$sqft_living[data_clean$sqft_living > out1 ] <- med
data_clean$sqft_living[data_clean$sqft_living < out2 ] <- med

med <- quantile(data_clean$sqft_above, 0.5)
out1 <- quantile(data_clean$sqft_above, 0.95)
data_clean$sqft_above[data_clean$sqft_above > out1 ] <- med

par(mfrow=c(2,2))
boxplot(data_clean$bedrooms, main = "Bedroom Plot")
boxplot(data_clean$bathrooms, main = "Bathroom Plot")
boxplot(data_clean$sqft_living, main = "Living SqFt Plot")
boxplot(data_clean$sqft_above, main = "Above SqFt Plot")

#Checking for skewness and adjusting those that add value to the prediction
apply(data_clean[,1:19], 2, skewness, na.rm =TRUE)

data_clean$price <- log(data_clean$price)
data_clean$sqft_lot <- log(data_clean$sqft_lot)
data_clean$sqft_lot15 <- log(data_clean$sqft_lot15)


#Data Visualization
ggplot(data = data_clean, aes(x = sqft_living, y = price)) + geom_point() +ggtitle("Prices of houses according to Square feet")
ggplot(data = data_clean, aes(x = bathrooms, y = price)) + geom_point() +ggtitle("Prices of houses according to Bathrooms")


#finding correlation

CorrelationResults = cor(data_clean)
corrplot(CorrelationResults)



#Separating data in train and test sets
set.seed(1234)
samp <- sample(nrow(data_clean),0.75*nrow(data_clean))
train <- data_clean[samp,]
test <- data_clean[-samp,]

#Applying linear regression model on all variables to check significance of each variable
model <- lm(data = train, price ~ .)
summary(model)

#predicting prices for reduced model
pred_lm<-predict(model, newdata = test, type = 'response')

#finding RMSE(root mean square error) less the value more better the model 
#and R2 to check how much variance the model explains
RMSE(pred_lm,test$price)
R2(pred_lm,test$price)


#forward selection method
frwd_model<-step(model,direction = 'forward')
Null_to_full<-lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + 
                   waterfront + view + condition + grade + sqft_above + sqft_basement + 
                   yr_built + yr_renovated + zipcode + lat + long + sqft_living15 + 
                   sqft_lot15, data=train)
summary(Null_to_full)

#backward selection method
bckd_model<-step(model,direction = 'backward')
reduced_model<-lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + 
                    waterfront + view + condition + grade + yr_built + yr_renovated + 
                    zipcode + lat + long + sqft_living15 + sqft_lot15, data=train)
summary(reduced_model)

#plotting the reduced model to check normality and homoscidastisity
par(mfrow=c(2,2))
plot(reduced_model)

#predicting prices for reduced model
pred_log_prob<-predict(reduced_model, newdata = test, type = 'response')

#finding RMSE(root mean square error) less the value more better the model and R2 to check how much variance the model explains
RMSE(pred_log_prob,test$price)
R2(pred_log_prob,test$price)

df2<-data.frame(pred_log_prob,test$price)
write.csv(df1, file = 'C:/Users/91773/Desktop/MSc Data Analytics/DMML/Project/New Folder/House_Price/house_price_reduced_2.csv'
          ,row.names = F)


