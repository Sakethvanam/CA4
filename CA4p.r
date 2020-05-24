library(plyr)
library(readr)
# Loading dataset
property_da <- read.csv('C:/Users/Saketh Vanam/Downloads/Data Science/Property_Price_Register.csv')

#Removing € from price column
property_da$Price = as.numeric(gsub("[\\€,]", "", property_df$Price))
property_da$Price
#displaying the structure of dataset 
str(property_da)


#data frame is created
property_df <- data.frame(property_da)


input_colnames <- c("dos", 
                    "Address",
                    "Postcode", 
                    "County",
                    "Price", 
                    "Not Full Market Price",
                    "VAT Exclusive", 
                    "Description")
colnames(property_df) <- input_colnames
str(property_df)



#new data frame is created for filtering only one county data as data is too large. 
#only dublin data is taken from 2010 to 2019
new_df <- data.frame(property_df$dos, property_df$County, property_df$Price)
str(new_df)
#renaming columns
out_col <- c ("dos", "county", "price")
colnames(new_df) <- out_col

new_df

#As we can see there are 359295 columns in the dataset. filtering data only for dublin.


new_df <- subset(new_df, new_df$county == 'Dublin')

write.csv(new_df, "Newfile.csv")

#---------loading new csv file in dataframe

df_1 <- read.csv("Newfile.csv")
str(df_1)
#As we can see data frame is created.  and structure of data is displayed.
#only years are taken along  with prices  and selected county as dublin 
head(df_1,20)
year_field <- format(as.Date(df_1$dos, format="%d/%m/%Y"),"%Y")
df_1$dos <- year_field
str(property_df)
df_1$dos <- as.numeric(df_1$dos)
str(df_1) 
#-----------Model estimation-------------
#first 90000 data is taken. 
#Time series is taken as my data consists of years from 2010 to 2019 .
sales_data <- ts(df_1$price[0:90000], start= c(2010), end = c(2019), frequency = 1)
sales_data

plot(sales_data)

start(sales_data)
end(sales_data)
frequency(sales_data)


# Installing the necesary time series and forecasting libraries.
#install.packages("tseries")
#install.packages("forecast")
library(forecast)
library(tseries)

# Plotting the Auto corelation function Acf() plot. 
acf_results <- Acf(sales_data, main = "ACF of sales data")
acf_results
#the measure of autocorrelation doen not cross the dashed blue line,
# then that specific lag is not significantly correlated with the associated
pacf_results <- Pacf(sales_data, main = "PACF of sales data")
pacf_results
#another test augmented dickey fuller test is conducted to check for the stationary
adf.test(sales_data) # p-value > 0.05 indicates the Time Series is not stationary
plot(sales_data, main = "Raw time series")

#checking for seasonal differencing
ndiffs(sales_data)
#clearly we can see, the differencing value is 0.

#-----calculating mean and median for the price column.
price_mean <- c(as.numeric(mean(df_1$price)))
price_mean
price_median <- c(as.numeric(median(df_1$price)))
price_median


#---------Arima model--------------
arima_model <- arima(sales_data, order = c(1,0,1))
arima_model
#AIC values are high (294).


#-----------------auto arima model-----------------
auto_arima_model <- auto.arima(sales_data)
auto_arima_model
#AIC value is 293. which is almost equivalent to arima model

#-----------finding accuracy for both the models ---------
accuracy(auto_arima_model)
#MAPE is 42.4
accuracy(arima_model)
#MAPE(Mean absolute percentage error)  value is 52. 


#--------Evaluation the models.----------------
#residuals is difference between the true and predicted values.
qqnorm(arima_model$residuals,
       main = "Normal Q-Q Plot (Estimated ARIMA Model)")
qqline(arima_model$residuals)

Box.test(arima_model$residuals, type = "Ljung-Box")
#p value is 0.85 which is greater than 0.05. so null hypothesis is accepted

#----------qq plot for auto arima model--------

qqnorm(auto_arima_model$residuals,
       main = "Normal Q-Q Plot (Estimated ARIMA Model)")
qqline(auto_arima_model$residuals)

Box.test(auto_arima_model$residuals, type = "Ljung-Box")
#p value is 0.6 which is greater than 0.05.

#------------training and testing data----------
#training data is taken for years 2010 to 2017
train_data <- window(x = sales_data, start=c(2010), end=c(2017))
#testing data is taken for years 2018 to 2019
test_data <- window(x=sales_data, start=c(2018), end=c(2019))
train_data
test_data


#-----------fitting the model--------


fit <- arima(train_data, c(1,0,1))
fit
#AIC value is 238.25

#-------forecasting auto arima model--------
predict_auto_arima <- forecast(auto_arima_model)
predict_auto_arima
plot(forecast(auto_arima_model, 10), xlab = "Year", ylab = "sales prices")

#---------Forecasting Arima model----------
predict_arima <- forecast(arima_model)
predict_arima
plot(predict_arima, xlab = "Years", ylab = "sales price")



# make actuals_predicted dataframe
# for auto ARIMA
actuals_predictions <- data.frame(cbind(actuals =test_data, predicted = predict_arima))
head(actuals_predictions)

