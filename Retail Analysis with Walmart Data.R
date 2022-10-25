#Importing the required libraries
library(dplyr)
library(lubridate)
library(ggplot2)

#Importing the dataset
walmart_sales = read.csv('Walmart_Store_sales.csv')
View(walmart_sales)
str(walmart_sales)

#Converting date variable to the right format
walmart_sales$Date = as.Date(walmart_sales$Date, format = '%d-%m-%Y')
str(walmart_sales)

#Creating quarter column
walmart_sales$quarter = quarter(walmart_sales$Date)

#Creating year column
walmart_sales$year = year(walmart_sales$Date)


#--------------------------Basic Statistic tasks--------------------------------
#---Which store has maximum sales
x = aggregate(Weekly_Sales~Store, walmart_sales, sum)
x = arrange(x, desc(Weekly_Sales))
max_sales = max(x$Weekly_Sales)
store_with_max_sales = x$Store[1] 
print(paste('Store no.', store_with_max_sales, 
            'has the maximum sales and the maximum sales = ', max_sales))

##Store number 20 has the maximum sales




#---Which store has maximum standard deviation i.e., the sales vary a lot. 
#---Also, find out the coefficient of mean to standard deviation

y = walmart_sales %>% 
    group_by(Store) %>% 
    summarise(mean_sales = mean(Weekly_Sales), 
              stdDev_sales = sd(Weekly_Sales))
  
y = arrange(y, desc(stdDev_sales))

max_sd = max(y$stdDev_sales)
store_with_maxSD = y$Store[1]

print(paste('Store no. ', store_with_maxSD, 
            'has the maximum standard deviation of ', max_sd))
# Store number 14 has maximum standard deviation of 317569.949

y$coeff_mn_sd = y$stdDev_sales / y$mean_sales

print(paste('Coefficient of mean to standard deviation for store', 
            store_with_maxSD, 'is ', y$coeff_mn_sd[1]))

#Though the standard deviation is high, the coefficient of mean to standard deviation is near ideal ratio




#---Which store/s has good quarterly growth rate in Q3'2012
p = aggregate(Weekly_Sales~Store + quarter + year, walmart_sales, sum)

q2 = filter(p, year == 2012 & quarter == 2 ) #Q2 sales
q2 = q2 %>% rename(Q2sales = Weekly_Sales)

q3 = filter(p, year == 2012 & quarter == 3 ) #Q3 sales
q3 = q3 %>% rename(Q3sales = Weekly_Sales)

q3 = q3[, c("Store", "Q3sales")]    #Dropping unnecessary columns
q3$Q2sales = q2$Q2sales

q3$growthRate = ((q3$Q3sales - q3$Q2sales)/q3$Q2sales)*100
q3 = arrange(q3, desc(growthRate))

max_growth_rate = max(q3$growthRate)
store_max_growth = q3$Store[1]

print(paste('Store no.', store_max_growth, 
            'has the highest quarterly growth of', round(max_growth_rate), 
            '% in Q3 2012'))

#Store no 7 has highest quarterly growth rate in Q3 2012 - 13%





#---Some holidays have a negative impact on sales. 
#---Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together
avg_sales_nonhldy = mean((filter(walmart_sales, Holiday_Flag == 0))$Weekly_Sales)

hldy_data = filter(walmart_sales, Holiday_Flag == 1)
r = aggregate(Weekly_Sales~Date, hldy_data, mean)

r$higher_than_avg = ifelse(r$Weekly_Sales > avg_sales_nonhldy, TRUE, FALSE)

good_hldys = r %>% filter(higher_than_avg == TRUE)

good_hldys$Date






#---Provide a monthly and semester view of sales in units and give insights

#Monthly sales
walmart_sales$year_mnth = format(walmart_sales$Date, "%Y-%m")
m = aggregate(Weekly_Sales~year_mnth, walmart_sales, sum)

ggplot(m, aes(year_mnth, Weekly_Sales)) + 
  geom_line(aes(group = 1)) + 
  geom_point()  + 
  theme(axis.text.x = element_text(size=8, angle=90)) +
  ggtitle('Monthly Sales - 2010 to 2012')

#The sales are highest in December and lowest in January


#Semester wise sales
walmart_sales$sem = semester(walmart_sales$Date)
s = aggregate(Weekly_Sales~year+sem, walmart_sales, sum)
s$year_sem = paste(s$year,' S', s$sem, sep = '')

ggplot(s, aes(year_sem, Weekly_Sales)) + 
  geom_line(aes(group = 1)) + 
  geom_point() + 
  ggtitle('Semester Sales - 2010 to 2012')

#The sales are higher in second semester of every year. 
#The plot shows a drop in sales in 2012 S2. 
#This is because the data does not contain December and hence is incomplete for 2012


#------------------------------Stat model---------------------------------------
#Statistical Model
#For Store 1 - Build  prediction models to forecast demand
#.	Linear Regression - Utilize variables like date and 
#restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). 
#Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
#.	Change dates into days by creating new variable.
#Select the model which gives best accuracy.


#Filtering store 1 data
store1_data = filter(walmart_sales, Store == 1)

#Restructuring date column as numbers
store1_data$datenum = 1:length(unique(store1_data$Date))



#Model 1------------------------------------------------------------------------
#Selecting only required columns(CPI, unemployment, fuel price, datenum)
dataset = store1_data[, c(3, 6:8, 13)]


# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Weekly_Sales ~ .,
               data = dataset)
summary(regressor)

#Out of the four variables that were used, 'unemployment; is the only one that is significant

#Predicting test set results
sales_pred = predict(regressor, newdata = dataset)

dataset_final = cbind(dataset, sales_pred)
dataset_final = transform(dataset_final, 
                          Error_pct = abs((Weekly_Sales - sales_pred)/Weekly_Sales))

#Error of the model
mean(dataset_final$Error_pct)

#Accuracy of the model
1 - mean(dataset_final$Error_pct)

#Model 2-------------------------------------------------------------------------------
#Using only 'Unemployment' column
dataset = store1_data[, c(3, 8)]


# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Weekly_Sales ~ .,
               data = dataset)
summary(regressor)
#Very low r square which implies that the model is not a good one


#Predicting test set results
sales_pred = predict(regressor, newdata = dataset)

dataset_final = cbind(dataset, sales_pred)
dataset_final = transform(dataset_final, 
                          Error_pct = abs((Weekly_Sales - sales_pred)/Weekly_Sales))

#Error of the model
mean(dataset_final$Error_pct)

#Accuracy of the model
1 - mean(dataset_final$Error_pct)

#The accuracy of the model doesn't change much after dropping the insignificant columns


#Model 3-------------------------------------------------------------------------------
#Using all columns

dataset = store1_data[, c(3:9, 13)]



# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Weekly_Sales ~ .,
               data = dataset)
summary(regressor)

#The variables, 'Holiday_flag', 'Temperature', 'CPI', 'quarter' are significant

#Predicting test set results
sales_pred = predict(regressor, newdata = dataset)

dataset_final = cbind(dataset, sales_pred)
dataset_final = transform(dataset_final, 
                          Error_pct = abs((Weekly_Sales - sales_pred)/Weekly_Sales))

#Error of the model
mean(dataset_final$Error_pct)

#Accuracy of the model
1 - mean(dataset_final$Error_pct)

#Model 4-------------------------------------------------------------------------------
#Using 'Holiday_flag', 'Temperature', 'CPI', 'quarter' variables
dataset = store1_data[, c(3, 4, 5, 7, 9)]


# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Weekly_Sales ~ .,
               data = dataset)
summary(regressor)

#The variables, 'Holiday_flag', 'Temperature', 'CPI', 'quarter' are significant

#Predicting test set results
sales_pred = predict(regressor, newdata = dataset)

dataset_final = cbind(dataset, sales_pred)
dataset_final = transform(dataset_final, 
                          Error_pct = abs((Weekly_Sales - sales_pred)/Weekly_Sales))

#Error of the model
mean(dataset_final$Error_pct)

#Accuracy of the model
1 - mean(dataset_final$Error_pct)

#Conclusions:
#1. The accuracy of all the above four models is nearly equal
#2. Comparing the R squared and adjusted R squared values for the models
#3. The 4th model('Holiday_flag', 'Temperature', 'CPI', 'quarter') is the best one
#4. It has an Rsquare of 0.1678 and adjRsquare of 0.1436
#5. Accuracy of the model = 0.9367
#6. All the variables used in the model have significantly low p values making it the better model

