
#Let's Begin
#download and load trip data for September 2015 for Green Taxis from the URL


data <- read.csv(url("https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv"))

#loading it locally
#data <- read.csv("C:/Users/Rasika/Desktop/Capital one/green_tripdata_2015-09.csv")

#Number of Rows
nrow(data)

#Number of Coloumns
ncol(data)


#Distribution curve
hist (data$Trip_distance, breaks = 5000, col = 'black',xlab = 'Trip Distance'
      ,main = 'Histogram of Trip distance vs frequency')

h2 <- hist (data$Trip_distance
            ,breaks = 5000
            ,col = "blue"
            ,xlab = 'Trip Distance'
            ,main = 'Trip distance vs Probability Density'
            ,xlim = c(0,mean(data$Trip_distance) + 3*sd(data$Trip_distance))
            ,freq = FALSE)

#Standard Deviation of the trip distance
sd(Trip_distance)


#Data Cleaning


#Check how many NAs exist in each column
missing_per_column <- colSums(is.na(data[,]))

missing_per_column


#clean the data function

clean_data <- function(data_set) {
  data_set <- as.data.frame(data_set)
  data_set <- data_set[,-17]
  data_set <- data_set [complete.cases(data_set),]
  data_set <- subset(data_set, data_set[,12] >= 2.5)
  data_set <- subset(data_set, data_set[,11] > 0)
  data_set <- subset(data_set, (data_set[,6] & data_set[,7] 
                                & data_set[,8] & data_set[,9]) != 0)
  #clean latitude n longitutde
}

fresh_data <- clean_data(data)


# Mean and Median trip distance grouped by hour of day

# Changing the format of date and time of pickup and drop off

fresh_data$Time_Pickup <- format(as.POSIXct(fresh_data$lpep_pickup_datetime)
                                 ,format = "%H:%M:%S")

fresh_data$Time_Dropoff <- format(as.POSIXct(fresh_data$Lpep_dropoff_datetime)
                                  ,format = "%H:%M:%S")

fresh_data$Date_Pickup <- as.Date(fresh_data$lpep_pickup_datetime)

fresh_data$Date_Dropoff <- as.Date(fresh_data$Lpep_dropoff_datetime)


library(chron)

fresh_data$Time_Pickup <- as.numeric(times(fresh_data$Time_Pickup))* 60 * 24

fresh_data$Time_Dropoff <- as.numeric(times(fresh_data$Time_Dropoff)) * 60 * 24

fresh_data$Time_duration <- abs(fresh_data$Time_Dropoff-fresh_data$Time_Pickup)

# New Variable
fresh_data$Speed <- fresh_data$Trip_distance/(fresh_data$Time_duration/60)

# Filter unknown data out
fresh_data <- fresh_data[!(fresh_data$Time_duration==0),]


# Using the package data.table
library(data.table)

# Using the data.table function for faster development for short and flexible syntax
fresh_data <- data.table(fresh_data)


attach(fresh_data)
# Converting the column back to H:M:S fomrat
fresh_data$Time_Pickup <- format(as.POSIXct(fresh_data$lpep_pickup_datetime)
                                 ,format = "%H:%M:%S")

# Grouping by hour the mean and median trip distance 
table <- fresh_data[,list(mean = mean(Trip_distance)
                          , median = median(Trip_distance))
                    ,by = list(hour(as.POSIXct(fresh_data$Time_Pickup 
                                               ,format = "%H:%M:%S")))
                    ]

# Printing the table
table

plot(table$hour,table$mean, type = 'b'
     , xlab = 'HOUR'
     , ylab = 'Mean Distance'
     , main = 'Mean Distance covered per hour'
     , col = 'red')

# JFK airport was chosen at random and an analysis was conducted on what sort of 
# transactions take place within those coordinates

# Coordinates were pick from the internet and a new data set was created on the basis
# of 4 square coordinates surrounding the airport.


# map_data JFK airport

map_data <- subset(fresh_data, (fresh_data$Pickup_latitude < 40.64999
                                | fresh_data$Dropoff_latitude < 40.64999))

map_data <- subset(map_data, (map_data$Pickup_latitude > 40.63900
                              | map_data$Dropoff_latitude > 40.63900))

map_data <- subset(map_data, (map_data$Pickup_longitude < -73.796883
                              |map_data$Dropoff_longitude < -73.796883))

map_data <- subset(map_data, (map_data$Pickup_longitude > -73.771305
                              | map_data$Dropoff_longitude > -73.771305))

# Count of transactions that fall within the coordinates of JFK airport

qplot(1:nrow(map_data), map_data$Fare_amount)


nrow(map_data)

summary(map_data)

summary(fresh_data)


# Data Modelling

# Build a derived variable for tip as a percentage of the total fare

# tip was calculated as a percent of the ratio of tip to total amount
fresh_data$Tip_percent <- 100*fresh_data$Tip_amount/fresh_data$Total_amount

# Creating a random sample of test and train data of equal no. of observations for our
# predictive models

set.seed(10)
test <- sample(1:nrow(fresh_data), 10000)
train <- sample(1:nrow(fresh_data), 10000)
train_set <- fresh_data[train,]           # Training set of data
test_set <- fresh_data[test,]             # Testing set of data


# test,train 2
# test <- (1:1000)
# train <- (1:1000)
# train_set <- fresh_data[train,]
# test_set <- fresh_data[test,]


# CROSS VALIDATION
# Load the caret package to train the training set by repeated 10-fold cross validation process
library(caret)
fitControl <- trainControl(method = "repeatedcv",  #Repeated Cross Validation
                           number = 10,            
                           repeats = 10,
                           classProbs = FALSE)




# random forest model
library(randomForest)


rfout <- randomForest(Tip_percent ~ Speed + Payment_type + Time_duration
                      + Total_amount + RateCodeID + Passenger_count + Trip_distance + Fare_amount
                      + improvement_surcharge + Trip_type ,
                      train_set,
                      ntree = 500,
                      trcontrol = fitControl )

rfout

#Predicting the importance of each explanatory variable vs the response variable
importance(rfout)

## Regression Models

# Model 1
# random forest model
rfout <- randomForest(Tip_percent ~ Speed + Payment_type + Time_duration
                      + Total_amount + Passenger_count + Trip_distance + Fare_amount
                      ,train_set
                      ,ntree = 500
                      ,trcontrol = fitControl )
rfout

# Plotting the error rate vs the number of tree
plot(rfout, main = "Error vs No. of Tree")

# Predicting the rf model
rfpredict <- predict(rfout, test_set)


# Model 2
# Linear model multivariate
lmout <- lm(Tip_percent ~ Speed + Payment_type + Time_duration
            + Total_amount + Passenger_count + Trip_distance + Fare_amount
            ,data =  train_set)
plot(lmout)

# Predicting the lm model
lmpredict <- predict(lmout
                     ,newdata = test_set
                     ,se.fit = TRUE
                     ,interval = "confidence"
                     ,level = 0.95)

summary(lmout)

# Using backward elimination to fit a better model
# Choose accordingly whose p-value is high and eliminate, I had encountered Speed,Passenger_count 
# and Time_duration the highest number of times.
lmout <- update(lmout,Tip_percent~. -Speed)
summary(lmout)
lmout <- update(lmout,~.-Passenger_count)
summary(lmout)
lmout <- update(lmout,~.-Time_duration)
summary(lmout)

# Aim is to find the best possible model that only includes significant factors.
# Its acheived by the process of backward elimination. We remove the insignificant 
# variables whose p-values (p-value>0.05 for 95% C.I.) are high and optimize the model

# Final Model Equation
# Tip_percent = 20.05 - 9.82*Payment_type + 1.61*Total_amount - 0.2786*Trip_distance
#               - 1.736*Fare_amount


# Model 3

# Recursive Partitioning and Regression Trees (rpart) model
library(rpart)
rpartout <- rpart(Tip_percent ~ Speed + Payment_type + Time_duration
                  + Total_amount + Passenger_count + Trip_distance + Fare_amount
                  , data = train_set
                  , method = "anova")


# Predicting rpart model
p1 <- predict(rpartout, test_set)


# Absolute Mean Error
MAE <- function(actual, predict){
  error <- abs(actual - predict)        
  return(mean(error))
}

MAE(test_set$Tip_percent,p1)

MAE(test_set$Tip_percent,rfpredict)

MAE(test_set$Tip_percent,lmpredict$fit)

# Root Mean squared error
RMSE <- function(actual, predict){
  sqrt(mean((actual - predict)^2))
}

RMSE(test_set$Tip_percent,p1)

RMSE(test_set$Tip_percent,rfpredict)

RMSE(test_set$Tip_percent,lmpredict$fit)




accuracy <- function(actual, predict){
  count = 0
  ncount = 0
  error = abs(actual - predict)
  for( i in 1:length(actual)){
    if (error[i] <= 2*sd(actual)) {count = count + 1}
    else( ncount = ncount + 1)}
  acc <- 100*count/(ncount + count)
  return(acc)
}

accuracy(test_set$Tip_percent,p1)

accuracy(test_set$Tip_percent,rfpredict)

accuracy(test_set$Tip_percent,lmpredict$fit)

# Question 5


## Building a derived varibale for avg speed over a ride.

# library chron should be pulled
library(chron)

# Variable
fresh_data$Speed <- fresh_data$Trip_distance/(fresh_data$Time_duration/60)

# Cleaning of data set, assuming that speeds above 200 mphr isn't realistic
fresh_data <- fresh_data[(fresh_data$Time_duration < 200 
                          & fresh_data$Time_duration > 1),]


# Mean speed per week
attach(fresh_data)

table1 <- aggregate(list(Mean_Speed = fresh_data$Speed), 
                    list(Week_of_month = cut(fresh_data$Date_Pickup, "7 days")), 
                    mean)

# Print Table of WEEK VS MEAN SPEED
table1

# Created small subsets to compare the avereage speeds per week
t_sample1 <- subset(fresh_data, 
                    fresh_data$Date_Pickup> "2015-09-01" 
                    & fresh_data$Date_Pickup <= "2015-09-08")

t_sample2 <- subset(fresh_data, 
                    fresh_data$Date_Pickup> "2015-09-08" 
                    & fresh_data$Date_Pickup <= "2015-09-15")

t_sample3 <- subset(fresh_data, 
                    fresh_data$Date_Pickup> "2015-09-15" 
                    & fresh_data$Date_Pickup <= "2015-09-21")

t_sample4 <- subset(fresh_data, 
                    fresh_data$Date_Pickup> "2015-09-21" 
                    & fresh_data$Date_Pickup <= "2015-09-28")

hist(t_sample1$Speed, breaks = 1000, xlim = c(0,50))

# T- test to check if the null hypothesis holds true

t.test(t_sample1$Speed, t_sample2$Speed
       , alternative = "two.sided"
       , mu = 0
       , paired = F
       , var.equal = FALSE
       , conf.level = 0.95)

# p-value is less than 0.05 thus suggesting that the null hypothesis does not hold true
# For one it could be the number of people moving in the city early september, that's
# when usually a new season/semester begins. Plus a holiday in early september can 
# disrupt the travel and traffic schedules in the week

# Another factor is correctness of the data provided, a lot of incorrect values may have
# not lead us in the right direction. 

# grouping by hour, mean trip speed 
fresh_data$Time_Pickup <- format(as.POSIXct(fresh_data$lpep_pickup_datetime)
                                 ,format = "%H:%M:%S")

table2 <- fresh_data[,list(mean = mean(Speed))
                     ,by = list(hour(as.POSIXct(fresh_data$Time_Pickup 
                                                ,format = "%H:%M:%S")))
                     ]
table2

# Plotting a graph for average trip speed as a function of time of day
plot(table2
     , type = 'o'
     , col = "red"
     , xlab = "Hour of the day"
     , ylab = "Avg trip speed"
     , main = "Avg Speed in a day")

# From the graph it is clear that the average speed reaches maximum at early morning
# One reason to build on this hypothesis is that traffic in the morning isn't that high
# thus giving clearer roads for drivers to drive through.
# One reason might be that people who have to travel long distances have to 
# pass a highway (where speeds are high usually) which 
# subsequently proves the higher speed range.
# Another point that can be noted here is that the speed at night isn't as high as
# early morning is because of the fact that on weekends, people like to stay out, thus 
# incresing the traffic on roads and keeping it lower than the peak. 
# The min value in the graph can be interpreted as leaving hours of people leaving the
# office and travelling at night, thus increasing the traffic and reducing the avg speed

