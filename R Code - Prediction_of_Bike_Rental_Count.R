#Clean the environment
rm(list = ls())

#Loading Libraries
libraries = c("plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest",
              "usdm","corrgram","DataCombine")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

library(dummies)
library(caret)
library(rpart.plot)

#Loading the data
df = read.csv("day.csv",header=T)

#first few rows of data
head(df)

#str names of data
str(df)
################# Exploratory Data Analysis ##############
df$dteday = as.Date(df$dteday)
df$season = as.factor(df$season)
df$yr = as.factor(df$yr)
df$mnth = as.factor(df$mnth)
df$holiday = as.factor(df$holiday)
df$weekday = as.factor(df$weekday)
df$workingday = as.factor(df$workingday)
df$weathersit = as.factor(df$weathersit)


################## Future Engineering #####################

#Creating a new variables 
df$actual_temp = df$temp * 39
df$actual_atemp = df$atemp * 50
df$actual_windspeed = df$windspeed * 67
df$actual_hum = df$hum * 100

df$actual_season = factor(x = df$season, levels = c(1,2,3,4), labels = c("Spring","Summer","Fall","Winter"))
df$actual_year = factor(x = df$yr, levels = c(0,1), labels = c("2011","2012"))
df$actual_workingday = factor(x = df$workingday, levels = c(0,1), labels = c("Holiday","Working day"))
df$actual_weathersit = factor(x = df$weathersit, levels = c(1,2,3,4), 
                               labels = c("Clear","Cloudy/Mist","Rain/Snow/Fog","Heavy Rain/Snow/Fog"))

################# Missing value analysis ##############

missing_val = sapply(df,function(x){sum(is.null(df))})
missing_val

#There is no missing values so will move forward to data distribution

############# Exploring data distribution by graphs ###############

#checking data distribution of categorical variables using bar graphs
bar_season = ggplot(data = df,aes(x = actual_season)) +
  geom_bar(fill = 'blue')+ggtitle("Count of Season")

bar_season

bar_weather = ggplot(data = df,aes(x = actual_weathersit)) +
  geom_bar(fill = 'blue')+ggtitle("Count of Weatherlist")

bar_weather

bar_workingday = ggplot(data = df,aes(x = actual_workingday)) +
  geom_bar(fill = 'blue')+ggtitle("Count of Workingday")

bar_workingday

gridExtra::grid.arrange(bar_season,bar_weather,bar_workingday,ncol=2)

#Checking data distribution numerical variables using histograms

hist_temp = ggplot(data = df, aes(x =actual_temp)) + ggtitle("Distribution of Temperature") + geom_histogram(bins = 25,fill='blue')
hist_atemp = ggplot(data = df, aes(x =actual_atemp)) + ggtitle("Distribution of feeled Temprature") + geom_histogram(bins = 25,fill='blue')
hist_windspeed = ggplot(data = df, aes(x =actual_windspeed)) + ggtitle("Distribution of Windspeed") + geom_histogram(bins = 25,fill='blue')
hist_hum = ggplot(data = df, aes(x =actual_hum)) + ggtitle("Distribution of Humidity") + geom_histogram(bins = 25,fill='blue')

gridExtra::grid.arrange(hist_temp,hist_atemp,hist_windspeed,hist_hum,ncol=2)

########### Outlier Analysis ################

continous_var = c("actual_temp","actual_atemp","actual_windspeed","actual_hum")

for (i in 1:length(continous_var))
{
  assign(paste0("gn",i), ggplot(aes_string(y = continous_var[i]), data = df)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=continous_var[i])+
           ggtitle(paste("Box plot for",continous_var[i])))
}
gridExtra::grid.arrange(gn1,gn3,gn2,gn4,ncol=2)

#Found that outliers in actual_windspeed and actual_hum
#Removing outliers in Windspeed

val = df[,19][df[,19] %in% boxplot.stats(df[,19])$out]
df = df[which(!df[,19] %in% val),]

#Removing outliers in humidity
val = df[,20][df[,20] %in% boxplot.stats(df[,20])$out]
df = df[which(!df[,20] %in% val),]

colnames(df)
######################## Future Selection ########################

#Checking multicollinearity using VIF

df_vif = df[,c('temp','atemp','windspeed','hum')]

vifcor(df_vif1)


#Checking Collinearity by using Correlation graph
corrgram(df, order = FALSE,lower.panel = panel.shade,upper.panel = panel.pie,
          text.panel = panel.txt, main="Correlation Graph")

#From above 2 Correlation analysis observed 'atemp' variable has multicollinearity problem
#Removing unwanted variables
df = subset(df,select = -c(holiday,instant,dteday,atemp,casual,registered,actual_temp,actual_atemp,actual_windspeed,
                           actual_hum,actual_season,actual_year,actual_workingday,actual_weathersit))

#Taking copy of data
df2 = df
#df = df2
colnames(df)

#Creating dummyvariables for categorical variables to trick the Regression models

catagorical_var = c('season','yr','mnth','weekday','workingday','weathersit')
df = dummy.data.frame(df, catagorical_var)
colnames(df)

rmExcept(keepers = 'df')

####################### Model Development ##########################

#Splitting data into train and test data
train_index = sample(1:nrow(df), 0.8*nrow(df))
train = df[train_index,]
test = df[-train_index,]

#-------------------Decision Tree----------------------#

#training the data with rpart
dt_model = rpart(cnt ~ ., data = train,method = "anova")
rpart.plot(dt_model)

#Predicting test data
dt_predictions = predict(dt_model,test[,-34])

#Create dataframe for actual and predicted values
df_pred = data.frame("actual"=test[,34], "pred"=dt_predictions)
head(df_pred)
summary(dt_model)

#Calculate RMSE and other error metrics
regr.eval(trues = test[,34], preds = dt_predictions, stats = c("mae","mse","rmse","mape"))

#Calculate R-Squared
print(postResample(pred = dt_predictions, obs = test[,34]))

# RMSE           Rsquared         MAE 
#961.0064188   0.7676469     736.2084616 

#--------------------Linear Regression---------------------# 

#Train the data using linear regression
lr_model = lm(formula = cnt~., data = train)

#Check the summary of the model
summary(lr_model)
          

#Predict the test cases
lr_predictions = predict(lr_model, test[,-34])

#Create dataframe for actual and predicted values
df_lin = cbind(df_pred,lr_predictions)
head(df_lin)

#Calculate RMSE and other error metrics
regr.eval(trues = test[,34], preds = lr_predictions, stats = c("mae","mse","rmse","mape"))

#Calculate R-Squared
print(postResample(pred = lr_predictions, obs = test[,34])) 

#  RMSE        Rsquared      MAE 
#847.8232089   0.8209717 629.3466018 


#--------------------------Random Forest-------------------# 

#Train the data using random forest
rf_model = randomForest(cnt~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-34])

#Create dataframe for actual and predicted values
df_lin = cbind(df_lin,rf_predictions)
head(df_lin)

#Calculate RMSE and other error metrics
regr.eval(trues = test[,34], preds = rf_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,34], rf_predictions)

#Calculate R-Squared
print(postResample(pred = rf_predictions, obs = test[,34]))

#      RMSE    Rsquared         MAE 
#693.8210337   0.8804323   512.0623566


#-------Hyperparameter Tuning ----------------#
 
# Tuning Random Forest

control <- trainControl(method="repeatedcv", number=10, repeats=3)
reg_fit <- caret::train(cnt~., data = train, method = "rf",trControl = control)
reg_fit$bestTune
y_pred <- predict(reg_fit, test[,-34])
print(caret::R2(y_pred, test[,34]))

