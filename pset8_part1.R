
#read two datasets
hist_data <- read.csv("~/Desktop/Dartboard_historical.csv")
futu_data <- read.csv("~/Desktop/Dartboard_future.csv")
str(hist_data)

#make season a factor
hist_data$Season <- as.factor(hist_data$Season)
futu_data$Season <- as.factor(futu_data$Season)

#split hist_data dataset
train <- subset(hist_data, Year <= 2013)
test <- subset(hist_data, Year > 2013)

#fit regression model
model1 <- lm(log10(Sales/Population) ~ log10(Income) + Week_Num + Season, data = train)
summary(model1)

#plot coefs, maybe this part is not necessary
library(arm)
library(jtools)
coefplot(model1)
plot_coefs(model1)

#model performance
#make predictions
pred <- predict(model1, newdata = test)

#transform predictions into sales
pred_sales <- 10^(pred)*test$Income
#compute MAPE

library(MLmetrics)
MAPE(pred_sales, test$Sales)
mean(abs((test$Sales-pred_sales)/test$Sales) * 100)

#predict future sales
futu_data$Sales <- 10^(predict(model1, newdata = futu_data))*futu_data$Income

#find aggregate Sales
#total online retail sales in the Northeast region from mid-2015 to end-2017
sum(futu_data$Sales)

#total online retail sales in the Northeast region in the last 8 weeks of 2017
library(data.table)
sum(futu_data$Sales[futu_data$Year == 2017 & futu_data$Week %between% c(46,52)])

#total online retail sales in Suffolk County, MA, in the last 8weeks of 2017
sum(futu_data$Sales[futu_data$Year == 2017 & futu_data$Week %between% c(46,52) &
                    futu_data$County.Name == "Suffolk County" & futu_data$State.Name == "Massachusetts"])


#save futu_data dataset for use in next question
write.csv(futu_data, file = "Dartboard_future_final.csv")

