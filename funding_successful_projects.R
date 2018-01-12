#convert unix format to date - train
train$deadline_date <- as.POSIXct(as.numeric(train$deadline), origin = '1970-01-01', tz = 'GMT')
train$state_changed_date <- as.POSIXct(as.numeric(train$state_changed_at), origin = '1970-01-01', tz = 'GMT')
train$created_date <- as.POSIXct(as.numeric(train$created_at), origin = '1970-01-01', tz = 'GMT')
train$launched_date <- as.POSIXct(as.numeric(train$launched_at), origin = '1970-01-01', tz = 'GMT')
#convert unix format to date - test
test$deadline_date <- as.POSIXct(as.numeric(test$deadline), origin = '1970-01-01', tz = 'GMT')
test$state_changed_date <- as.POSIXct(as.numeric(test$state_changed_at), origin = '1970-01-01', tz = 'GMT')
test$created_date <- as.POSIXct(as.numeric(test$created_at), origin = '1970-01-01', tz = 'GMT')
test$launched_date <- as.POSIXct(as.numeric(test$launched_at), origin = '1970-01-01', tz = 'GMT')
#find number of days
train$date_diff <- as.Date(as.character(train$deadline_date), format="%Y-%m-%d")-
       as.Date(as.character(train$created_date), format="%Y-%m-%d")
test$date_diff <- as.Date(as.character(test$deadline_date), format="%Y-%m-%d")-
       as.Date(as.character(test$created_date), format="%Y-%m-%d")

#change currency,country to numbers
train$currency_num<-factor(train$currency,levels=sort(unique(train$currency)))
train$currency_num<-as.numeric(train$currency_num)

train$country_num<-factor(train$country,levels=sort(unique(train$country)))
train$country_num<-as.numeric(train$country_num)

test$currency_num<-factor(test$currency,levels=sort(unique(test$currency)))
test$currency_num<-as.numeric(test$currency_num)
test$country_num<-factor(test$country,levels=sort(unique(test$country)))
test$country_num<-as.numeric(test$country_num)
#change disable communication to 0 1
train$logical = as.logical(train$disable_communication)
train$logical_num = as.numeric(train$logical)
test$logical = as.logical(test$disable_communication)
test$logical_num = as.numeric(test$logical)
#build model to predict backers count using training file
model2 = lm(backers_count ~ goal+created_date+deadline_date+state_changed_date+launched_date+currency_num+country_num+logical_num+date_diff,data = train)
library(relaimpo)
varImp(model2, scale = FALSE)
summary(model2)
model2 = lm(backers_count ~ goal+created_date+deadline_date+state_changed_date+date_diff,data = train)
varImp(model2, scale = FALSE)
summary(model2)
#model1 = lm(backers_count ~ goal+deadline_date+launched_date+disable_communication,data = train)
#summary(model1)
#predict backers count in test file
test$predict_model1 = predict(model2,newdata = test)
#round the decimal values to a numeric value
test$backers_count = round(test$predict_model1)
#compute the final status using KNN

#train the model

#construct new training and testng set - numerical data
newtrain = data.frame(goal = train$goal,backers_count = train$backers_count,currency = train$currency_num,country = train$country_num)
newtest = data.frame(goal = test$goal,backers_count = test$backers_count,currency = test$currency_num,country = test$country_num)

#apply knn
#decide the value of k
library(class)
test_final_status = knn(train = newtrain, test = newtest, cl= train$final_status,k = 17)
test$final_status = test_final_status











