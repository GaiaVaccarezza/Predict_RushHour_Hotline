#import modified dataset with month day hour weekday day_year(1-365)
train <- read.csv("C:/Users/39346/OneDrive/Desktop/train__complete_time.csv")
train_10000 <- train[train$y<10000,]
train_1300 <- train[train$y <1300, ]

#rescaling
install.packages("car")
library(car)
install.packages("forecast")
library(forecast)

###GRAPHICAL ANALYSIS
y <- train$y
hist(y)
y_log <- log(y)
hist(y_log)
y_sqrt <- sqrt(y)
hist(y_sqrt)
boxCox(y~1, family="yjPower", plotit = TRUE)

y_yeo <- yjPower(y, 0.15)
hist(y_yeo)
y_yeo <- yjPower(train$y, 0.15)
hist(y_yeo)
y_yeo <- yjPower(y, 0.09)
hist(y_yeo)
y_yeo1 <- yjPower(train$y, 0.065)
hist(y_yeo1)
y_yeo <- yjPower(train$y, 0.055)
par(mfrow=c(1,2))
train$y_yeo <- yjPower(train$y, 0.06) 
hist(train$y_yeo) 
train$y_yeo1 <- yjPower(train$y, 0.09)
hist(train$y_yeo1)
train$y_yeo2 <- yjPower(train$y, 0.1)
hist(train$_yeo2)

y1 <- train_10000$y
hist(y1)
y_log1 <- log(y1)
hist(y_log1)
y_sqrt1 <- sqrt(y1)
hist(y_sqrt1)
y_yeo1 <- yjPower(y1, 0.1)
hist(y_yeo1)
y_yeo1 <- yjPower(y1, 0.075)
hist(y_yeo1)
y_yeo1 <- yjPower(y1, 0.065)
hist(y_yeo1)
train_10000$y_yeo1 <- yjPower(train_10000$y, 0.055)
hist(train_10000$y_yeo1) #first try
a <- boxCox(y1~1, family="yjPower", plotit = TRUE)


y2 <- train_1300$y
hist(y2)
y_log2 <- log(y2)
hist(y_log2)
y_sqrt1 <- sqrt(y1)
hist(y_sqrt1)
y_yeo2 <- yjPower(y2, 0.15)
hist(y_yeo2)

#to transform back 
install.packages("VGAM")
library("VGAM")
y_yeo_try <- yeo.johnson(y1, 0.055)
hist(y_yeo_try)
y_try <- yeo.johnson(y_yeo_try, 0.055, inverse=TRUE)
hist(y_try)

##DATASET CREATION
#introduction of dummies
install.packages("fastDummies") 
library(fastDummies)
train  <- dummy_cols(train , select_columns = "operator")
train  <- dummy_cols(train , select_columns = "customer")
train  <- dummy_cols(train , select_columns = "product")
train  <- dummy_cols(train , select_columns = "work_group")
train  <- dummy_cols(train , select_columns = "priority")
train  <- dummy_cols(train , select_columns = "channel")
train  <- dummy_cols(train , select_columns = "type")
train  <- dummy_cols(train , select_columns = "service_mode")
train  <- dummy_cols(train , select_columns = "service_center")
train  <- dummy_cols(train , select_columns = "sla_contract")
train  <- dummy_cols(train , select_columns = "month")
train  <- dummy_cols(train , select_columns = "hour")


##STATICAL ANALYSIS 
#REGRESSION TREES
install.packages("tree")
library(tree)

#product 
tree = tree(y~ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 , data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0) # y 1 

tree = tree(y_yeo~ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 , data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0) #yeo 2 41

tree = tree(log(y)~ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 , data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0)#log 2 41 

#work_group 
tree = tree(y~ work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 , data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0)  #y 1          
              
tree = tree(y_yeo~ work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 , data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0)  #yeo 3 29-131 

tree = tree(log(y)~ work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 , data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0) #log 2 29 

#customer 
tree = tree(y~ customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + customer_3950, data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0) #y 1 10674 

tree = tree(y_yeo~ customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + customer_3950, data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0) #6 y_yeo 5622 5709 3758 3752 10467

tree = tree(log(y)~ customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + customer_3950, data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0) #6 log 5622 5709 3758 3752 10467

#priority 
tree = tree(y~ priority, data=train) 
summary(tree) #1

tree = tree(y_yeo~ priority, data=train) 
summary(tree) #1

tree = tree(log(y)~ priority, data=train) 
summary(tree) #1

#type 
tree = tree(y~ as.factor(type), data=train) 
summary(tree) 
plot(tree)
text(tree, pretty = 0) #y 2 

tree = tree(y_yeo~ type_3 + type_5 + type_20 + type_9 +type_17, data=train) 
summary(tree) #yeo 1 

tree = tree(log(y)~ type_3 + type_5 + type_20 + type_9 +type_17, data=train) 
summary(tree) #log 1 

#channel 
tree = tree(y~ channel_2 + channel_5, data=train) 
summary(tree) #y 1

tree = tree(y_yeo~ channel_2 + channel_5, data=train) 
summary(tree)
plot(tree)
text(tree, pretty = 0) #yeo 2 2 

tree = tree(log(y)~ channel_2 + channel_5, data=train) 
summary(tree)
plot(tree)
text(tree, pretty = 0) #log 2 2 

#operator 
tree = tree(y~ operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 , data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0) #y 1 node

tree = tree(y_yeo ~ operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 , data = train)
summary(tree) 
plot(tree)
text(tree, pretty = 0) #yeo 2 node (155)

tree = tree(log(y)~ operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 , data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0) #log 3 node (155, 911)

#service_center 
tree = tree(y~ service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15, data = train ) 
summary(tree) #y 1    

tree = tree(y_yeo~ service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15, data = train ) 
summary(tree)
plot(tree)
text(tree, pretty = 0) #yeo 2 13 

tree = tree(log(y)~ service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15, data = train ) 
summary(tree)
plot(tree)
text(tree, pretty = 0) #log 2 13

#service mode 
tree = tree(y~ service_mode_2 + service_mode_3, data = train)
summary(tree) #1

tree = tree(y_yeo~ service_mode_2 + service_mode_3, data = train)
summary(tree) #1

tree = tree(log(y)~ service_mode_2 + service_mode_3, data = train)
summary(tree) #1

#sla_contract

#forwarded

#month
tree = tree(y~ month_1 + month_2 + month_4 + month_12 + month_6, data = train)
summary(tree)

tree = tree(y_yeo~ month_1 + month_2 + month_4 + month_12 + month_6, data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0) #yeo 2 1 

tree = tree(log(y)~ month_1 + month_2 + month_4 + month_12 + month_6, data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0) #log 2 1 

#hour 
tree = tree(y ~ hour_13 + hour_18 + hour_19 + hour_11 , data = train)
summary(tree)

tree = tree(y_yeo ~ hour_13 + hour_18 + hour_19 + hour_11 , data = train)
summary(tree)

tree = tree(log(y) ~ hour_13 + hour_18 + hour_19 + hour_11 , data = train)
summary(tree)

#total tree
tree = tree(y ~ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + customer_3950 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour_13 + hour_18 + hour_19 + hour_11 + month_1 + month_2 + month_4 + month_12 + month_6 + service_mode_2 + service_mode_3, data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0) #y 2 customer_10674

tree = tree(y_yeo ~ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + customer_3950 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour_13 + hour_18 + hour_19 + hour_11 + month_1 + month_2 + month_4 + month_12 + month_6 + service_mode_2 + service_mode_3, data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0) #yeo channel_2 sla_contract operator_2331 service_center13 

tree = tree(log(y) ~ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + customer_3950 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour_13 + hour_18 + hour_19 + hour_11 + month_1 + month_2 + month_4 + month_12 + month_6 + service_mode_2 + service_mode_3, data = train)
summary(tree)
plot(tree)
text(tree, pretty = 0) #log 4 channel_2 service_center8 sla_contract 

##RANDOM FOREST
install.packages("randomForest")
library(randomForest)

rf = randomForest(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + customer_3950 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour_13 + hour_18 + hour_19 + hour_11 + month_1 + month_2 + month_4 + month_12 + month_6 + service_mode_2 + service_mode_3, data = train, ntree=100, importance=TRUE)
rf #day_year channel_2 service_center13 sla_contract hour11 customer_5622
importance(rf) 

##BOOSTING
install.packages("gbm")
library("gbm")

boost = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + customer_3950 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train, distribution =  "gaussian", n.trees=500, interaction.depth=3 ) 
summary(boost)

boost = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + customer_3950 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train, distribution =  "gaussian", n.trees=500, interaction.depth=3 ) 
summary(boost) 

boost = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train, distribution =  "gaussian", n.trees=5000, interaction.depth=3 ) 
summary(boost) 

boost = gbm(log(y) ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train, distribution =  "gaussian", n.trees=5000, interaction.depth=3 ) 
summary(boost) 


#assuming that the dataset in a randomic order i split it in 5 folds (17995/5=3599) and so create 5 train set 
test1 = train[train$X<3601,]
test2 = train[3600<train$X & train$X<7201,]
test3 = train[7200<train$X & train$X<10801,]
test4 = train[10800<train$X & train$X<14401,]
test5 = train[14400<train$X,] #see than some a missing info 
train1 = train[train$X<14401,] #without 5 fold
train2 = train[train$X<10801 | 14400<train$X,] #without 4 fold 
train3 = train[train$X<7201 | 10800<train$X,] #without 3
train4 = train[train$X<3601 | 7200<train$X,] #without 2 
train5 = train[train$X> 3600, ] #without1
  
#yeo 0.06 all data all variable n.trees=1000, interaction.depth=1
#hour and month together
x <- c(1:5)
boost1 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train1, distribution =  "gaussian", n.trees=1000, interaction.depth=1 ) 
summary(boost1)
pred1 = predict(boost1, newdata=test5)
scale1 <- yeo.johnson(pred1, 0.06, inverse=TRUE)
mean1 = mean((scale1-test5$y)^2)
mean1

boost2 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train2, distribution =  "gaussian", n.trees=1000, interaction.depth=1 ) 
summary(boost2)
pred2 = predict(boost2, newdata=test4)
scale2 <- yeo.johnson(pred2, 0.06, inverse=TRUE)
mean2 = mean((scale2-test4$y)^2)
mean2

boost3 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train3, distribution =  "gaussian", n.trees=1000, interaction.depth=1 ) 
summary(boost3)
pred3 = predict(boost3, newdata=test3)
scale3 <- yeo.johnson(pred3, 0.06, inverse=TRUE)
mean3 = mean((scale3-test3$y)^2)
mean3

boost4 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train4, distribution =  "gaussian", n.trees=1000, interaction.depth=1 ) 
summary(boost4)
pred4 = predict(boost4, newdata=test2)
scale4 <- yeo.johnson(pred4, 0.06, inverse=TRUE)
mean4 = mean((scale4-test2$y)^2)
mean4

boost5 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train5, distribution =  "gaussian", n.trees=1000, interaction.depth=1 ) 
summary(boost5)
pred5 = predict(boost5, newdata=test1)
scale5 <- yeo.johnson(pred5, 0.06, inverse=TRUE)
mean5 = mean((scale5-test1$y)^2)
mean5

a = mean1 + mean2 + mean3 +  mean4 + mean5
a/5


#yeo 0.06 all data all variable n.trees=1000, interaction.depth=2
#hour and month together
x <- c(1:5)
boost1 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train1, distribution =  "gaussian", n.trees=1000, interaction.depth=2 ) 
summary(boost1)
pred1 = predict(boost1, newdata=test5)
scale1 <- yeo.johnson(pred1, 0.06, inverse=TRUE)
mean1 = mean((scale1-test5$y)^2)
mean1

boost2 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train2, distribution =  "gaussian", n.trees=1000, interaction.depth=2 ) 
summary(boost2)
pred2 = predict(boost2, newdata=test4)
scale2 <- yeo.johnson(pred2, 0.06, inverse=TRUE)
mean2 = mean((scale2-test4$y)^2)
mean2

boost3 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train3, distribution =  "gaussian", n.trees=1000, interaction.depth=2 ) 
summary(boost3)
pred3 = predict(boost3, newdata=test3)
scale3 <- yeo.johnson(pred3, 0.06, inverse=TRUE)
mean3 = mean((scale3-test3$y)^2)
mean3

boost4 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train4, distribution =  "gaussian", n.trees=1000, interaction.depth=2 ) 
summary(boost4)
pred4 = predict(boost4, newdata=test2)
scale4 <- yeo.johnson(pred4, 0.06, inverse=TRUE)
mean4 = mean((scale4-test2$y)^2)
mean4

boost5 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train5, distribution =  "gaussian", n.trees=1000, interaction.depth=2 ) 
summary(boost5)
pred5 = predict(boost5, newdata=test1)
scale5 <- yeo.johnson(pred5, 0.06, inverse=TRUE)
mean5 = mean((scale5-test1$y)^2)
mean5

a = mean1 + mean2 + mean3 +  mean4 + mean5
a/5

#yeo 0.06 all data all variable n.trees=1000 interaction.depth=3
#hour and month together
boost1 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train1, distribution =  "gaussian", n.trees=1000, interaction.depth=3 ) 
summary(boost1)
pred1 = predict(boost1, newdata=test5)
scale1 <- yeo.johnson(pred1, 0.06, inverse=TRUE)
mean1 = mean((scale1-test5$y)^2)
mean1

boost2 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train2, distribution =  "gaussian", n.trees=1000, interaction.depth=3 ) 
summary(boost2)
pred2 = predict(boost2, newdata=test4)
scale2 <- yeo.johnson(pred2, 0.06, inverse=TRUE)
mean2 = mean((scale2-test4$y)^2)
mean2

boost3 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train3, distribution =  "gaussian", n.trees=1000, interaction.depth=3 ) 
summary(boost3)
pred3 = predict(boost3, newdata=test3)
scale3 <- yeo.johnson(pred3, 0.06, inverse=TRUE)
mean3 = mean((scale3-test3$y)^2)
mean3

boost4 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train4, distribution =  "gaussian", n.trees=1000, interaction.depth=3 ) 
summary(boost4)
pred4 = predict(boost4, newdata=test2)
scale4 <- yeo.johnson(pred4, 0.06, inverse=TRUE)
mean4 = mean((scale4-test2$y)^2)
mean4

boost5 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train5, distribution =  "gaussian", n.trees=1000, interaction.depth=3 ) 
summary(boost5)
pred5 = predict(boost5, newdata=test1)
scale5 <- yeo.johnson(pred5, 0.06, inverse=TRUE)
mean5 = mean((scale5-test1$y)^2)
mean5

a = mean1 + mean2 + mean3 +  mean4 + mean5
a/5


#yeo 0.06 all data all variable n.trees=3000 interaction.depth=3
#hour and month together
boost1 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train1, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost1)
pred1 = predict(boost1, newdata=test5)
scale1 <- yeo.johnson(pred1, 0.06, inverse=TRUE)
mean1 = mean((scale1-test5$y)^2)
mean1

boost2 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train2, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost2)
pred2 = predict(boost2, newdata=test4)
scale2 <- yeo.johnson(pred2, 0.06, inverse=TRUE)
mean2 = mean((scale2-test4$y)^2)
mean2

boost3 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train3, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost3)
pred3 = predict(boost3, newdata=test3)
scale3 <- yeo.johnson(pred3, 0.06, inverse=TRUE)
mean3 = mean((scale3-test3$y)^2)
mean3

boost4 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train4, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost4)
pred4 = predict(boost4, newdata=test2)
scale4 <- yeo.johnson(pred4, 0.06, inverse=TRUE)
mean4 = mean((scale4-test2$y)^2)
mean4

boost5 = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train5, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost5)
pred5 = predict(boost5, newdata=test1)
scale5 <- yeo.johnson(pred5, 0.06, inverse=TRUE)
mean5 = mean((scale5-test1$y)^2)
mean5

a = mean1 + mean2 + mean3 +  mean4 + mean5
a/5


#yeo 0.09 all data all variable n.trees=3000 interaction.depth=3
#hour and month together
boost1 = gbm(y_yeo1 ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train1, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost1)
pred1 = predict(boost1, newdata=test5)
scale1 <- yeo.johnson(pred1, 0.06, inverse=TRUE)
mean1 = mean((scale1-test5$y)^2)
mean1

boost2 = gbm(y_yeo1 ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train2, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost2)
pred2 = predict(boost2, newdata=test4)
scale2 <- yeo.johnson(pred2, 0.06, inverse=TRUE)
mean2 = mean((scale2-test4$y)^2)
mean2

boost3 = gbm(y_yeo1 ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train3, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost3)
pred3 = predict(boost3, newdata=test3)
scale3 <- yeo.johnson(pred3, 0.06, inverse=TRUE)
mean3 = mean((scale3-test3$y)^2)
mean3

boost4 = gbm(y_yeo1 ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train4, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost4)
pred4 = predict(boost4, newdata=test2)
scale4 <- yeo.johnson(pred4, 0.06, inverse=TRUE)
mean4 = mean((scale4-test2$y)^2)
mean4

boost5 = gbm(y_yeo1 ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train5, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost5)
pred5 = predict(boost5, newdata=test1)
scale5 <- yeo.johnson(pred5, 0.06, inverse=TRUE)
mean5 = mean((scale5-test1$y)^2)
mean5

a = mean1 + mean2 + mean3 +  mean4 + mean5
a/5

#yeo1 0.09  all data all variable n.trees=1000 interaction.depth=4 shrinkage 0.01
boost = gbm(y_yeo ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train, distribution =  "gaussian", n.trees=1000, interaction.depth=4, shrinkage= 0.01 ) 
summary(boost)
pred = predict(boost, newdata=test)
ciao <- yeo.johnson(pred5, 0.09, inverse=TRUE)
ciao<- yeo.johnson(pred, 0.09, inverse=TRUE)
hist(ciao)
setwd("C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE")
write.table(ciao, file="C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE/boost_yeo09_4level_s001.txt", append = TRUE, sep = "\n", dec = ".", row.names = FALSE, col.names= FALSE)


fh

#yeo 1 con depth 3 e 5000 3 MENO VAR - 
boost = gbm(y_yeo1 ~ day_year + product_203 + product_298 + product_41 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53  + work_group_121 + work_group_24 + work_group_50  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_1 + customer_10674 + customer_927  + customer_918 + priority + type_3 + channel_2 + channel_5 + operator_155 + operator_911  + operator_2331+ operator_1213 + operator_1223 +  operator_2744 + operator_200 + operator_2742 + + operator_1218 + operator_20 + operator_4804 + operator_225 + service_center_13 + service_center_8 + service_center_19 + service_center_3 + service_center_7+ forwarded + sla_contract + hour  + service_mode_2 + service_mode_3, data = train, distribution =  "gaussian", n.trees=5000, interaction.depth=3 ) 
summary(boost)
pred = predict(boost, newdata=test)
ciao<- yeo.johnson(pred, 0.09, inverse=TRUE)
hist(ciao)
setwd("C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE")
write.table(ciao, file="C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE/boost_yeo09_lessvar.txt", append = TRUE, sep = "\n", dec = ".", row.names = FALSE, col.names= FALSE)


#log all data all variable n.trees=3000 interaction.depth=3
#hour and month together
boost1 = gbm(log(y) ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train1, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost1)
pred1 = predict(boost1, newdata=test5)
scale1 = exp(pred1)
mean1 = mean((scale1-test5$y)^2)
mean1

boost2 = gbm(log(y) ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train2, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost2)
pred2 = predict(boost2, newdata=test4)
scale2 = exp(pred2)
mean2 = mean((scale2-test4$y)^2)
mean2

boost3 = gbm(log(y) ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train3, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost3)
pred3 = predict(boost3, newdata=test3)
scale3 = exp(pred3)
mean3 = mean((scale3-test3$y)^2)
mean3

boost4 = gbm(log(y) ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train4, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost4)
pred4 = predict(boost4, newdata=test2)
scale4 <- exp(pred4)
mean4 = mean((scale4-test2$y)^2)
mean4

boost5 = gbm(log(y) ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train5, distribution =  "gaussian", n.trees=3000, interaction.depth=3 ) 
summary(boost5)
pred5 = predict(boost5, newdata=test1)
scale5 <- exp(pred5)
mean5 = mean((scale5-test1$y)^2)
mean5

a = mean1 + mean2 + mean3 +  mean4 + mean5
a/5

#random forest
install.packages("randomForest")
library(randomForest)
rf = randomForest(y_yeo2 ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + service_mode_2 + service_mode_3, data = train, ntree = 100, importance =TRUE ) 
importance(rf)
rf

rf = randomForest(y_yeo2 ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + service_mode_2 + service_mode_3, data = train,ntree = 300,  importance =TRUE ) 
importance(rf)
rf

rf = randomForest(y_yeo2 ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + service_mode_2 + service_mode_3, data = train, ntree=500, importance =TRUE ) 
importance(rf)
rf

#assuming that the dataset in a randomic order i split it in 5 folds (17995/5=3599) and so create 5 train set 
test1 = train[train$X<3601,]
test2 = train[3600<train$X & train$X<7201,]
test3 = train[7200<train$X & train$X<10801,]
test4 = train[10800<train$X & train$X<14401,]
test5 = train[14400<train$X,] #see than some a missing info 
train1 = train[train$X<14401,] #without 5 fold
train2 = train[train$X<10801 | 14400<train$X,] #without 4 fold 
train3 = train[train$X<7201 | 10800<train$X,] #without 3
train4 = train[train$X<3601 | 7200<train$X,] #without 2 
train5 = train[train$X> 3600, ] #without1


  
#prediction
test <- read.csv("C:/Users/39346/OneDrive/Desktop/test (1).csv")
test <- dummy_cols(test, select_columns = "operator")
test <- dummy_cols(test, select_columns = "customer")
test <- dummy_cols(test, select_columns = "product")
test <- dummy_cols(test, select_columns = "work_group")
test<- dummy_cols(test, select_columns = "priority")
test<- dummy_cols(test, select_columns = "type")
test<- dummy_cols(test, select_columns = "channel")
test<- dummy_cols(test, select_columns = "service_center")
test <- dummy_cols(test, select_columns = "service_mode")
test <- dummy_cols(test, select_columns = "sla_contract")

pred = predict(boost, newdata=test)
install.packages("VGAM")
library("VGAM")
ciao<- yeo.johnson(pred, 0.06, inverse=TRUE)
ciao<- yeo.johnson(pred, 0.09, inverse=TRUE)
hist(ciao)
setwd("C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE")
write.table(ciao, file="C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE/boost_yeo09.txt", append = TRUE, sep = "\n", dec = ".", row.names = FALSE, col.names= FALSE)


#try with <10000
boost = gbm(y_yeo1 ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train_10000, distribution =  "gaussian", n.trees=5000, interaction.depth=3 ) 
summary(boost) #channel2 day_year sla_contract service_center13 operator_2331 hour work_group_121 service_center_121 service_center_8

boost = gbm(log(y) ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train, distribution =  "gaussian", n.trees=5000, interaction.depth=3 ) 
summary(boost) #channel2 day_year sla_contract service_center13 operator_2331 hour work_group_121 service_center_121 service_center_8

pred = predict(boost, newdata=test)
ciao<- yeo.johnson(pred, 0.055, inverse=TRUE)
hist(ciao)
setwd("C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE")
write.table(ciao, file="C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE/boost10000.txt", append = TRUE, sep = "\n", dec = ".", row.names = FALSE, col.names= FALSE)

##SHRINKAGE
#shrinkage 0.001 livelli 4 tree 3000 yeo 0.9 
boost = gbm(y_yeo1 ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train, distribution =  "gaussian", n.trees=3000, interaction.depth=4) 
summary(boost)
pred = predict(boost, newdata=test)
ciao<- yeo.johnson(pred, 0.09, inverse=TRUE)
hist(ciao)
setwd("C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE")
write.table(ciao, file="C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE/boost_6.txt", append = TRUE, sep = "\n", dec = ".", row.names = FALSE, col.names= FALSE)

#shrinkage 0.01 livelli 4 tree 3000 yeo 0.9 
boost = gbm(y_yeo1 ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train, distribution =  "gaussian", shrinkage=0.01, n.trees=3000, interaction.depth=4) 
summary(boost)
pred = predict(boost, newdata=test)
ciao<- yeo.johnson(pred, 0.09, inverse=TRUE)
setwd("C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE")
write.table(ciao, file="C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE/boost_7.txt", append = TRUE, sep = "\n", dec = ".", row.names = FALSE, col.names= FALSE)

#shrinkage 0.01 livelli 4 tree 00 yeo 0.9 
boost = gbm(y_yeo1 ~ day_year+ product_313 + product_203 + product_298 + product_347 + product_457 + product_41 + product_12 + product_221 + product_158 + product_50 + product_27 + product_679 + product_956 + product_168 + work_group_131 + work_group_35 + work_group_29 + work_group_7 + work_group_31 + work_group_53 + work_group_27 + work_group_26 + work_group_22 + work_group_121 + work_group_24 + work_group_50 + work_group_20  + work_group_24  + work_group_33 + customer_5622 + customer_3752 + customer_5709 + customer_3758 + customer_10467 + customer_3752 + customer_10853 + customer_3032 + customer_1+ customer_8912 + customer_771 + customer_10674 + customer_927 + customer_888 + customer_918 + customer_3178 + priority + type_3 + type_5 + type_20 + type_9 +type_17 + channel_2 + channel_5 + operator_155 + operator_911 + operator_3652 + operator_4174 + operator_899 + operator_1941 + operator_2331+ operator_1213 + operator_1223 + operator_4220 + operator_910 + operator_1223 +operator_1565 + operator_2331 + operator_2744 + operator_200 + operator_2742 + operator_4198 + operator_2742 + operator_1218 + operator_4158 + operator_4828 + operator_20 + operator_4804 + operator_225 + service_center_11 + service_center_13 + service_center_8 + service_center_19 + service_center_1 + service_center_3 + service_center_7 + service_center_15 + forwarded + sla_contract + hour + month + service_mode_2 + service_mode_3, data = train, distribution =  "gaussian", shrinkage=0.01, n.trees=200, interaction.depth=4) 
summary(boost)
pred = predict(boost, newdata=test)
ciao<- yeo.johnson(pred, 0.09, inverse=TRUE)
setwd("C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE")
write.table(ciao, file="C:/Users/39346/OneDrive/Desktop/MACHINE LEARNING/FINAL DATA CHALLENGE/boost_8.txt", append = TRUE, sep = "\n", dec = ".", row.names = FALSE, col.names= FALSE)

