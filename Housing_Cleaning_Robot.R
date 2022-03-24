library(dplyr)
library(magrittr)
library(tidyverse)
library(e1071)
library(caTools)
library(class)


library(mice)

require(plyr)
robo_data <- read_csv("C:/Users/HP/Desktop/wox/R_ML/sensor_24.csv")

robo_data = na.omit(robo_data)
library(caret)

attach(robo_data)


library(neuralnet)

attach(robo_data)
scaledata = scale(robo_data)
norm = function(x){
  return((x-min(x))/max(x)-min(x))
}

unique(y)

minmax_df1 = as.data.frame(lapply(rel,norm))
train1 = robo_data[1:400,]
test1 = robo_data[400:545,]
names(robo_data)
nn = neuralnet(y~a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p,data = train1,rep=2,hidden = c(4,2),threshold = 0.01)
nn$result.matrix
plot(nn)

backprop = neuralnet(y~a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+w+x,data = train1,algorithm = "backprop",rep=1,hidden = c(10,8,4,2,1),threshold = 0.01,learningrate=0.0001)
plot(backprop)



validation_index <- createDataPartition(robo_data$y, p=0.80, list=FALSE)
validation <-robo_data[-validation_index,]
robo_data <- robo_data[validation_index,]
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"



#robo_data$y <- revalue(x = robo_data$y, c("Slight-Right-Turn"=0 ,"Sharp-Right-Turn"=1,  "Move-Forward"=2,     "Slight-Left-Turn"=3   ))

unique(y)


#Random forest

fit.rf <- train(y~., data=robo_data, method="rf", metric=metric, trControl=control)
fit.rf
predictions1 <- predict(fit.rf, validation)

#KNN
fit.knn <- train(y~., data=robo_data, method="knn", metric=metric, trControl=control)
fit.knn
predictions1 <- predict(fit.rf, validation)



#decision Tree
fit.rpart <- train(y~., data=robo_data, method="rpart", metric=metric, trControl=control)
fit.rpart
predictions1 <- predict(fit.rpart, validation)



#plot for all column with in data
matplot(robo_data, type = c("b"),pch=1,col = 2:10) #plot
legend("topleft", legend = 1:23, col=1:23, pch=1) # optional legend


