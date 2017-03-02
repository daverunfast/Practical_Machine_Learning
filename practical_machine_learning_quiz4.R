#1 Load the vowel.train and vowel.test data sets:

	library(ElemStatLearn)
	data(vowel.train)
	data(vowel.test)
	
	#Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. Fit (1) a random forest predictor relating the factor variable y to the remaining variables and (2) a boosted predictor using the "gbm" method. Fit these both with the train() command in the caret package.
	vowel.train$y<- as.factor(vowel.train$y)
	vowel.test$y<- as.factor(vowel.test$y)
	
	set.seed(33833)
	modelFit_RF<- train(y~.,data=vowel.train,method="rf")
	modelFit_GBM<- train(y~.,data=vowel.train,method="gbm")
	predict_RF <- predict(modelFit_RF, vowel.test)
	predict_GBM <- predict(modelFit_GBM, vowel.test)

	#What are the accuracies for the two approaches on the test data set? What is the accuracy among the test set samples where the two methods agree?
	confusionMatrix(predict_RF, vowel.test$y)$overall[1]
	# Accuracy 
	#0.6147186 
	
	confusionMatrix(predict_GBM, vowel.test$y)$overall[1]
	#Accuracy 
	#0.521645
	
	agree <- (predict_RF == predict_GBM)
	confusionMatrix(vowel.test$y[agree], predict_RF[agree])$overall[1]
	#Accuracy 
	#0.6450617 
	

#2 Load the Alzheimer's data using the following commands
	library(caret)
	library(gbm)
	set.seed(3433)
	library(AppliedPredictiveModeling)
	data(AlzheimerDisease)
	adData = data.frame(diagnosis,predictors)
	inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
	training = adData[ inTrain,]
	testing = adData[-inTrain,]
	
	# Set the seed to 62433 and predict diagnosis with all the other variables using a random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis ("lda") model. Stack the predictions together using random forests ("rf"). What is the resulting accuracy on the test set? Is it better or worse than each of the individual predictions?
	set.seed(62433)
	modelFit_rf<- train(diagnosis~.,data=training,method="rf")
	modelFit_gbm<- train(diagnosis~.,data=training,method="gbm")
	modelFit_lda<- train(diagnosis~.,data=training,method="lda")
	predict_rf <- predict(modelFit_rf, testing)
	predict_gbm <- predict(modelFit_gbm, testing)
	predict_lda <- predict(modelFit_lda, testing)
	
	#stack predictions together
	stack_pred = data.frame(predict_rf,predict_gbm,predict_lda,diagnosis = testing$diagnosis)
	
	# training a model on the stack using rf
	combinedFit <- train(diagnosis~., data=stack_pred, method = "rf")
	predict_combined <- predict(combinedFit, testing)
	
	confusionMatrix(predict_rf, testing$diagnosis)$overall[1]
	#Accuracy 
	#0.7804878 
	confusionMatrix(predict_gbm, testing$diagnosis)$overall[1]
	#Accuracy 
	#0.7926829 
	confusionMatrix(predict_lda, testing$diagnosis)$overall[1]
	#Accuracy 
	#0.7682927 
	confusionMatrix(predict_combined, testing$diagnosis)$overall[1]
	#Accuracy 
	#0.804878 

#3 Load the concrete data with the commands:
	set.seed(3523)
	library(AppliedPredictiveModeling)
	data(concrete)
	inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
	training = concrete[ inTrain,]
	testing = concrete[-inTrain,]
	# Set the seed to 233 and fit a lasso model to predict Compressive Strength. Which variable is the last coefficient to be set to zero as the penalty increases? (Hint: it may be useful to look up ?plot.enet).
	
	set.seed(233)
	fit_lasso = train(CompressiveStrength~.,data = training, method = "lasso")
	plot.enet(fit_lasso$finalModel,xvar="penalty", use.color=TRUE)
	fit_lasso$finalModel$beta.pure
	
	
	
#4 Load the data on the number of visitors to the instructors blog from here:

	https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv

	setwd( "C:/R/Practical Machine Learning")
	library(lubridate) # For year() function below
	dat = read.csv("gaData.csv")
	training = dat[year(dat$date) < 2012,]
	testing = dat[(year(dat$date)) > 2011,]
	tstrain = ts(training$visitsTumblr)
	#Fit a model using the bats() function in the forecast package to the training time series. Then forecast this model for the remaining time points. For how many of the testing points is the true value within the 95% prediction interval bounds?

	library(forecast)
	# build a bats model based on the original time series
	visits.exp.smoothing = bats(tstrain)
	# build the forecast with the same range as the testing set (2012)
	visits.forecast = forecast(visits.exp.smoothing, nrow(testing))
	# plot the forecast
	plot(visits.forecast)

	# extracting the 95% prediction boundaries
	visits.forecast.lower95 = visits.forecast$lower[,2]
	visits.forecast.upper95 = visits.forecast$upper[,2]

	# see how many of the testing visit counts do actually match
	table ((testing$visitsTumblr>visits.forecast.lower95) & (testing$visitsTumblr<visits.forecast.upper95))

	# FALSE  TRUE 
	#     9   226

	# percentages
	226/nrow(testing)
	# 0.9617021
	
	
	
	
#5 

	set.seed(3523)
	library(AppliedPredictiveModeling)
	data(concrete)
	inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
	training = concrete[ inTrain,]
	testing = concrete[-inTrain,]

	#Set the seed to 325 and fit a support vector machine using the e1071 package to predict Compressive Strength using the default settings. Predict on the testing set. What is the RMSE?

	#install.packages("e1071")
	library(e1071)
	set.seed(325)
	conc.fit.svm = svm(CompressiveStrength ~ .,data=training)
	# comparing predictions to actual values
	conc.pred.svm = predict(conc.fit.svm, newdata = testing)

	# Root Mean Squared Error
	error = conc.pred.svm - testing$CompressiveStrength
	sqrt(mean(error^2))
	## [1] 6.715009
	# plot the relationship between the forecasted svm values and the actual values, coloured by Age
	plot(conc.pred.svm, testing$CompressiveStrength, 
				  pch=20, cex=1, 
				  col=testing$Age,
				  main="Relationship between the svm forecast and actual values")