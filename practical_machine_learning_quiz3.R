#1 Load the cell segmentation data from the AppliedPredictiveModeling package using the commands:
	library(AppliedPredictiveModeling)
	data(segmentationOriginal)
	library(caret)
		#1. Subset the data to a training set and testing set based on the Case variable in the data set.
		train<-segmentationOriginal[segmentationOriginal$Case=="Train",]
		test<-segmentationOriginal[segmentationOriginal$Case=="Test",]
		#2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings.
		set.seed(125)
		model<-train(Class ~ ., data = train, method = "rpart")
		#3. In the final model what would be the final model prediction for cases with the following variable values:
		library(rattle)
		library(rpart.plot)
		fancyRpartPlot(model$finalModel)

		#A:
		#a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
		PS
		#b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
		WS
		#c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
		PS
		#d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
		??


#2 If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) accuracy smaller or bigger? If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger. Is K large or small in leave one out cross validation?
	
	#A:  The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.



#3 Load the olive oil data using the commands:
	library(pgmm)
	data(olive)
	olive = olive[,-1]

	#These data contain information on 572 different Italian olive oils from multiple regions in Italy. Fit a classification tree where Area is the outcome variable. Then predict the value of area for the following data frame using the tree command with all defaults
	newdata = as.data.frame(t(colMeans(olive)))
	modelFit2<- train(Area~., data=olive,method="rpart")

	result2<- predict(modelFit2,newdata)
	result2
	# A -- 2.783. It is strange because Area should be a qualitative variable - but tree is reporting the average value of Area as a numeric variable in the leaf predicted for newdata

#4 Load the South Africa Heart Disease Data and create training and test sets with the following code:
	library(ElemStatLearn)
	data(SAheart)
	set.seed(8484)
	train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
	trainSA = SAheart[train,]
	testSA = SAheart[-train,]
	#Then set the seed to 13234 and fit a logistic regression model (method="glm", be sure to specify family="binomial") with Coronary Heart Disease (chd) as the outcome and age at onset, current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors. Calculate the misclassification rate for your model using this function and a prediction on the "response" scale:

		set.seed(13234)
		#create model
		modelFit3<- train(chd~age+alcohol+obesity+tobacco+typea+ldl,data=trainSA,method="glm",family="binomial")

		#Evaluate misclassification
		missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
		
		result3_train<- missClass(trainSA$chd,predict(modelFit3,trainSA))
		result3_test<- missClass(testSA$chd,predict(modelFit3,testSA))
		
		#A result3_train = 0.2727273  and  result3_test = 0.3116883
		
		
		
#5 Load the vowel.train and vowel.test data sets:
	library(ElemStatLearn)
	library(randomForest)
	data(vowel.train)
	data(vowel.test)
	
	#Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. Fit a random forest predictor relating the factor variable y to the remaining variables. Read about variable importance in random forests here: http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr The caret package uses by default the Gini importance.
	# change variable y to a factor
	vowel.train$y<- as.factor(vowel.train$y)
	vowel.test$y<- as.factor(vowel.test$y)
	# train model with all remaining variables to predict y using random forest method
		# [NOTE: Use randomForest() specifically, not caret, as theres been some issues reported with that approach. 11/6/2016]
	modelFit4<- train(y~.,data=vowel.train,method="rf")

	#Calculate the variable importance using the varImp function in the caret package. What is the order of variable importance? 
	varImp(modelFit4)
	#A
	#rf variable importance

    #Overall
	#x.2  100.000
	#x.1   96.575
	#x.5   43.649
	#x.6   31.117
	#x.8   25.257
	#x.4   13.137
	#x.3    9.204
	#x.9    9.071
	#x.7    4.573
	#x.10   0.000

	  