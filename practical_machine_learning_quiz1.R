###1 Load the Alzheimer's disease data using the commands:
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
#Which of the following commands will create non-overlapping training and test sets with about 50% of the observations assigned to each?

##1-A
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]



###2 Load the cement data using the commands:
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
##Make a plot of the outcome (CompressiveStrength) versus the index of the samples. Color by each of the variables in the data set (you may find the cut2() function in the Hmisc ##package useful for turning continuous covariates into factors). What do you notice in these plots?

##2-A
library(gridExtra)
library(Hmisc)
str(training)
summary(training)
index <- seq_along(1:nrow(training))

Age_10 <- cut2(training$Age,g=10)
FlyAsh_10 <- cut2(training$FlyAsh,g=10)
Cement_10 <- cut2(training$Cement,g=10)
BlastFurnaceSlag_10 <- cut2(training$BlastFurnaceSlag,g=10)
Water_10 <- cut2(training$Water,g=10)
Superplasticizer_10 <- cut2(training$Superplasticizer,g=10)
CoarseAggregate_10 <- cut2(training$CoarseAggregate,g=10)
FineAggregate_10 <- cut2(training$FineAggregate,g=10)


byAge <- qplot(index,CompressiveStrength,data=training,col=Age_10)
byFlyAsh <- qplot(index,CompressiveStrength,data=training,col=FlyAsh_10)


##3 Load the cement data using the commands:
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
###Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might use the log transform to try to make the data more symmetric. Why would that be a poor ###choice for this variable?

##3-A
hist(training$Superplasticizer, breaks=20)
hist(log(training$Superplasticizer+1), breaks=20)
training$Superplasticizer
head(training)



###4  Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]
#Find all the predictor variables in the training set that begin with IL. Perform principal components on these variables with the preProcess() function from the caret package. #Calculate the number of principal components needed to capture 90% of the variance. How many are there?


##4-A
IL_col_idx <- grep("^[Ii][Ll].*", names(training))
preObj <- preProcess(training[, IL_col_idx], method=c("center", "scale", "pca"), thresh=0.9)
preObj
names(preObj)



###5 Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

##Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. Build two predictive models, one using the predictors as ##they are and one using PCA with principal components explaining 80% of the variance in the predictors. Use method="glm" in the train function.

#What is the accuracy of each method in the test set? Which is more accurate?
IL_col_idx <- grep("^[Ii][Ll].*", names(training))
library(dplyr)
new_training <- training[, c(names(training)[IL_col_idx], "diagnosis")]
names(new_training)
new_testing <- testing[, c(names(testing)[IL_col_idx], "diagnosis")]
names(new_testing)


install.packages("e1071")
non_pca_model <- train(diagnosis ~ ., data=new_training, method="glm")
non_pca_result <- confusionMatrix(new_testing[, 13], predict(non_pca_model, new_testing[, -13]))
non_pca_result


# perform PCA extraction on the new training and testing sets
pc_training_obj <- preProcess(new_training[, -13], method=c('center', 'scale', 'pca'), thresh=0.8)
pca_predicts <- predict(pc_training_obj, new_training[, -13])
pc_testing_predict <- predict(pc_training_obj, new_testing[, -13])
# compute the model with pca predictors
###################this last part doens't run##############################################
pca_model <- train(new_training$diagnosis ~ ., data=pca_predicts, method="glm")
# apply the PCA model on the testing set
pca_result <- confusionMatrix(new_testing[, 13], predict(pca_model, pc_testing_preds))
pca_result



