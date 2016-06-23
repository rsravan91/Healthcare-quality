setwd("C:/Users/DELL/Desktop/ISSMTECHASSIGNMENTS/Kaggle/Healthcare quality/")
healthcare=read.csv("quality.csv")
str(healthcare)
# wee see that office vists and narcotics are important, given when both are
# low then patient is most prob recieving good care
with(healthcare,plot(OfficeVisits,Narcotics,col=factor(PoorCare),pch=19))

# Now we choose baseline model by selecting most frequent target, here its 0
table(healthcare$PoorCare)
# we predict output as 0 in baseline model regardless of input
# accuracy of such a model is 98/(98+33)=74.8%

# Now we built logistic reg model
# install package to split dataset
# install.packages("caTools") 

library("caTools")
set.seed(88)
split=sample.split(Y = healthcare$PoorCare,SplitRatio = 0.75) # It also makes sure that target is well balanced
healthcareTrain=subset(healthcare,split==TRUE)
healthcareTest=subset(healthcare,split==FALSE)

# Build logistic reg model using office visits and narcotics

QualityLog=glm(PoorCare~OfficeVisits+Narcotics,healthcareTrain,family="binomial") # glm stands for generalised linear model
summary(QualityLog)

# we see that coef of officevisits and narcotics are positive which prove that
# there is positive linear relationship wrt poorcare as proved via graph above
# AIC is similar to adjusted R2 as it uses number of variables used
# AIC can be used or model selection
# Lesser AIC means a better model. So Model having lesser AIC is selected
# here in predict test data is not provided, this makes the function predict on the train set itself

predictTrain=predict(QualityLog,type="response")
# type = reponse tells function to give probabilities
summary(predictTrain)

# We want to check if we predicting higher prob for actual poor care cases
# This can be done calculating the average prob value for actual poorcare value

tapply(predictTrain,healthcareTrain$PoorCare,mean)
# We see that on average we predict higher prob for actual poor case(1) than actual good care(0)

# We create confusion matrix with diff values for threshold
# True stands for poor care and False stands for good care
# here we choose the threshold of 0.5
table(healthcareTrain$PoorCare,predictTrain>0.5)
# We calculate sensitivity and specificity
sensitivity=10/25
specificity=70/74

# We will generate ROC curve to pick the best threshold value
library(ROCR)
# Prediction function is used to transform the input data into a standardized format
# Performance is called on this transformed data to plot ROC
ROCRPred=prediction(predictTrain,healthcareTrain$PoorCare)
ROCRPerf=performance(ROCRPred,"tpr","fpr")
plot(ROCRPerf)
# Add color and labels to ROC curve based on threshold values, we replot the above graph
# The value of adj determines the way in which text strings are justified in text
# A value of 0 produces left-justified text, 0.5 (the default) centered text and 1 right-justified text.
# Note that the adj argument of text also allows adj = c(x, y) for different adjustment in x- and y- directions

plot(ROCRPerf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=0.0)



####### Now we apply the model on test set and check ROC and AUC
predictTest = predict(QualityLog, type="response", newdata=healthcareTest)
ROCRpredTest = prediction(predictTest, healthcareTest$PoorCare)
ROCRperfTest = performance(ROCRpredTest,"tpr","fpr")
# plot ROC
plot(ROCRperfTest,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=0.0)
# calculate AUC
as.numeric(performance(ROCRpredTest, "auc")@y.values)
