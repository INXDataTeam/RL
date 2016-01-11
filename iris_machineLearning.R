head(iris,5)
mean(iris$Sepal.Length)
round(mean(iris$Sepal.Length),2)
# Use attach to create the link with iris, thus no need to set the data set prefix
attach(iris)
round(mean(Sepal.Length),2)
summary(iris)
# Plot
plot(Petal.Length,Species) # Plot the scatter chart with two variables
plot(Species,Petal.Length)   # Plot the Bar chart 
plot(Petal.Width,Species) # Box plot
plot(Species,Petal.Length) # Box plot by Petal Length
# Matrix of the scatter chart
plot(iris)
# From the Matrix scatter plot
# Get (1) linear realtion - petal.length & petal.widthe
#     (2) dinstinction of species 1 & 3 by the petal.length & the petal.width
plot(Petal.Length,Species) # x axis : lenght ; y axis : species
plot(Species,Petal.Width)  # x axis : species; y axis : length
# plot with more arguments & add the point and the legend by layer
# main - the chart name
# pch -> point type; assign by number
# col -> point color, assign by number or color ("black","red","green","blue"..)
# xlim,ylim; the range of x axis & y axis
# xlab, ylab; the lable of the x axis & y axis
# points - add the point by layer
# legend - add the point by layer
plot(Petal.Length[Species=="setosa"],Petal.Width[Species=="setosa"],pch=1,col=1,xlim=c(1,7),ylim=c(0,3),main = "Scatter-PLenth & PWidth",xlab ="PLen" ,ylab = "PWid")
points(Petal.Length[Species=="versicolor"],Petal.Width[Species=="versicolor"],pch=2,col=2)
points(Petal.Length[Species=="virginica"],Petal.Width[Species=="virginica"],pch=3,col="blue")
legend(1.5,3,legend = c("setosa","versicolor","virginica"),col=c(1,2,3),pch = c(1,2,3))

# Test of the normal distributio
# sample to test if these four measuremens according normal distribution

# Shapiro-Wilk test
shapiro.test(Sepal.Length)
shapiro.test(Sepal.Width)
shapiro.test(Petal.Length)
shapiro.test(Petal.Width)
# Shapiro-Wilk test by the Species group
rs1 <- with(iris, tapply(Sepal.Length, Species, shapiro.test))
rs2 <- with(iris, tapply(Sepal.Width, Species, shapiro.test))
rs3 <- with(iris, tapply(Petal.Length, Species, shapiro.test))
rs4 <- with(iris, tapply(Petal.Width, Species, shapiro.test))

# Anderson-Darling test
library(nortest)
ad.test(Sepal.Length)
ad.test(Sepal.Width)
ad.test(Petal.Length)
ad.test(Petal.Width)

# Anderson-Darling test by the Species group
ra1 <- with(iris, tapply(Sepal.Length, Species, ad.test))
ra2 <- with(iris, tapply(Sepal.Width, Species, ad.test))
ra3 <- with(iris, tapply(Petal.Length, Species, ad.test))
ra4 <- with(iris, tapply(Petal.Width, Species, ad.test))
# Compare consistence of the shapiro.test & the ad.test; 
nt_l <- c("rs1","rs2","rs3","rs4","ra1","ra2","ra3","ra4")


par(mfrow=c(1,3))
library(tree)
# Decision tree use tree and low test data
n=0.01*nrow(iris)
test.index=sample(1:nrow(iris),n)
iris.train001=iris[-test.index,]
iris.test001=iris[test.index,]
iris.tree001=tree(Species ~ .,data=iris.train)
iris.tree001
plot(iris.tree001)
title(main = "tree with 0.01 test sample")
text(iris.tree001)


# Decision tree use tree and 10% test data
n=0.1*nrow(iris)
test.index=sample(1:nrow(iris),n)
iris.train01=iris[-test.index,]
iris.test01=iris[test.index,]
iris.tree01=tree(Species ~ .,data=iris.train01)
iris.tree01
plot(iris.tree01)
title(main = "tree with 0.1 test sample")
text(iris.tree01)


# Decision tree use tree and 20% test data
n=0.2*nrow(iris)
test.index=sample(1:nrow(iris),n)
iris.train=iris[-test.index,]
iris.test02=iris[test.index,]
iris.tree02=tree(Species ~ .,data=iris.train)
iris.tree02
plot(iris.tree02)
title(main = "tree with 0.2 test sample")
text(iris.tree02)


# Decision Trees with Package party
par(mfrow=c(1,3))
library(party)
#set.seed(1234) # the random seed is set to a fixed value to make the result reproducible
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.99,0.01)) #Bootstrap resampling; replace = TRUE
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data = trainData)
#Check the prediction
table(predict(iris_ctree),trainData$Species)
summary(iris_ctree)
plot(iris_ctree,main = "tree by ctree, test sample 0.01")
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.9,0.1))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
library(party)
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data = trainData)
#Check the prediction
table(predict(iris_ctree),trainData$Species)
summary(iris_ctree)
plot(iris_ctree,main = "tree by ctree, test sample 0.1")

ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.8,0.2))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
library(party)
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data = trainData)
#Check the prediction
table(predict(iris_ctree),trainData$Species)
summary(iris_ctree)
plot(iris_ctree,main = "tree by ctree, test sample 0.2")
plot(iris_ctree,main = "tree by ctree, test sample 02; plot type = simple", type = "simple")



#Random Forest
library(randomForest)
set.seed(777)
iris.rf=randomForest(Species ~.,data=iris.train,importane=T,proximity=T)
print(iris.rf)
round(importance(iris.rf),2)
names(iris.rf)
(table.rf=iris.rf$confusion)
sum(diag(table.rf)/sum(table.rf))
#predict
(rf.pred=predict(iris.rf,newdata=iris.test))
(table.test=table(Species=species.test,Predicted=rf.pred))
sum(diag(table.test)/sum(table.test))
