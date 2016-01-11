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
text(iris.tree001)

# Decision tree use tree and 10% test data
n=0.1*nrow(iris)
test.index=sample(1:nrow(iris),n)
iris.train01=iris[-test.index,]
iris.test01=iris[test.index,]
iris.tree01=tree(Species ~ .,data=iris.train01)
iris.tree01
plot(iris.tree01)
text(iris.tree01)


# Decision tree use tree and 20% test data
n=0.2*nrow(iris)
test.index=sample(1:nrow(iris),n)
iris.train=iris[-test.index,]
iris.test02=iris[test.index,]
iris.tree02=tree(Species ~ .,data=iris.train)
iris.tree02
plot(iris.tree02)
text(iris.tree02)
