#12.10 The following data are given:
#  x 0 1 2 3 4 5 6
#  y 1 4 5 3 2 3 4
### (a) Fit the quartic model μY |x = β0 +β1x+β2x^2 +β3x^3+β4x^4.
### (b) Fit the cubic model μY |x = β0 +β1x+β2x^2 +β3x^3.
### (c) Fit the quadratic model μY |x = β0 +β1x+β2x^2.
### (d) Fit the linear model μY |x = β0 +β1x.
### (e) Predict Y when x = 2 by (a),(b),(c),(d).


# Input the data
y <- c(1,4,5,3,2,3,4)
x0 <- c(1,1,1,1,1,1,1)
x1 <- c(0,1,2,3,4,5,6)
x2 <- x1^2
x3 <- x1^3
x4 <- x1^4
XY <- cbind(x1,y)
# Sol: Transform the quartic model to multiple variables; x -> x1, x^2 -> x2, x^3 -> x3, x^4 -> x4
# Rewrite the data as
#   x1 0 1  2  3   4   5    6  ## x1 = x
#   x2 0 1  4  9  16  25   36  ## x2 = x^2
#   x3 0 1  8 27  64 125  216  ## x3 = x^3
#   x4 0 1 16 81 256 625 1296  ## x4 = x^4
#    y 1 4  5  3   2   3   4
# Solve b = Inverse(X'X)%*%X'y or b=Inverse(A)%*%g

Z4XX1 <- c(length(y),sum(x1),sum(x2),sum(x3),sum(x4))
Z4XX2 <- c(sum(x1),sum(x1*x1),sum(x1*x2),sum(x1*x3),sum(x1*x4))
Z4XX3 <- c(sum(x2),sum(x2*x1),sum(x2*x2),sum(x2*x3),sum(x2*x4))
Z4XX4 <- c(sum(x3),sum(x3*x1),sum(x3*x2),sum(x3*x3),sum(x3*x4))
Z4XX5 <- c(sum(x4),sum(x4*x1),sum(x4*x2),sum(x4*x3),sum(x4*x4))
Z4XX <- rbind(Z4XX1,Z4XX2,Z4XX3,Z4XX4,Z4XX5)
Z4XX    #Z4XX is (X'X) or A
Z4XXi <- solve(Z4XX)   #Inverse (X'X) or A
Z4XXi
Z4Xy <- c(sum(y),sum(x1*y),sum(x2*y),sum(x3*y),sum(x4*y))
Z4Xy    #Z4Xy is (X'y) or g
Z4b <- Z4XXi%*%Z4Xy   #Apply the analytical solution b=Inverse(A)*g
Z4b     #Get b, the regression coefficient
Z4pX <- cbind(x0,x1,x2,x3,x4)
Z4pY <- Z4pX%*%Z4b
Z4pXpY <- cbind(x1,Z4pY) #combind the x,y,predict y

# Sol: Transform the cubic model to multiple variables; x -> x1, x^2 -> x2, x^3 -> x3
# Rewrite the data as
#   x1 0 1 2  3  4   5   6  ## x1 = x
#   x2 0 1 4  9 16  25  36  ## x2 = x^2
#   x3 0 1 8 27 64 125 216  ## x3 = x^3
#    y 1 4 5  3  2   3   4
# Solve b = Inverse(X'X)%*%X'y
Z3XX1 <- c(length(y),sum(x1),sum(x2),sum(x3))
Z3XX2 <- c(sum(x1),sum(x1*x1),sum(x1*x2),sum(x1*x3))
Z3XX3 <- c(sum(x2),sum(x2*x1),sum(x2*x2),sum(x2*x3))
Z3XX4 <- c(sum(x3),sum(x3*x1),sum(x3*x2),sum(x3*x3))
Z3XX <- rbind(Z3XX1,Z3XX2,Z3XX3,Z3XX4)
Z3XXi <- solve(Z3XX)
Z3Xy <- c(sum(y),sum(x1*y),sum(x2*y),sum(x3*y))
Z3b <- Z3XXi%*%Z3Xy
Z3pX <- cbind(x0,x1,x2,x3)
Z3pY <- Z3pX%*%Z3b
Z3pXpY <- cbind(x1,Z3pY)

# Sol: Transform the quadratic model to multiple variables; x -> x1, x^2 -> x2
# Rewrite the data as
#   x1 0 1 2  3  4   5   6  ## x1 = x
#   x2 0 1 4  9 16  25  36  ## x2 = x^2
#    y 1 4 5  3  2   3   4
# Solve b = Inverse(X'X)%*%X'y

Z2XX1 <- c(length(y),sum(x1),sum(x2))
Z2XX2 <- c(sum(x1),sum(x1*x1),sum(x1*x2))
Z2XX3 <- c(sum(x2),sum(x2*x1),sum(x2*x2))
Z2XX <- rbind(Z2XX1,Z2XX2,Z2XX3)
Z2XXi <- solve(Z2XX)
Z2Xy <- c(sum(y),sum(x1*y),sum(x2*y))
Z2b <- Z2XXi%*%Z2Xy
Z2pX <- cbind(x0,x1,x2)
Z2pY <- Z2pX%*%Z2b
Z2pXpY <- cbind(x1,Z2pY)

# Sol: Transform the linear model to single variables; x -> x1
# Rewrite the data as
#   x1 0 1 2  3  4   5   6  ## x1 = x
#    y 1 4 5  3  2   3   4
# Solve b = Inverse(X'X)%*%X'y
Z1XX1 <- c(length(y),sum(x1))
Z1XX2 <- c(sum(x1),sum(x1*x1))
Z1XX <- rbind(Z1XX1,Z1XX2)

Z1XXi <- solve(Z1XX)
Z1Xy <- c(sum(y),sum(x1*y))
Z1b <- Z1XXi%*%Z1Xy
Z1pX <- cbind(x0,x1)
Z1pY <- Z1pX%*%Z1b
Z1pXpY <- cbind(x1,Z1pY)

par(mfrow=c(2,2))
# Plot
plot(XY,pch=6,col=6,type = "b")
points(Z1pXpY,pch=1,col=1,type = "b")
title(main = "μY |x = β0 +β1x ")

plot(XY,pch=6,col=6,type = "b")
title(main = "μY |x = β0 +β1x+β2x^2 ")
points(Z2pXpY,pch=2,col=2,type = "b")

plot(XY,pch=6,col=6,type = "b")
title(main = "μY |x = β0 +β1x+β2x^2 +β3x^3 ")
points(Z3pXpY,pch=3,col=3,type = "b")

plot(XY,pch=6,col=6,type = "b")
title(main = "μY |x = β0 +β1x+β2x^2 +β3x^3+β4x^4 ")
points(Z4pXpY,pch=4,col=4,type = "b")

# Add the legend
#legend(4,5,legend = c("SampleData","Z1","Z2","Z3","Z4"),col=c(6,1,2,3,4),pch = c(6,1,2,3,4))

# Verify the Coefficents result with the result by lm()
# Translate the list to a data frame
ds1 <- data.frame(cbind(y,x1,x2,x3,x4))
attach(ds1)

fit1 <- lm(y~x1)
fit1$coefficients   #The coefficient is to compare with Z1b
Z1b
fit2 <- lm(y~x1+x2)
fit2$coefficients   #The coefficient is to compare with Z2b   
Z2b
fit3 <- lm(y~x1+x2+x3)
fit3$coefficients   #The coefficient is to compare with Z3b
Z3b
fit4 <- lm(y~x1+x2+x3+x4)
Z4b
fit4$coefficients #The coefficient is to compare with Z4b
