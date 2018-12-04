
#clear environment, if required
#rm(list = ls()) 

# Read Dataset 
rings <- read.csv("a.csv")
head(rings)
nrow(rings)
cor(rings$Rings, rings$Shell.weight)
plot(rings$Shell.weight, rings$Rings)


# Divide dataset:
set.seed(100)
rand = sample(1:nrow(rings), 500)
train = rings[rand, ]
test = rings[-rand, ]


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================

m1 <- lm(Rings ~ Shell.weight, train)
m1


#PLOTTING THE MODEL OVER THE DATA
plot(train$Shell.weight,train$Rings, pch=1, cex=1, xlab = "Weight of Shell\n(grams)",
     ylab = "No. of Rings", main = "Polynomial Regression Lines\nSample size:500")
lines(sort(train$Shell.weight), fitted(m1)[order(train$Shell.weight)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
sum((pred-test$Rings)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================

m2 <- lm(Rings ~ Shell.weight + I(Shell.weight^2), train)
m2

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Shell.weight,train$Rings, pch=19, cex=0.5)
lines(sort(train$Shell.weight), fitted(m2)[order(train$Shell.weight)], col='blue', type='l', pch=19) 

#TRAIN AND TEST ACCURACY
sum(m2$residuals^2)
pred = predict(m2, newdata=test)
sum((pred-test$Rings)^2)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 3
#=============================================================================================

m3 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3), train)
m3

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Shell.weight,train$Rings, pch=19, cex=0.5)
lines(sort(train$Shell.weight), fitted(m3)[order(train$Shell.weight)], col='darkgreen', type='l', pch=19) 

#TRAIN AND TEST ACCURACY
sum(m3$residuals^2)
pred = predict(m3, newdata=test)
sum((pred-test$Rings)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 4
#=============================================================================================

m4 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) + I(Shell.weight^4), train)
m4

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Shell.weight,train$Rings, pch=19, cex=0.5)
lines(sort(train$Shell.weight), fitted(m4)[order(train$Shell.weight)], col='magenta', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
sum(m4$residuals^2)
pred = predict(m4, newdata=test)
sum((pred-test$Rings)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 5
#=============================================================================================

m5 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) + I(Shell.weight^4) + I(Shell.weight^5), train)
m5

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Shell.weight,train$Rings, pch=19, cex=0.5)
lines(sort(train$Shell.weight), fitted(m5)[order(train$Shell.weight)], col='green', type='l',pch=20)

#TRAIN AND TEST ACCURACY
sum(m5$residuals^2)                  #Train
pred = predict(m5, newdata=test)
sum((pred-test$Rings)^2)               #Test



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 6
#=============================================================================================

m6 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) + I(Shell.weight^4) + I(Shell.weight^5)+ I(Shell.weight^5) + I(Shell.weight^6), train)
m6

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Shell.weight,train$Rings, pch=19, cex=0.5)
lines(sort(train$Shell.weight), fitted(m6)[order(train$Shell.weight)], col="darkblue", type='l',pch=20) 

#TRAIN AND TEST ACCURACY
sum(m6$residuals^2)
pred = predict(m6, newdata=test)
sum((pred-test$Rings)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================


m7 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) + I(Shell.weight^4) + I(Shell.weight^5) 
         + I(Shell.weight^6) + I(Shell.weight^7), train)
m7

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Shell.weight,train$Rings, pch=19, cex=0.5)
lines(sort(train$Shell.weight), fitted(m7)[order(train$Shell.weight)], col='brown', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
sum(m7$residuals^2)
pred = predict(m7, newdata=test)
sum((pred-test$Rings)^2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================

m8 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) + I(Shell.weight^4) +
             I(Shell.weight^5) + I(Shell.weight^6) + I(Shell.weight^7) + I(Shell.weight^8) , train)

m8

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Shell.weight,train$Rings, pch=19, cex=0.5)
lines(sort(train$Shell.weight), fitted(m8)[order(train$Shell.weight)], col='orange', type='l',pch=19) 

#TRAIN AND TEST ACCURACY
sum(m8$residuals^2)
pred = predict(m8, newdata=test)
sum((pred-test$Rings)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================

m9 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) + I(Shell.weight^4) +
             I(Shell.weight^5) + I(Shell.weight^6) + I(Shell.weight^7) +
             I(Shell.weight^8) + I(Shell.weight^9) , train)

m9

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Shell.weight,train$Rings, pch=19, cex=0.5)
lines(sort(train$Shell.weight), fitted(m9)[order(train$Shell.weight)], col=118, type='l',pch=19) 


#TRAIN AND TEST ACCURACY
sum(m9$residuals^2)
pred = predict(m9, newdata=test)
sum((pred-test$Rings)^2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================

m10 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) + I(Shell.weight^4) +
             I(Shell.weight^5) + I(Shell.weight^6) + I(Shell.weight^7) +
             I(Shell.weight^8) + I(Shell.weight^9) + I(Shell.weight^10), train)

m10

#PLOTTING THE MODEL OVER THE DATA
#plot(train$Shell.weight,train$Rings, pch=19, cex=0.5)
lines(sort(train$Shell.weight), fitted(m10)[order(train$Shell.weight)], col=115, type='l',pch=19) 

legend("bottomright",legend = c('Order 1', 'Order 2', 'Order 3', 'Order 4', 
                         'Order 5', 'Order 6', 'Order 7', 'Order 8', 
                         'Order 9', 'Order 10'), lty = c(1),
              col = c('red', 'blue', 'darkgreen', 'magenta', 'green', 
                    "darkblue", 'brown', 'orange', 118, 115), cex = 0.41,
       density = 1.5, box.lty = 2)
