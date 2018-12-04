#clear environment, if required
rm(list = ls()) 

# Read Dataset 
rings <- read.csv("a.csv")
head(rings)
nrow(rings)
cor(rings$Rings, rings$Shell.weight)
plot(rings$Shell.weight, rings$Rings)

#RMSE Function
rmse <- function(error){
    return(sqrt(mean((error)^2)))
}


# error dataframe - data for test and train rmse segmented by order of polynomial 
rmse_df = data.frame(matrix(ncol = 3, nrow = 0))

#rings <- rings[1:1000,]

# Divide dataset:
set.seed(1)
rand = sample(1:nrow(rings), 0.875*nrow(rings))
train = rings[rand, ]
test = rings[-rand, ]



set.seed(20)
rand = sample(1:nrow(train), 700)
train_sample = train[rand, ]
#set.seed(10)
#rand = sample(1:nrow(test), 50)
test_sample = test


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================

m1 <- lm(Rings ~ Shell.weight, train_sample)

#TRAIN AND TEST ACCURACY
train_rmse = rmse(m1$residuals)
pred = predict(m1, newdata=test_sample)
test_rmse = rmse(round(pred)-test_sample$Rings)

rmse_df <- rbind(rmse_df, c(1, test_rmse, train_rmse))

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================

m2 <- lm(Rings ~ Shell.weight + I(Shell.weight^2), train_sample)

#TRAIN AND TEST ACCURACY
train_rmse = rmse(m2$residuals)
pred = predict(m2, newdata=test_sample)
test_rmse = rmse(round(pred)-test_sample$Rings)

rmse_df <- rbind(rmse_df, c(2, test_rmse, train_rmse))


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 3
#=============================================================================================

m3 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3), 
         train_sample)

#TRAIN AND TEST ACCURACY
train_rmse = rmse(m3$residuals)
pred = predict(m3, newdata=test_sample)
test_rmse = rmse(round(pred)-test_sample$Rings)

rmse_df <- rbind(rmse_df, c(3, test_rmse, train_rmse))


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 4
#=============================================================================================

m4 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) 
         + I(Shell.weight^4), train_sample)

#TRAIN AND TEST ACCURACY
train_rmse = rmse(m4$residuals)
pred = predict(m4, newdata=test_sample)
test_rmse = rmse(round(pred)-test_sample$Rings)

rmse_df <- rbind(rmse_df, c(4, test_rmse, train_rmse))


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 5
#=============================================================================================

m5 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) 
         + I(Shell.weight^4) + I(Shell.weight^5), train_sample)

#TRAIN AND TEST ACCURACY
train_rmse = rmse(m5$residuals)
pred = predict(m5, newdata=test_sample)
test_rmse = rmse(round(pred)-test_sample$Rings)

rmse_df <- rbind(rmse_df, c(5, test_rmse, train_rmse))


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 6
#=============================================================================================

m6 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) 
         + I(Shell.weight^4) + I(Shell.weight^5) + I(Shell.weight^6), 
         train_sample)

#TRAIN AND TEST ACCURACY
train_rmse = rmse(m6$residuals)
pred = predict(m6, newdata=test_sample)
test_rmse = rmse(round(pred)-test_sample$Rings)

rmse_df <- rbind(rmse_df, c(6, test_rmse, train_rmse))


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================


m7 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) 
         + I(Shell.weight^4) + I(Shell.weight^5)  + I(Shell.weight^6) 
         + I(Shell.weight^7), train_sample)

#TRAIN AND TEST ACCURACY
train_rmse = rmse(m7$residuals)
pred = predict(m7, newdata=test_sample)
test_rmse = rmse(round(pred)-test_sample$Rings)

rmse_df <- rbind(rmse_df, c(7, test_rmse, train_rmse))

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================

m8 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) + 
             I(Shell.weight^4) + I(Shell.weight^5) + I(Shell.weight^6) + 
             I(Shell.weight^7) + I(Shell.weight^8) , train_sample)

#TRAIN AND TEST ACCURACY
train_rmse = rmse(m8$residuals)
pred = predict(m8, newdata=test_sample)
test_rmse = rmse(round(pred)-test_sample$Rings)

rmse_df <- rbind(rmse_df, c(8, test_rmse, train_rmse))


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================

m9 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) + 
             I(Shell.weight^4) + I(Shell.weight^5) + I(Shell.weight^6) + 
             I(Shell.weight^7) + I(Shell.weight^8) + I(Shell.weight^9), 
         train_sample)

#TRAIN AND TEST ACCURACY
train_rmse = rmse(m9$residuals)
pred = predict(m9, newdata=test_sample)
test_rmse = rmse(round(pred)-test_sample$Rings)

rmse_df <- rbind(rmse_df, c(9, test_rmse, train_rmse))

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================

m10 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) + 
              I(Shell.weight^4) +
              I(Shell.weight^5) + I(Shell.weight^6) + I(Shell.weight^7) +
              I(Shell.weight^8) + I(Shell.weight^9) + I(Shell.weight^10), 
          train_sample)

#TRAIN AND TEST ACCURACY
train_rmse = rmse(m10$residuals)
pred = predict(m10, newdata=test_sample)
test_rmse = rmse(round(pred)-test_sample$Rings)

rmse_df <- rbind(rmse_df, c(10, test_rmse, train_rmse))
colnames(rmse_df) <- c('Order', 'Test_RMSE', 'Train_RMSE')

rmse_df


plot(rmse_df$Order, rmse_df$Train_RMSE, pch=19, cex=0.5, type = 'b', 
     xlab = 'Order of Polynomial (Model Complexity)', 
     ylab = 'RMSE', col = 'darkblue', 
     main = 'RMSE vs Model Complexity',
     sub = "Sample Size: 700", ylim = c(1.5,3.5))


lines(rmse_df$Order, rmse_df$Test_RMSE, col = 'red', 
      type='b',pch = 19)

legend("bottomright", legend = c('Test RMSE', 'Train RMSE'),
       lty = c(1), col = c('red', 'darkblue'), cex = 0.5, box.lty = 2)
