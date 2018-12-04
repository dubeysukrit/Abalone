#setwd("C:/Users/Archer/Desktop/Praxis_Notes/MI")

#Clear Environment
#rm(list = ls())

# Import Dataset
rings <- read.csv("a.csv")
head(rings)
nrow(rings)
cor(rings$Rings, rings$Shell.weight)
#plot(rings$Shell.weight, rings$Rings)

summary(rings)

#Plotting dataset
png(filename = "Dataset.png", width = 1280, height=788)
plot(rings$Shell.weight, rings$Rings, pch=19, cex=0.9, 
     xlab = "Weight of Shell (grams)",
     ylab = "No. of Rings", main = "Abalone Dataset",
     sub = "ScatterPlot: Weight of Shell vs No. of rings", 
     col = "darkblue")
dev.off()


#DATA:
set.seed(1)
rand = sample(1:nrow(rings), 0.875*nrow(rings))
train = rings[rand, ]
test = rings[-rand, ]


# error dataframe - data for test and train error segmented by sample size 
error_df = data.frame(matrix(ncol=3, nrow=0))


# loop over sample sizes to generate error dataframe
for(n in c(10, 20, 50, 100, 150, 250, 300, 400, 500, 600, 700, 800)){
    set.seed(1)
    rand_train = sample(1:nrow(train), n)
    train_sample = train[rand_train, ]
    
    m7 <- lm(Rings ~ Shell.weight + I(Shell.weight^2) + I(Shell.weight^3) 
              + I(Shell.weight^4) + I(Shell.weight^5) + I(Shell.weight^6) 
              + I(Shell.weight^7), train_sample)
    
    #TRAIN AND TEST ACCURACY
    train_error = sum(round(m7$residuals)^2)
    pred = predict(m7, newdata=test)
    test_error = sum((round(pred) - test$Rings) ^ 2)
    
    error_df <- rbind(error_df, c(n, test_error, train_error))
}
colnames(error_df) <- c('Sample_Size', 'Test_Error', 'Train_Error')
error_df


#Plotting Test Error vs Sample Size
png(filename = "Test_vs_Sample.png", width = 1280, height=788)
plot(error_df$Sample_Size, error_df$Test_Error, pch=19, cex=0.5, type = 'l', 
     xlab = 'Sample Size', ylab = 'Test Error', col = 'blue', 
     main = 'Test Error vs Sample Size\nPolynomial Regression of Order 7')
dev.off()


#Plotting Train Error vs Sample Size
png(filename = "Train_vs_Sample.png", width = 1280, height=788)
plot(error_df$Sample_Size, error_df$Train_Error, pch=19, cex=0.5, type = 'b', 
     xlab = 'Sample Size', ylab = 'Train Error', col = 'darkblue', 
     main = 'Train Error vs Sample Size\nPolynomial Regression of Order 7')
dev.off()
