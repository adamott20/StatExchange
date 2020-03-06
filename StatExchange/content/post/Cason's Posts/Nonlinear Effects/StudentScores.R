results <- read.table('SchoolResults.txt', header = TRUE)

model <- lm(Score ~ 1 + Lunch + Computer + Expenditure + poly(Income,9) + English + STratio, data = results)
model2 <- lm(Score ~ 1 + Lunch + Computer + Expenditure + poly(Income,5) + English + STratio, data = results)
model3 <- lm(Score ~ 1 + Lunch + Computer + Expenditure + poly(Income,4) + English + STratio, data = results)

summary(model)
as.matrix(poly(results$Income, 5))

numPoly <- 5

all_x <- rbind(results, predict_xs)




for(i in 2:numPoly){
  all_x <- cbind(all_x, poly(all_x$Income, numPoly)[,i]) 
  colnames(all_x)[ncol(all_x)] <- paste0("Income",i)
}


library(glmnet)
lambda <- 10^seq(-1, -.5, by = .0001)
inputs <- as.matrix(all_x[1:nrow(results),-1])
response <- as.matrix(all_x[1:nrow(results),1])


cv_output <- cv.glmnet(inputs, response, alpha = 1, lambda = lambda)
plot(cv_output)
best_lam <- cv_output$lambda.min

lasso_best <- glmnet(inputs, response, alpha = 1, lambda = best_lam)

pred_x <- all_x[(nrow(results)+1):nrow(all_x),-1]

predictions <- predict(lasso_best, newx = as.matrix(pred_x))

plot(predict_xs$Income, predictions, type = 'l')
points(results$Income, results$Score, col = 2)

predict_xs$Score <- c(as.matrix(cbind(1,pred_x))%*%as.matrix(coef(lasso_best)))

write.csv(predict_xs, 'C:/Users/cason/Desktop/Classes/Assignments/Stat 536/Homework 3/prediction_xs_2.csv', row.names = FALSE)



these.results <- cbind(results, poly(results$Income, numPoly)[,-1]) 

SE <- numeric()
bias <- numeric()
for(i in 1:nrow(results)){
  this.model <- glmnet(as.matrix(these.results[-i,-1]), these.results$Score[-i], alpha = 1, lambda = best_lam)
  this.pred <- as.numeric(predict(this.model, newx = as.matrix(these.results[i,-1])))
  bias[i] <- this.pred-these.results$Score[i]
  SE[i] <- (bias[i])^2
}

# RMSE
sqrt(mean(SE))




this.model <- glmnet(as.matrix(these.results[,-1]), these.results$Score, alpha = 1, lambda = best_lam)
this.pred <- as.numeric(predict(this.model, newx = as.matrix(these.results[,-1])))

# R squared
1-mean((this.pred-these.results$Score)^2)/mean((mean(these.results$Score)-these.results$Score)^2)


coefs <- c()
for(i in 1:1000){
  ids <- sample(nrow(these.results), replace = TRUE)
  this.sample <- these.results[ids,]
  this.model <- glmnet(as.matrix(this.sample[,-1]), this.sample$Score, alpha = 1, lambda = best_lam)
  coefs <- cbind(coefs,as.matrix(coef(this.model)))
}




plot(predict(this.model, as.matrix(these.results[,-1])), results$Score, pch = 19, col = "cornflowerblue",
     main = "Fitted vs Actual Student Scores", xlab = "Fitted Scores", ylab = "Actual Scores")
abline(0,1, lwd = 2)
