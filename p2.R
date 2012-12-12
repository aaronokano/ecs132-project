parkinson <- read.csv('parkinsons.data', header=TRUE)

# Generalized logit function. Inputs are the input variables (the t's in the
# book) and the coefficients (the betas in the book). Both inputs are in vector
# form. Naturally, the vector t should have one less element than the vector b.
logit <- function(t,b) {1/(1+exp(-(b %*% c(1,t)))) }

# Generalized glm cross validation. Takes as input a data frame where the first
# column is status and the remaining columns are predictors. Returns proportion
# of successful predictions.
# Call the function with cvglmprop( parkinson[,c("status","var1","var2",...)])
# If you want to add interaction terms, perform the multiplication beforehand:
# temp <- parkinson[,c("status","var1","var2")]
# temp$v1v2 <- temp$var1 * temp$var2
# cvglmprop( temp )
cvglmprop <- function( variables ) {
  v <- sample( 1:(nrow(variables) - 1), (nrow( variables ) - 1) * 0.5 )
  notv <- setdiff( 1:(nrow(variables) - 1), v )
  model <- glm( status ~ ., data = variables[v,], family = binomial )
  pred <- apply( as.matrix( variables[notv,-1] ), 1, logit,
                model$coefficients )
  pred <- pred > 0.5
  mean( pred == variables[notv,1] )
}

#Nonparametric 
#mean( sapply( 1:4000, function(n) {v <- sample(1:195, 150); mean( parkinson$status[-v] == knn(predMat[v,],parkinson$status[v],10,predMat[-v,],"cls")$predyvals)} ) )
