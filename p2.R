parkinson <- read.csv('parkinsons.data', header=TRUE)

# Generalized logit function. Inputs are the input variables (the t's in the
# book) and the coefficients (the betas in the book). Both inputs are in vector
# form. Naturally, the vector t should have one less element than the vector b.
logit <- function(t,b) {1/(1+exp(-(b %*% c(1,t)))) }
crossvalglm <- function( response, predictor, predictor2, predictor3 ) {
  v <- sample( 1:(length(response) - 1), (length( response ) - 1) * 0.5 )
  notv <- setdiff( 1:(length(response) - 1), v )
  model <- glm( response[v] ~ predictor[v] + predictor2[v] + predictor3[v],
               family = binomial )
  cor( response[notv], mapply( logit3, model$coefficients[1],
                              model$coefficients[2],
                              model$coefficients[3],
                              model$coefficients[4],
                              predictor[notv],
                              predictor2[notv],
                              predictor3[notv])
  )^2
}

crossvalglm2 <- function( response, predictor, predictor2 ) {
  v <- sample( 1:(length(response) - 1), (length( response ) - 1) * 0.5 )
  notv <- setdiff( 1:(length(response) - 1), v )
  model <- glm( response[v] ~ predictor[v] + predictor2[v], family = binomial
               )
  cor( response[notv], mapply( logit2, model$coefficients[1],
                              model$coefficients[2],
                              model$coefficients[3],
                              predictor[notv],
                              predictor2[notv])
  )^2
}

crossvalglm3 <- function( response, predictor ) {
  v <- sample( 1:(length(response) - 1), (length( response ) - 1) * 0.5 )
  notv <- setdiff( 1:(length(response) - 1), v )
  model <- glm( response[v] ~ predictor[v,], family = binomial )
  cor( response[notv], mapply( logit, model$coefficients[1],
                              model$coefficients[2],
                              predictor[notv]
                              ) )^2
}

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

mean( sapply( 1:4000, function(n) {v <- sample(1:194, 150); cor(
                                                                parkinson$status[-v],
                                                                knn(
                                                                    predMat[v,],
                                                                    parkinson$status[v],
                                                                    10,
                                                                    predMat[-v,]
                                                                    )$predyvals
                                                                )} )^2 )