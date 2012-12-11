
# the function knn() does k-nearest neighbor regression; the user has a
# choice of either just fitting to the x,y dataset or using that data to
# predict new observations newobs for which only the predictors are
# known

# arguments:

# x:  matrix or data frame of the predictor variable data, one row per
#     observation
# 
# y:  vector of the response variables corresponding to x; in the
#     classification case, these are assumed to be 1s and 0s
# 
# k:  the number of nearest neighbors to use for estimating the regression
#     or predicting the new data
# 
# newobs:  a matrix of values of the predictors, one row per observation,
#          on which to predict the responses; default value is NULL
# 
# regtype:  "reg" for prediction of continuous variables, "cls" for
#           classification problems; default value "reg"
#

# return value: an R list with the following components
# 
# predsuccess:  computed only if newobs = NULL; measure of predictive 
#               success on the original data; R^2 in the "reg" case, 
#               proportion of correctly classified observations in the 
#               "cls" case
# 
# predyvals:  if newobs is NULL, predicted values for y from x, used to
#             compute predsuccess; otherwise, the predicted values from newobs

# to do:  I'd rather exclude an observation in x,y from predicting itself,
# amounting to cross validation; easy fix, but to be done later

library(RANN)  # fast nearest-neighbor finder on CRAN

knn <- function(x,y,k,newobs=NULL,regtype="reg") {
   # make sure x is a matrix or data frame for use with RANN
   if (is.vector(x)) x <- matrix(x,ncol=1)
   retval <- list()  
   # just trying out on current data set?
   if (is.null(newobs)) {  
      nearones <- nn2(data=x,k=k,query=x)$nn.idx
   } else {
      nearones <- nn2(data=x,k=k,query=newobs)$nn.idx
   }
   # row i of nearones consists of the indices in x of the k 
   # closest observations in x to observation i of x or newobs
   retval$predyvals <- apply(nearones,1,pred1y,y,regtype)
   if (is.null(newobs)) {
      if (regtype=="reg") {
         tmp <- cor(retval$predyvals,y)
         retval$predsuccess <- tmp^2
      } else {
         retval$predsuccess <- mean(retval$predyvals == y)
      }
   }
   retval
}

# determine the predicted value of y in a single observation, knowing
# the indices xidxs of the values in the original data x that are
# closest to a given observation
pred1y <- function(xidxs,y,regtype) {
   predval <- mean(y[xidxs])
   ifelse (regtype == "reg", predval, as.integer(predval > 0.5)) 
}

