# Construct matrix with mean area for each coordinate
areaGrid <- function( data ) {
  # For a general dataset of the same format, since I'm in the mood
  origin <- min( data$X,data$Y )
  maximum <- max( data$X,data$Y )
  # Generate all possible coordinates. Use various transformations to create a
  # list of the desired form
  coords <- as.list(as.data.frame(t(expand.grid( origin:maximum, origin:maximum ))))
  # Create map with mean areas
  t(matrix( sapply( coords, 
    function(x) mean( data[ which( data$X == x[1] & data$Y == x[2] ), ]$area ) ),
           maximum, maximum ))
}

# Modification of above to show number of fires per location
frequencyGrid <- function( data ) {
  origin <- min( data$X,data$Y )
  maximum <- max( data$X,data$Y )
  coords <- as.list(as.data.frame(t(expand.grid( origin:maximum, origin:maximum ))))
  # Create map with fire frequencies
  t(matrix( sapply( coords, 
    function(x) length( data[ which( data$X == x[1] & data$Y == x[2] ), ]$area ) ),
           maximum, maximum ))
}

# Find the mean area for each unique entry in dataVector, e.g. the mean area
# for each value of DC would be found with meanArea( data, data$DC ).
# Returns a matrix where the first column is the value in the vector and the
# second is the mean area for that value.
meanArea <- function( data, dataVector ) {
  uniques <- unique( dataVector )
  cbind( uniques, lapply( uniques, function(x) 
                         mean( data[dataVector == x,]$area ) ) )
}

# Function for plugging stuff into linear function
linear <- function(t,b) { b%*%c(1,t) }

# Cross validate. Variables data frame must have area first and all predictors
# afterwards. Output is a matrix with a column of predicted values and a column
# of actual values.
cvlm <- function( variables, ntest ) {
  v <- sample( 1:nrow(variables), ntest )
  model <- lm( area ~ ., variables[v,] )
  cbind( apply( as.matrix( variables[-v,-1] ), 1, linear, model$coefficients
                 ), variables[-v,1] )
}

# Same as above, but different data for training and testing. 
# Both data frames must have same columns
cvlmmore <- function( trainers, testers, ntest ) {
  v <- sample( 1:nrow(testers), ntest )
  model <- lm( area ~ ., trainers )
  cbind( apply( as.matrix( testers[v,-1] ), 1, linear, model$coefficients
                 ), testers[v,1] )
}

# Like cvlmmore, but picks the best of the prediction or a zero
cvlm_or <- function( trainers, testers, ntest ) {
  v <- sample( 1:nrow(testers), ntest )
  model <- lm( area ~ ., trainers )
  m <- cbind( apply( as.matrix( testers[v,-1] ), 1, linear, model$coefficients
                 ), testers[v,1] )
  m[m[,2] == 0,1] <- 0
  m
}

# Same as above, but calls knn instead
cvknn <- function( set, ntrain, k ) {
  v <- sample(1:nrow(set), ntrain)
  r <- data.frame( cbind( knn( set[v,-1], set[v,1], k, set[-v,-1] )$predyvals,
                         set[-v,1] ) )
  r
}

cvknnmore <- function( trainers, testers, ntest, k ) {
  v <- sample(1:nrow(testers), ntest); 
  r <- data.frame( cbind( knn( trainers[,-1], trainers[,1], k, testers[v,-1])$predyvals,
        testers[v,1] ))
  colnames(r) <- c("pred","actual")
  r
}

data <- read.csv('forestfires.csv',head=TRUE)
# Reorder months and days
data$month <- factor(data$month,
    levels=c('jan','feb','mar','apr','may','jun',
             'jul','aug','sep','oct','nov','dec'))
data$day <-
  factor(data$day,levels=c('mon','tue','wed','thu','fri','sat','sun'))
# The following lines are the sine transform of month and day
#data$month <- sin( as.integer( data$month ) * pi / 6 )
#data$day <- sin( as.integer( data$day ) * 2 * pi / 7 )
ordata <- data[order(data$area),][248:400,]
