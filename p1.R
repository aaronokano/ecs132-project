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

# Modification to show mean temperature per location
tempGrid <- function( data ) {
  origin <- min( data$X,data$Y )
  maximum <- max( data$X,data$Y )
  coords <- as.list(as.data.frame(t(expand.grid( origin:maximum, origin:maximum ))))
  # Create map with mean temperature
  t(matrix( sapply( coords, 
    function(x) mean( data[ which( data$X == x[1] & data$Y == x[2] ), ]$temp ) ),
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

genCond <- function( v ) {
  sorted <- sort( v )
  s <- split( sorted, ceiling( seq( length(v) ) /  (length(v)/3) ) )
  cbind( v >= head(s$'1', n=1) & v <= tail(s$'1', n=1),
         v >= head(s$'2', n=1) & v <= tail(s$'2', n=1),
         v >= head(s$'3', n=1) & v <= tail(s$'3', n=1))
}

# Mean area based on conditions
meanConds <- function(data, conditions, v) {
  mean( data[ !apply( conditions, 1, function(x)
                     any(!x[cbind(v,1:length(x[1,]))])), ]$area )
}

# Makes condition arrays using list of variables to form conditions on
makeConds <- function( variables ) {
  m <- lapply( variables, genCond )
  array( unlist( m ), dim = c(dim(m[[1]]),length(m)))
}

#conds <- makeConds( data[,5:11] )
#perms <- data.matrix( expand.grid( rep( list(1:4), 7 ) ) )
#means <- apply( perms, 1, function(x) meanConds( data, conds, x ) )
#table <- cbind( perms, means )
#table <- table[complete.cases(table),]
#table <- table[order(table[,8]),]
#mapply( function(x,y,n) mean( table[ table[,8] > x & table[,8] <= y, ][,n] ),
#       0, 4.4, 1:7 )
                       

#model <- lm( log( area + 1 ) ~ month + temp + RH + DC + month:RH, data=ordata)


#> summary( lm( area ~ FFMC + ISI + ISI:FFMC + DC + DC:ISI + month, data=ordata
#              > ) )
#
#Call:
#lm(formula = area ~ FFMC + ISI + ISI:FFMC + DC + DC:ISI + month, 
#       data = ordata)
#
#Residuals:
#   Min     1Q Median     3Q    Max 
#-3.353 -1.556 -0.476  1.118  4.743 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)   
#(Intercept) 18.7922107  8.5250001   2.204  0.02909 * 
#FFMC        -0.2123159  0.1031263  -2.059  0.04132 * 
#ISI         -2.1986055  1.3395298  -1.641  0.10291   
#DC           0.0103264  0.0031436   3.285  0.00128 **
#month        2.6778341  0.8689659   3.082  0.00247 **
#FFMC:ISI     0.0258508  0.0146401   1.766  0.07956 . 
#ISI:DC      -0.0004180  0.0002632  -1.588  0.11442   
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#Residual standard error: 2.007 on 144 degrees of freedom
#Multiple R-squared: 0.1142, Adjusted R-squared: 0.0773 
#F-statistic: 3.094 on 6 and 144 DF,  p-value: 0.007022 




data <- read.csv('forestfires.csv',head=TRUE)
data$month <- factor(data$month,
    levels=c('jan','feb','mar','apr','may','jun',
             'jul','aug','sep','oct','nov','dec'))
data$day <-
  factor(data$day,levels=c('mon','tue','wed','thu','fri','sat','sun'))
data$month <- sin( as.integer( data$month ) * pi / 6 )
data$day <- sin( as.integer( data$day ) * 2 * pi / 7 )
ordata <- data[order(data$area),][248:400,]
ordata2 <- data[order(data$area),][248:448,]
ordata2$area <- log( ordata2$area + 1 )

