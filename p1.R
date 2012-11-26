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

data <- read.csv('forestfires.csv',head=TRUE)
data$month <- factor(data$month,
    levels=c('jan','feb','mar','apr','may','jun',
             'jul','aug','sep','oct','nov','dec'))
data$day <-
  factor(data$day,levels=c('mon','tue','wed','thu','fri','sat','sun'))
