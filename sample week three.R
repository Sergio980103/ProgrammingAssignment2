

makeVector <- function(x = numeric()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setmean <- function(mean) m <<- mean
   getmean <- function() m
   list(set = set, get = get,
        setmean = setmean,
        getmean = getmean)
}

cachemean <- function(x, ...) {
   m <- x$getmean()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- mean(data, ...)
   x$setmean(m)
   m
}

r <- makeVector(1:1000)
cachemean(r)

makeMatrix <- function( x = matrix()){
   m <- NULL
   set <- function(y){
      x <<- y
      m <<- NULL
   }
   get <- function()x
   setmatrix <- function(matrix) m <<- matrix
   getmatrix <- function() m
   list(set = set,
        get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
   
}

cacheMatrix <- function(x, ...){
   m <- x$getmatrix()
   if(!is.null(m)){
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- matrix(data, ...)
   x$setmatrix(m)
   m
}

h <- makeMatrix(10:10)
solve(h)

m <- matrix(c(1/2, 3/4, 5/7, 4/3), 2, 2)
n <- matrix(c(2, 4, 7, 3), 2, 2)

inv <- round(j%*%m)
j <- solve(m)


