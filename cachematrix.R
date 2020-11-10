## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function uses lexical scoping in order to show how variables and
# objects are nested into a function. In this case we need to cache a
# matrix inverted so it's important to store the values in memory to 
# accelerate subsequent access to the same object.
# This function initialize a list of function where first argument is a
# matrix by default.
# Once you go to the last code section you can a list of function that 
# will be returned in the parent enviroment.
# Now this function is ready to be cached by cacheSolve.

makeCacheMatrix <- function( x = matrix()){
   m <- NULL
   set <- function(y){
      x <<- y
      m <<- NULL
   }
   get <- function()x
   setmatrix <- function(solve) m <<- solve
   getmatrix <- function() m
   list(set = set,
        get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
   
}


## Write a short comment describing this function

# This function will be used to retrieve the inverted of makecacheMatrix,
# otherwise the latter function will be incompleted.
# cacheSolve takes 2 arguments. First an object that will be retrived from 
# makecacheMatrix and another additional arguments to be passed.
# Later checks if the result (is.null) is false cacheSolve will take the input
# and it will calculate the matrix inverted.

cacheSolve <- function(x, ...) {
   m <- x$getmatrix()
   if(!is.null(m)){
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setmatrix(m)
   m
}

# Now we can tested it.

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
h <- makeMatrix(m1)
g <- cacheMatrix(h)

# Here you can initialize a vector to be able to return th value of the cacheSolveS


