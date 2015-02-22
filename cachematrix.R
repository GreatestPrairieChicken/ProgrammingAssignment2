## Together, these functions calculate and store the value of an inverted matrix.
## They take advantage of the <<- superassignment operator to assign the "answer matrix" 
## (i.e., the matrix returned by using the solve() function on an input matrix) to a 
## storage variable outside of the local, function environment.  

## Using this strategy can save time/processing power/memory
## when dealing with large data sets.

## makeCacheMatrix() sets up a function with one argument, x, which will take the matrix class.  
## It defines all of the variables and sub-functions that will be put to work in cacheSolve().
## Note the use of <<- to store values for x and m in the global environment instead of the 
## function's environment.  This lets cacheSolve() access those values.  Since lexical scoping 
## searches the environment in which variable or function is defined first, m and x would not 
## be available to cacheSolve() if the <- operator had been used instead.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## cacheSolve() uses an if/then statement to check whether the local m has a value.  
## In other words, whether the answer matrix has already been calculated.  
## If it finds a non-null version of m in the local environment, it tells the user 
## that it is "getting cached data," and then displays that cached data. 
## If it finds that m is null, it will calculate the inverse, store the answer 
## matrix locally, and display it.
## The first time the user runs cacheSolve(), it will find a null value in m, because
## because it was defined that way in makeCacheMatrix().  However, if cacheSolve() is run again, 
## it will now find the stored answer matrix that was calculated in the previous run/pass
## and display that instead of going through the calculation again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
