## Creates the cached matrix
## Contains functions to set/get the value of the inverse
## Contains ability to get/set the value of the matrix (cached)

makeCacheMatrix <- function(x = matrix())
{
     m <- NULL
     set <- function(y)
     {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInvert <- function(solve) m <<- solve
     getInvert <- function() m
     list(set = set, get = get,
          setInvert = setInvert,
          getInvert = getInvert)    
}


## Solves the matrix
## first checks if the matrix has already been solved
## if it has, uses the cached solution. 
## otherwise calculate the inverse

cacheSolve <- function(x, ...) 
{
     ## Return a matrix that is the inverse of 'x'
     m <- x$getInvert()
     if(!is.null(m))
     {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInvert(m)
     m
}
