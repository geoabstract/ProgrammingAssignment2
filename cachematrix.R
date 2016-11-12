library(MASS)

# This function creates a special "matrix" object that can cache its inverse
# 1 - create the NULL object for inverse
# 2 - set the value of the matrix
# 3 - get the value of the matrix
# 4 - set the value of inverse matrix
# 5 - get the value of inverse matrix
makeCacheMatrix <- function(x) {
  
  inv <- NULL
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  # create a list with function methods
  list(get=get, setInv=setInv, getInv=getInv)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("matrix from cache")
    return(inv)
  }
  data <- x$get()
  inv <- ginv(data)
  x$setInv(inv)
  inv
}

## Example usage:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)

## First run -no cache-
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Second run -cache found in object-
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## >