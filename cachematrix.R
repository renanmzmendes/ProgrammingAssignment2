## The functions bellow are used to create a special matrix
## that caches its inverse and to compute the inverse of this
## special matrix only when its inverse is not cached

## Creates a list that contains the following methods:
## - set the matrix value
## - get the matrix value
## - set the inverse of the matrix
## - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## If the inverse of the matrix is not yet set,
## computes the inverse of the matrix using the
## function `solve`. Otherwise, returns the cached
## value of the inverse

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, diag(dim(data)[1]), ...) # applies the solve function to
                                                # get the inverse of the matrix,
                                                # solving the equation 
                                                # data %*% inverse = identity
    x$setinverse(inv)
    inv
}
