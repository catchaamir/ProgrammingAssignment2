## The functions create a cache for time consuming
## operation, in this case inverse of a matrix

## The following function creates a special matrix-
## "makeCacheMatrix", which is a list containing 
## a function to

##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse
##  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special matrix
## created with the above function but first checks if inverse is
## already calculated. If it gets mean from cache, it skips the calculation
## otherwise calculates the inverse of matrix in cahce and via "setmean" function.


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
