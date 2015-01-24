## This is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
 }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated , then the cachesolve should retrieve the inverse from the cache and
## return the message "getting cached data";
## Otherwise, caculate and return the inverse of the matrix and the message "calculating the inverse".  

cacheSolve <- function(x, ...) {

        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        message("calculating the inverse")
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m       
}
