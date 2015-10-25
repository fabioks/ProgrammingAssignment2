## The functions bellow works creating an object to cache the result of
## the inverse of a matrix that is expensive computation.

## The makeCacheMatrix function creates a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the valeu of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function returns the inverse of the matrix firt checking
## if already exists. If exists it gets the value and if not exists it sets
## the value in the cache via setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
