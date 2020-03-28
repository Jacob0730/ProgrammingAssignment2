## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse property
        k <- NULL
        set <- function(y) {
                x <<- y
                k <<- NULL
        }
        ## Method the get the matrix
        get <- function() x
        ## Method to set the inverse of the matrix
        setinverse <- function(inverse) k <<- inverse
        ## Method to get the inverse of the matrix
        getinverse <- function() k
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        k <- x$getinverse()
        ## Just return the inverse if its already set
        if (!is.null(k)) {
                message("getting cached data")
                return(k)
        }
        data <- x$get()
        k <- solve(data, ...)
        x$setinverse(k)
        k
}