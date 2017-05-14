## Put comments here that give an overall description of what your
## functions do
## The first function inputs a matrix and outputs it in a format such that
## its inverse can be cached
## The second function first looks if the matrix has been cached before
## if it has, the saved solution is outputted
## if not, the solution is computed using solve() and then saved

makeCacheMatrix <- function(x = matrix()) {
        # set the value of the matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get the value of the matrix
        get <- function() x
        # set the value of the inverse
        setinverse <- function(solve) m <<- solve
        # get the value of the inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
