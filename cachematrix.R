## These functions create and manage a matrix and the inverse of the matrix. The inverse is calculated
## once and saved so subsequent request for the inverse is retrieved from the saved cache.

## makeCacheMatrix - This function creates a matrix where the inverse of the matrix can be cached.
## The functions takes one optional argument:
#       x - the initial value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Argument x is the matrix
        ## i is the cached inverse of the matrix.
        i <- NULL
        
        ## The set() function sets the matrix value.
        set <- function(inputMatrix) {
                x <<- inputMatrix
                i <<- NULL
        }
        ## The get() funciton retrieves the matrix value
        get <- function() x
        ## The setInverse() function sets the matrix inverse value
        setInverse <- function(inverse) i <<- inverse
        ## The getInverse() function retrieves the matrix inverse value
        getInverse <- function() i
        ## This list the return value of the cache matrix.
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve - This function will get the inverse of a cache matrix. If the inverse is not cached, it will
## calculate the inverse and cache it in the cache matrix. On subsequent calls to the cacheSolve, the inverse
## will be retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##  Get the inverse from the cache matrix
        i <- x$getInverse()
        ## If there is a value, return it
        if(!is.null(i)){
                message("getting cached inverse of the matrix")
                return(i)
        }
        ## If there there was not a value for the inverse, calculate the inverse, cache it in the matrix
        ## and return it.
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}
