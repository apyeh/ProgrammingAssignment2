## makeCacheMatrix function creates a list containing a function to do the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                        ## initialize value of inverse to NULL
        set <- function(y) {               ## create function 'set' whose arg y is the matrix passed to makeCacheMatrix function
                x <<- y                    ## and caches input matrix y to x
                inv <<- NULL               ## and sets inverse to NULL
        }
        get <- function() x                ## create function 'get' and assign vector x to it
        setinverse <- function(inverse) inv <<- inverse    ## create function 'setinverse'
        getinverse <- function() inv       ## create function getinverse and assign vector inv to it
        list(set = set, get = get,         ## lists the values of the functions within makeCacheMatrix function
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve function checks if matrix inverse has already been calculated and is in cache.
## If not, function calculates the inverse of matrix 'x' and places it into cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()       ## retrieves value stored in cache (x) and assigns it to inv 
        if(!is.null(inv)) {         ## if there already is a computed matrix inverse in cache,
                message("getting cached data")  ## print message "getting cached data"
                return(inv)         ## and return the cached matrix
        }
        data <- x$get()             ## If computed matrix inverse is not in cache already, 
        inv <- solve(data, ...)     ## compute the matrix inverse
        x$setinverse(inv)           ## and place the computed matrix inverse into cache.
        inv
}
