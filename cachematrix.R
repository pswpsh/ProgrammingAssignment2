## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# The first function, makeCacheMatrix creates a special (cache) "matrix", 
# which is really a list containing a function to
#
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix inverse
# 4. get the value of the matrix inverse
#
# Computing the inverse of a square matrix can be done with the solve function 
# in R. The <<- operator is used to assign a value to an object in an 
# environment that is different from the current environment(set, setinverse). 
#
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(x_param) {
                x <<- x_param
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inv_param) inv <<- inv_param
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#
# The following function calculates the inverse of the special (cache) "matrix" 
# created with the above function. It first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and skips 
# the computation. Otherwise, it calculates the inverse via solve function and
# sets the value of the "inv" variable in the cache via the setinverse function.
#
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