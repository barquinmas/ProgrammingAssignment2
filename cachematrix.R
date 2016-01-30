## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function first sets the the matrix, then gets the matrix, then sets the inverse of the matrix and finally gets the inverse 
## of the matrix.
makeCacheMatrix <- function(x = matrix()) {
           inv <- NULL
           set <- function(y){
                x <<- y
                inv <<- NULL
           }
           get <- function() x
           setsolve <- function(solve) inv <<- solve
           getsolve <- function() inv
           list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
## This function first checks if the inverse of the function has been calculated from the makeCacheMatrix function. If so it gets
## the inverse and finishes. If not, it calculates it´s inverse and sets it in the cache via the setsolve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setsolve(inv)
        inv 
}
