## Programming Assignment 2: Lexical Scoping
## Pair of functions that can be used cache the inverse of a matrix.


## Make an enhanced matrix object that can cache its inverse.
## Return a list of setters/getters for a given matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {

    # init inverse
    i <- NULL
    
    # set new matrix and reset inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # return matrix
    get <- function() x
    
    # set inverse
    setInverse <- function(inverse) i <<- inverse
    
    # return inverse
    getInverse <- function() i
    
    # return list of setters and getters
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}


## Return the inverse of a matrix. 
## If inverse not cached yet, compute inverse and add to cache.

cacheSolve <- function(x, ...) {
    
    # get inverse from enhanced cache matrix object
    # see makeCacheMatrix()
    i <- x$getInverse()
    
    # if inverse available return cached data...
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    
    # otherwise compute inverse and add to cache
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}


## Not sure if there are any benefits in splitting this functionality
## into two separate functions. The following function accomplishes 
## the same with a single function, and the setInverse() method is no
## longer needed.
##
## Example:
##
## gerrit <- matrix(c(1,2,3,4), nrow=2, ncol=2)
##
## cached <- cacheInverseMatrix(gerrit)
##
## Return original 2x2 matrix:
## cached$get()  
##
## Return inverse matrix:
## cached$getInverse()  

#cacheInverseMatrix <- function(x = matrix(), ...) {
#    # init inverse
#    i <- NULL
#    
#    # set new matrix and reset inverse
#    set <- function(y) {
#        x <<- y
#        i <<- NULL
#    }
#    
#    # return matrix
#    get <- function() x
#    
#    # return inverse
#    getInverse <- function() {
#        if (!is.null(i)) {
#            message("getting cached inverse")
#            return(i)
#        }
#        
#        i <<- solve(x, ...)
#        return(i)
#    }
#    
#    # return list of setters and getters
#    list(set = set, get = get, getInverse = getInverse)
#}
