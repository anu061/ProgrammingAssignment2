#### This file contains two functions, as described below:
##
##1.`makeCacheMatrix`: This function creates a special "matrix" object that
##  can cache its inverse.
##
##2.`cacheSolve`: This function computes the inverse of the special "matrix"
##  returned by `makeCacheMatrix` above. If the inverse has already been
##  calculated, then `cacheSolve` retrieves the inverse from the cache.
##
####


###
## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse.
##   
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y)
                x <<- y
        
        get <- function() 
                x
        
        setinverse <- function(inverse) 
                inv <<- inverse
                
        
        getinverse <- function() 
                inv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


### 
## The cacheSolve function computes the inverse of the special "matrix" returned
## by `makeCacheMatrix` above. If the inverse has already been calculated, then
## `cacheSolve` retrieves the inverse from the cache.
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse of matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        message("caching inverse of matrix... ")
        
        x$setinverse(inv)
        inv
                
}

