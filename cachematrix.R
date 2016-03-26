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
## The makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse. It includes set and get functions to set and retrieve a matrix
## "x" and cache and retrieve the value of its inverse
## 
makeCacheMatrix <- function(x = matrix()) {
        # cache to store inverse of matrix
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() 
                x
        
        #sets or caches inverse of matrix
        setinverse <- function(inverse) 
                inv <<- inverse
                
        #retrieves inverse of matrix from cache
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

        # Trying to retrieve inverse from cache
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse of matrix")
                return(inv)
        }
        
        data <- x$get()
        # Inverse is calculated
        inv <- solve(data)
        
        # Inverse is cached
        message("caching inverse of matrix... ")
        x$setinverse(inv)
        
        # return inverse
        inv
                
}

