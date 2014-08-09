## makeCacheMatrix takes a square invertible matrix as argument and returns an object
## to cache its inverse so that it need not be evaluated again
## Note: the argument is not tested for invertibility
##
## the caching-object is a list of 4 functions:
## set(newdata) which overwrites the data matrix and empties the cache of any inverse
## get() which returns the data matrix
## setinv(inverse) which loads the cache with its argument
## getinv() which returns the contents of the cache
##
## cacheSolve takes as argument an object created by makeCacheMatrix and returns the 
## inverse of the data matrix recorded in that object
##
## If cacheSolve has not yet been applied to its argument, then it calculates the inverse
## of the data matrix recorded in the argument and caches that inverse in the object
## If cacheSolve has already cached the inverse, then it is returned from the cache

## makeCacheMatrix initializes a cache-object for the inverse of a matrix, which
## cacheSolve then uses to return the inverse of the matrix and cache the inverse for
## later use
## The set(newdata) component function of the caching-object is used to change the data
## matrix without rerunning makeCacheMatrix

## makeCacheMatrix initializes a cache-object for the inverse of a matrix, the component
## functions of which then allow the cache to be used and the cached inverse to be changed

makeCacheMatrix <- function(x=martrix()) {       # pass an invertible matrix
                   inv <- NULL                   # initialize an empty cache
                   set <- function(newdata) {    # function to change to new data
                          x <<- newdata            # overwrite previous matrix
                          inv <<- NULL             # empty the cache
                   }
                   get <- function() x                         # returns recorded matrix
                   setinv <- function(inverse) inv <<- inverse # loads the cache
                   getinv <- function() inv                    # returns the cache contents
                   list(set=set, get=get, setinv=setinv,getinv=getinv) #return cache-object
}

## cacheSolve takes as argument an object created by makeCacheMatrix, returns the inverse
## of the data matrix recorded in that object and caches the inverse for later use

cacheSolve <- function(x, ...) {     # pass a cache-object initialized by makeCacheMatrix
              inv <- x$getinv()      # load the contents of the cache, if any
              if (!is.null(inv)) {   # if there is something in the cache
                     message("getting cached inverse matrix") # announce it
                     return(inv)                              # return cache contents
              }
              mat <- x$get()         # if cache is empty , get matrix of the cache object
              inv <- solve(mat, ...) # calculate its inverse
              x$setinv(inv)          # load its inverse into the cache
              inv                    # return the inverse
}