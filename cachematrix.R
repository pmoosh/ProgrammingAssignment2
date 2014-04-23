## This set of R utilities optimizes the inversion of square matrixes by 
## caching already converted matrixes

## Calling this function with a matrix as parameter will store the matrix. 
## The set function will achieve the same. 
## This function stores the original matrix as well as the inverted

makeCacheMatrix <- function(x = matrix()) {
        # initialise the inversion flag
        inv <- NULL
        
        # set the matrix x
        set <- function(y) {
        
            x <<- y
            # and clear the inversion flag
            inv <<- NULL
        
        }

        # returns the matrix x
        get <- function() x

        # stores the inverted matrix
        setinv <- function(ix) inv <<- ix

        # returns the cached inverted matrix
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## A cached version of solve(). Uses the a matrix stored in makeCacheMatrix 
## inverts it and saves it in makeCacheMatrix. Before it runs the inversion
## process again, it checks with makeCacheMatrix if it already 
## contains a cached copy

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        # ckeck it is already in the cache
        if(!is.null(inv)) {
            message("We inverted the matrix already")
            return(inv)
        }
        
        # invert the matrix
        mat <- x$get()
        
        # if there is an error we let the caller of the function deal with it
        inv <- solve(mat)
        
        # store the inverted matrix in the cache
        x$setinv(inv)
        
        # return the inverted matrix
        inv
            
        
    
}
