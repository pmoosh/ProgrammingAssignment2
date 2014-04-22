## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # initialise the inversion flag
        inv <- NULL
        set <- function(y) {
            # set the martix x
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("We inverted the matrix already")
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)
        inv
            
        
    
}
