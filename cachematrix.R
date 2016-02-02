## cachematrix.R -- R functions for caching the inverse of a 
##   matrix.
##
## Latest version: 2/1/16 (George Chadderdon)

## Create a matrix data structure that can cache the inverse of 
## itself.  This takes a matrix (which should be square and 
## invertible) as an input.

makeCacheMatrix <- function(x=matrix()) 
{
    ## Initialize the inverse to NULL.
    inv <- NULL
    
    ## Set (or reset) the stored matrix.
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    ## Get the stored matrix.
    get <- function() x
    
    ## Set the stored matrix's inverse.
    setinv <- function(theInv) inv <<- theInv
    
    ## Get the stored matrix's inverse.
    getinv <- function() inv
    
    ## Return the list with the embedded functions and data.
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Get the inverse from the cache matrix object, either from the 
## cache if the solve() has been done, or from a call to solve if
## it's not been done yet.  If a new call to solve() is done, cache 
## its result for next time.

cacheSolve <- function(x, ...) 
{
    ## Get the stored inverse of the matrix.
    theInv <- x$getinv()
    
    ## If the stored inverse is not NULL...
    if (!is.null(theInv)) 
    {
        ## Display a message indicating use of the cached data.
        message("getting cached inverse")
        
        ## Return the cached data.
        return(theInv)
    }
    
    ## (Assuming we had NULL...)
    ## Grab the stored matrix.
    m <- x$get()
    
    ## Calculate the inverse.
    theInv <- solve(m, ...)
    
    ## Store the inverse in the cache object.
    x$setinv(theInv)
    
    ## Return the inverse.
    theInv
}
