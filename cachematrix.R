## Create an object which embedds a matrix, but which can also store
## a cached value for the inverse of that matrix (so that if you
## ask for the inverse repeatedly the work is only done once).

## Create a "cachematrix" object which is capable of remembering its
## inverse.  (N.b. We don't actually compute the inverse here (since
## maybe it isn't going to be needed after all); we just save a place
## to store it later if needed.)
makeCacheMatrix <- function(x = matrix()) {
        # Some argument checking...
        stopifnot(is.matrix(x)) # must be a matrix
        stopifnot(dim(x)[1] == dim(x)[2]) # must be square
    
        myCachedInverse <- NULL
        
        # Change the stored matrix from the originally provided value
        set <- function(y) {
            stopifnot(is.matrix(y)) # must be a matrix
            stopifnot(dim(y)[1] == dim(y)[2]) # must be square
            x <<- y
            # If we change undelying matrix we must invalidate old cache!
            myCachedInverse <<- NULL
        }
        
        # Retrieve the underlying matrix
        get <- function() {
            x
        }
        # Remember (i.e. put in cache) the inverse of x
        setInverse <- function(inv) {
            myCachedInverse <<- inv
        }
        # Return cached inverse. Returns null if cache isn't yet set
        getInverse <- function() {
            myCachedInverse
        }
        # Returns a structure (list) with these four member functions
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Operates on a cachematrix object and returns the matrix inverse, at
## the same time remembering its answer in cache (to make any
## subsequent requests for the inverse more efficient.)
cacheSolve <- function(x, ...) {
    # The next two lines act as a crude check that "x" is
    # of the correct sort, i.e. is a cacheMatrix object
    stopifnot(is.list(x) && !is.null(x$getInverse))
    stopifnot(is.list(x) && !is.null(x$setInverse))
              
    inv <- x$getInverse()
    if(!is.null(inv)) {
        # Message is just for debugging; can be removed eventually
        message("getting cached data for matrix inverse")
        # Hooray! We have an answer already cached; return it.
        return(inv)
    }
    # Otherwise, cache was empty so evaluate from scratch, save, return
    data <- x$get()
              
    # "Real" code needs to check that 'solve' succeeds in getting
    # an inverse, i.e. data was an invertible matrix.
    # But we've been asked to assume that inversion is always possible.
              
    inv <- solve(data, ...)

    x$setInverse(inv) # store in cache

    inv
    ## Returns a matrix that is the inverse of 'x'
}
