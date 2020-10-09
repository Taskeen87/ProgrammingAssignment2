## The first function creates an Object of the type makeCacheMatrix. This basically acts like a
## square matrix. In addition to creating the matrix, this caches the matrix elements and its dimensions.

## After creating an object of class makeCacheMatrix, we don't have to initialize it yet again, we can just
## set a new value to it as we please. 

##The second function first checks if there is any existing inverted result
## of the object. If there is, it will not calculate it again and go straight to the result. If the object definition
## has changed, it will find its invert(as long as it's not singular).


## The first function stores the elements of the square matrix and lists four functions.

## set -- when called, will set the elements and dimensions to the new values and reset the invert in the 
## parent environment because they will be required even after this function is done.

## get -- user can get the current value of the object.
## setinv -- sets the inverted value to inv in the parent directory, as we might use it later.
## getinv -- gets the cached value of the invert.

## creates a named list of the functions for object attribute that can be accessed with $subset

makeCacheMatrix <- function(x = matrix()) 
{   
	numrow <- nrow(x)
	numcol <- ncol(x)
	inv <- NULL
        set <- function(y) 
        {
                x <<- y
                numrow <<- nrow(x)
                numcol <<- ncol(x)
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverted) inv <<- inverted
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)      
}

## inverts the matrix but first checks if there's already a result from the previous definition.
## if there's a new definition to the object, then the cache is wiped out.

cacheSolve <- function(x, ...) 
{
        inv <- x$getinv()
        if(!is.null(inv)) 
	{
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
