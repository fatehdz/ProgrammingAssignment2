## The functions listed below aim to save time to the programmer when dealing with Matrix inversion
## The functions allow he programmer to benefit from caching the inverse of a square Matrix and use that cache in 
## the other parts of his program instead of recalculating the inverse at each step which is very time consuming.



## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1-set the value of the vector
## 2-get the value of the vector
## 3-set the values of the inverse Matrix
## 4-get the values of the inverse Matrix

makeCacheMatrix <- function(x = matrix()) 
        {        
        
        i <- matrix()
        set <- function(y) 
                {
                x <<- y
                i <<- matrix()
                }
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getminverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)      
       }


## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
