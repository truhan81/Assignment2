## These functions returns inverse of square matrix. If the inverse of the matrix was already calculated, it does not 
## calculate its inverse again, it just read cache and returns the value.


## This function sets the matrix in the global environment and its inverse in the containing environment.
## You can get the inverse of the matrix using getinverse() function.

makeCacheMatrix <- function(x = matrix(nrow = 2, ncol = 2)) {
       
         m <- NULL
        
        set <- function(y){
                
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## First of all, this function checks cache. If the inverse of the matrix was calculated already, then it
## returns the inverse from cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
