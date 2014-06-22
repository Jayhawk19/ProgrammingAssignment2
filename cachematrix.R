## Put comments here that give an overall description of what your
## functions do

#Set <- This function sets / updates the value of the matrix
#on which the inverse of matrix needs to be calucalted, also wile doing this,
#it nullifis the cached mean to null, since a new mean needs to be
#calculated
#get <- This function reads the matrix that needs to inversed
#setInvMatrix <- assings the latest inverse matrix calculated
#getInvMAtrix <- get the latest inverse matrix, if null or has any value

## Write a short comment describing this function
#This funciton is used to make a list of 4 elements to store
# get, set, getInvMatrix and setInvMatrix values to reduce repeated
# inverse matrix computations
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setInvMatrix <- function(inverse) m <<- inverse
    getInvMatrix <- function() m
    list(set = set, get = get,
    setInvMatrix = setInvMatrix,
    getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function
# this function will first check if there is an inverse matrix calculated
# if no, then computes it and returns and sets the updated value, if already
# computed reads from the cache and displays to ther user
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInvMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInvMatrix(m)
    m
    
}
