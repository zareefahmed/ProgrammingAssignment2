
#  We are creating functions to calculate matrix inversion and then saving it to cache, so we can use cache to prevent heavy calculation in future. 

###################################################################################################################

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# This function returns the inverse of the matrix. 
# the inverse has already been computed and it is available in cache, then it return the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# our earlier  setinverse function.
# we are  assuming that the matrix is always invertible.


cacheSolve <- function(x, ...) {
inv <- x$getinverse()
if(!is.null(inv)) {
message("getting cached data.")
return(inv)
}
data <- x$get()
inv <- solve(data)
x$setinverse(inv)
inv
}