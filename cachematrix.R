## Functions calculate inverse of matrix; if matrix has been cached, cached inverse will be returned
## makeCacheMatrix makes a special "matrix"; matrix contains set, get, setinverse, and getinverse functions
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y)
{
x <<- y
m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve returns inverse matrix of special "matrix" created in makeCacheMatrix
## function checks to see if matrix has been cached, and returns cached inverse of matrix if so
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
m <- x$getinverse()
if(!is.null(m)) {
message("getting cached data")
return(m)
}
data <- x$get()
m <- solve(data, ...)
x$setinverse(m)
m
}



## testMatrix for application of functions above
## Define 2x2 test matrix
testmatrix <- matrix(c(1, 2, 3, 4), 2,2)

## Define testcache, create special "matrix"
testcache <- makeCacheMatrix(testmatrix)

## Run cacheSolve on testmatrix & testcache to return inverse
testsolve <- cacheSolve(testcache)