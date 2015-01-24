## Put comments here that give an overall description of what your
## sample of usage
## > x <- matrix(c(4,2,7,6), nrow=2, ncol=2)
## > y <- makeCacheMatrix(x)
## > z <- cacheSolve(y)
## running cacheSolve (y) again will return you "getting cached data"
## > m <- x %*% z
## print(m) should return Identity Matrix (which is like "1" for Matrices)

##
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates an environment with two variables, one for storing the matrix and one for storing its inverse. 
## This environment also contains four functions for getting and setting these variables. 
## These four functions keep a reference to the environment in which they were declared.

## "i" is set to NULL at the top of makeCacheMatrix function because of the curious way that <<- works.  
## <<- "cause[s] a search to made through parent environments for an existing definition of the variable being assigned. 
## If such a variable is found (and its binding is not locked) then its value is redefined, otherwise assignment takes place in the global environment."[0].  
## So <<- first looks in its "parent environment", which is to say the environment which this instantiation of makeCacheMatrix points to.  
## If "i" was not initialized/did not exist in this instance of makeCacheMatrix's environment, it would get popped into the R_GlobalEnv (i.e. the interactive space) which we don't want.

## to get value of i call x$getinverse(). call i in global environment will give you error because i is not defined in golbal environment.
## running x$i will also give you NULL because Any object assigned with <- would be destroyed as soon as the function exited.

makeCacheMatrix <- function(x = matrix()) {
	  i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve takes the list of four functions as an argument. 
## All it does is check to see if the inverse is already stored in the environment created by makeCacheMatrix and if not calculate the mean and store it using a getInverse() function. 
## cacheSolve does not create the parent environment of makeCacheMatrix

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
