## What is this?   I don't understand what this thing does AT ALL.
## This is where I took the example function and substituted the word
## "inverse" for the word "mean". In principle this is supposed to cache the 
## result of a function and retrive it.  If you put a gun to my head, I 
## couldn't tell you how it works.

## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to:

##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
      }
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## This function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the
## data and sets the value of the inverse in the cache via the setinverse
## function.

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

