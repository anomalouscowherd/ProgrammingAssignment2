## OK, So I was being a bit obnoxious in my last commit.  
## This is an explanation of what is happening here:
## you run this set of functions like so:
## 	example_matrix <- matrix(c(1,2,3,.5),2,2)
##
## The matrix looks like this: 
##    		[,1] 	[,2]
##	[1,]     1	 3.0
##	[2,]     2	 0.5
##
## 	example_cached_matrix <- makeCacheMatrix(example_matrix)
##	cacheSolve(example_cached_matrix)
##
## The result looks like this:
##	            [,1]       [,2]
##	[1,] -0.09090909  0.5454545
##	[2,]  0.36363636 -0.1818182
##
## The next time it is run ( with the same input matrix, it should
## deliver the pre-cached result array)


## 	
##	
## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing four functions to:

##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse

## Argument x is the input matrix to be solved ( and cached if 
## necessary)


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

## The argument x is a list of four previously cached functions ( not a 
## matrix) with associated environments.

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

