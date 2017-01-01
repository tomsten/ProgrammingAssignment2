## The aim of the following functions is to save time calculating the inverse of a matrix, 
## by storing the result in a cache.
## This is completed by the following steps:
## 1. Create the matrix we want to calculate the inverse of.
## 2. Check the cache to see if we already have the inverse stored.
## 3. If we do have the inverse stored, display it straight from the cache without re-calculating, thus saving time.
## 4. If we have no inverse stored, calculate the inverse of the matrix and add it to the cache for future use.
## 5. Finally, display the inverse of the matrix. 

## This function is used to create the matrix that we are going to cache the inverse of in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
	t <- NULL
	set <- function(y) {
		x <<- y
		t <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) t <<- solve
	getinverse <- function() t
	list(set = set, get = get,
		setinverse = setinverse, 
		getinverse = getinverse)
}


## This function checks the cache, if it contains the inverse it displays it.
## If the cache doesn't contain the inverse, it calculates it, adds it to the cache, and displays it.

cacheSolve <- function(x) {
    t <- x$getinverse()
    if(!is.null(t)) {
    	message("getting cached data")
    	return(t)
    }
    data <- x$get()
    t <- solve(data)
    x$setinverse(t)
    t
}