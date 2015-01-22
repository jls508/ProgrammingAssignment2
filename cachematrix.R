##PROG ASSIGNMENT 2 PART 1


##This function contains a group of functions used in the cacheSolve function.
##Each time it is ran it will create a unique environment for the matrix entered into it.
makeCacheMatrix <- function(x = matrix()) {
	##Set the value of inv to NULL within the environment of the makeCacheMatrix function
	inv <- NULL
	##Function to set the value of x into the parent environment 
	##and set the value of inv to NULL in the parent environment
	set <- function(y) {
                x <<- y
                inv <<- NULL
		 }
	##Function to get what is known, in this case the matrix x
	get <- function() x
	##Function to set the value of inv to the inverse of matrix x
	setinverse <- function(solve) inv <<- solve
	##Function used to retrieve the value of inv
	getinverse <- function() inv
	##List of functions contained in makeCacheMatrix
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##PROG ASSIGNMENT 2 PART 2
##The argument of this function is the makeCacheMatrix function with the desired matrix you wish to solve
cacheSolve <- function(x, ...) {
	##set the value of inv using the getinverse function from makeCacheMatrix
	inv <- x$getinverse()
	##checks to see if the value of inv is NULL, if not prints message and 
	##returns the stored value for inv exitting the function cacheSolve
      if(!is.null(inv)) {
		message("getting cached inverse")
            return(inv)
      }
	##sets the value of data using the get function from makeCacheMatrix
      data <- x$get()
	##sets the value of inv to the inverse of the matrix x using the solve function
      inv <- solve(data, ...)
	##runs the setinverse function in makeCacheMatrix using the inv value calculated with solve
      x$setinverse(inv)
	##prints value of inv
      inv
}
