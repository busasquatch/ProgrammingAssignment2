#makeCacheMatrix is like a "class".  It's utility is to cache a matrix passed to it, and return it when called upon.

makeCacheMatrix <- function(x = matrix()) {
  #if an object is called without a matrix, set m to NULL
  m <- NULL
  
  #set is a function that uses the super assignment operator to set x to y and m to NULL.  If x or m are found in the parent enivronments, it is those values that are redefined
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get is a function that returns x, which is the matrix
  get <- function() { x }
  
  #setinverse is a function and might be better named savecache(), bec/it takes a matrix passed into it and stores it in m, the cached variable
  setinverse <- function(inverse) { m <<- inverse } 
  
  #getinverse is a function and might be better named getcache(), bec/it simply returns the cached variable m
  getinverse <- function() { m }
  
  #create a list of function elements and give them appropriate names
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


# cacheSolve takes an instance of the makeCacheMatrix class.  
# If the inverse of the matrix existing in makeCacheMatrix has already been inverted, it returns the previously inverted matrix.  Otherwise, it uses solve() to invert the matrix, stores the inverse matrix in a parent variable, and returns the inverted matrix.
cacheSolve <- function(x, ...) {
  #the m variable in this function has no relation to the cached m in 
  #makeCacheMatrix. The m varaible could be renamed (i.e. any.x and the function 
  #would still work)
  
  #call getinverse() and assign result to m.  If setinverse() was never called
  #prior to this, getinverse() will be null
  m <- x$getinverse()
  
  #if it isn't null (i.e. setinverse() was already invoked), then return 
  #the cached matrix m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #otherwise, if m is null, get the origial matrix
  data <- x$get()
  
  #solve for the inverse using the solve() function.  For this assignment, 
  #it is okay to assume that the matrix provided will always be invertible.
  #Otherwise, we would have to check for invertibleness here
  m <- solve(data, ...)
  
  #store the inverse in m and return m
  x$setinverse(m)
  m
}
