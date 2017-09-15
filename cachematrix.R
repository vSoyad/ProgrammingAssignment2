makeCacheMatrix <- function(x=matrix())
{
  inv <- NULL
  
  set = function(y)
  {
    #Assign the input argument to the x object in the parent environment.
    x <<- y
    #This line of code clears any value of "inv" that had been cached by a prior execution of cacheSolve().
    inv <<- NULL
    
  }
  
  #Since the symbol x is not defined within get(), R retrieves it from the parent environment of makeCacheMatrix().
  get = function() x
  
  #assign the input argument to the value of inv in the parent environment
  
  setinverse = function(invers) inv <<- invers
  
  # R retrieves it from the parent environment of makeCacheMatrix().
  getinverse = function() inv 
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

cacheSolve <- function (x,...)
{
  #checks to see whether the result is NULL. Since makeCacheMatrix() sets the cached mean to NULL whenever a new vector is set into the object,
  #if the value here is not equal to NULL, we have a valid, cached solve and can return it to the parent environment.
  
  i <- x$getinverse()
  
  if(!is.null(i))
  {
    message("print cached data")
    return(i)
  }
  #If the result of !is.null(m) is FALSE, cacheSolve() gets the matrix from the input object, 
  #calculates a solve(), uses the setinverse() function on the input object to set the solve in the input object, 
  #and then returns the value of the solve to the parent environment by printing the inverse matrix object.
  data <- x$get()
  i<- solve(data,...)
  x$setinverse(i)
  return(i)
  
}

