makeCacheMatrix <- function(x=matrix())
{
  inv <- NULL
  
  set = function(y)
  {
    x <<- y
    inv <<- NULL
    
  }
  
  get = function() x
  
  setinverse = function(invers) inv <<- invers
  getinverse = function() inv 
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

cacheSolve <- function (x,...)
{
   i <- x$getinverse()
   
   if(!is.null(i))
   {
     message("print cached data")
     return(i)
   }
   
  data <- x$get()
  i<- solve(data,...)
  x$setinverse(i)
  return(i)
  
}

