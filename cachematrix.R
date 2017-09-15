makeCacheMatrix <- function(x = matrix())
    {
              inv <- NULL
              set = function(y)
              {
                #Assign the input argument to the x object in the parent environment.
                x <<- y
                #This line of code clears any value of "inv" that had been cached by a prior execution of cacheSolve().
                inv <<- NULL
              }
              #R retrieves it from the parent environment of makeCacheMatrix().
              get = function() x
              #assign the input argument to the value of inv in the parent environment
              setinverse = function(invers)
                inv <<- invers
              # R retrieves it from the parent environment of makeCacheMatrix().
              getinverse = function()inv
              list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
              )
      }

#cachesolve()checks to see whether the result is NULL. #if the value here is not equal to NULL, we have a valid, 
#cached solve and can return it to the parent environment or else calculates the value.

cacheSolve <- function (x, ...)
      {
              i <- x$getinverse()
              if (!is.null(i))
              {
                message("print cached data")
                return(i)
              }
              #If the result of !is.null(m) is FALSE,  calculates a solve(), 
              #uses the setinverse() function to set the solve in the input object,
              data <- x$get()
              i <- solve(data, ...)
              x$setinverse(i)
              return(i)
        
      }
