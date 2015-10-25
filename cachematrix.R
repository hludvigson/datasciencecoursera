## Hudson Ludvigson, Coursera student, RProgramming course
## 10/25/2015.  Comments for programming assignment 2.

## This function takes a matrix as an argument and stores
## in cache it's inverse IF the inverse exists.  It returns these
## values to an assigneed variable when making the call has completed

##  Example:  x <- matrix(c(1,2,3,4),2,2)
##            values <- makeCacheMatrix(x)
##            values$set, values$get, etc. are all then valid calls
## 
## To verify an inverse exists we first check that the 
## number of rows == number of columns.  Then we check
## for a non-zero determinant. If conditions are met we 
## calculate the inverse, else we provide useful feedback
## to the user.
##
## 

makeCacheMatrix <- function(x = matrix()) {

  if (nrow(x) != ncol(x))
  {
    message("you cannot create an inverse of this matrix,
            you need the same numbers of rows and columns and
            the determinant must be non-zero")
  }
  else
  {
    if(det(x) != 0)
    {
      storedMatrix <- NULL
      fSet <- function(vNewValue) 
      {
        x <<- vNewValue
        storedMatrix <<- NULL
      }
      
      fGet <- function() 
      {
        x
      }
      
      fSetinverse <- function(inverse) 
      { 
        storedMatrix <<- inverse
      }
      
      fGetinverse <- function()
      {
        storedMatrix
      }
      
      list(
        set = fSet, 
        get = fGet,
        setinverse = fSetinverse,
        getinverse = fGetinverse
      )
    }
    else
    {
      message("det = 0, cannot find inverse")
    }
  }
}


## This function can take as an input a variable that was create
## using the makeCacheSolve function.  It will then leverage the four
## methods of get, set, getinverse, setinverse where appropriate to only
## calculate the inverse of a matrix if it needs to, otherwise it will 
## simply pull that value from cache so it does not have to recalculate it.  

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
