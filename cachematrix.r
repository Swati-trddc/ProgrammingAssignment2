## Inverse of matrix
## Calculating the inverse of a matrix requires cumbersome computation. 
## Following functions creates a matrix object and caches the inverse of matrix rather than computing it repeatedly.

## Following function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  inv<-NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) inv<<-inverse
  getInverse<-function() inv
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Following function computes the inverse of matrix. 
## If it is already calculted then it will retrieve the inverse from cache.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv))
  {
    print("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}