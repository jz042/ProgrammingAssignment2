## The cachematrix function is a two step process:
## 1. Creates a specific matrix and calculates its inverse
## 2. Computes whether the inverse matrix has been cached and recalls it

## THIS FUNCTION CREATES A SPECIFIC MATRIX FOR WHICH WE WILL CALCULATE THE INVERSE MATRIX

makeCacheMatrix <- function(d,r,c) {
  
  x <- matrix(d,r,c)                        #The matrix is created here
  
  m <- NULL                                 #The solution that is set to NULL every time a new matrix is created
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { x }                   #Returns the matrix that has been created
  setsolve <- function(solve) m <<- solve   #Returns the inverse matrix the first time the cacheSolve function is called
  getsolve <- function() m                  #Returns the inverse matrix the subsequent times the cacheSolve function is called
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## THIS FUNCTION RECALLS THE CACHED INVERSE MATRIX SOLUTION (IF AVAILABLE)
## IF NO SOLUTION HAS BEEN CACHED, IT WILL GENERATE A NEW SOLUTION

cacheSolve <- function(x,...) {             ## Return a matrix that is the inverse of 'x'

  m <- getsolve()                           #Returns the inverse matrix solution for the matrix that's been created
  if(!is.null(m)) {                         #If the solution has been called once already
    message("getting cached data")          #Return this message
    return(m)                               #Then return the cached solution
  }
  data <- get()                             #Recalls the matrix created in the last step
  m <- solve(data)                          #Generate a new solution
  setsolve(m)                               #Store the new solution
  m                                         #Return the new solution
}