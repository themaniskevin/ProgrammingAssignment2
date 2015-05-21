## makeCacheMatrix takes in a square matrix (x) and creates a list of functions (set,get,setinverse,getinverse) 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                  # Set m = NULL
  
  set <- function(y) {                       # Define 'set' as a function of 'y'
    
    x <<- y                                  # Set 'x' equal to 'y' in the global environment.
    
    m <<- NULL                               # Set 'm' equal to NULL in the global environment
  }
  get <- function() x                        # Define 'get' as a function w/o inputs.'get' returns the value of 'x'
  
  setinverse <- function(inverse) m <<- inverse # Define 'setinverse' as a function w/ input 'mean'. 
  # setinverse sets m=inverse in the global environment.
  
  getinverse <- function() m                 # Define 'getinverse' as a function w/o inputs. 'getinverse' returns the value of 'm'
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)              # Create a list containing set,get,setinverse,getinverse
}


## cacheSolve takes in a square matrix (x) and returns its inverse. It checks to see if the inverse is stored globally,
## if not stored globally, it is set globally.
cacheSolve <- function(x, ...) {
  #'...' passes on arguments passed down from calling function.
  m <- x$getinverse()                     #Set m = x (value derived from getmean function)
  
  if(!is.null(m)) {                       #Test if m IS NOT NULL (empty)
    
    message("getting cached data")        #If m IS NOT empty, get x from cached data
    
    return(m)                             # return m
  }                                       #Otherwise m IS Null, execute lines below.
  
  data <- x$get()                         #Set 'data' equal to x (value derived from get function)
  
  m <- solve(data, ...)                   #Set m equal to the mean of data
  
  x$setinverse(m)                         #Call setmean function with input m. Sets mean, 'm,' in global environment.
  
  m                                       #Return 'm'
}
