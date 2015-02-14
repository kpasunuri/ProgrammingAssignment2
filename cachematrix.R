## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

# Setting inverse to Null
 inv<-NULL
 
 # This function is used to change the elements of matrix and reset inv to Null
   set<-function(y){
   x<<-y
   inv<<-NULL 
   }
   
  # This function is used to display the elements of the matrix
   get<-function() x
   
  # This function is used to set inverse of matrix
   setinverse <-function(inverse) inv <<-inverse
   
  # This function is used to get inverse of a given matrix ( First time it is called only when we use cacheSolve() due to lazy computation
   getinverse <-function() inv
   
  # makeCacheMatrix returns a list containing functions to set/get a matrix and inverse.
   list(set=set,get=get,
        setinverse=setinverse,
		getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
	# Fetching inverse of input matrix x
	
		inv<-x$getinverse()
		
	# If the inverse exists then retrieve from cache and return it
		if(!is.null(inv)){
		        message("getting cached inverse")
                return(inv)
	    }
		
	# In case if the inverse does not exists then use get function to get matrix and compute inverse using solve
		mat<-x$get()
		inv<-solve(mat)
		
    # using set inverse to assign the calculated inverse
		x$setinverse(inv)

	# return the inverse
		inv
}
