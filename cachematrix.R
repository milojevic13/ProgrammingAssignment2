## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ##This function creates a special "matrix" object. It chaches the inverse of the matrix.   
       
         ##Set the inv value to NULL and asing the matrix to a value x.            
        inv<-NULL
        set<-function(y) {
                x <<- y
                inv <<- NULL
        }
        ##Calculate the inverse of a matrix x using inverse function.
        get<-function() x
        setinv<-function(solve) inv <<- solve 
        getinv <-function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

        ##This function takes as an input the created special "matrix" returend from the previosuly defined 
        ##function.
        
        inv<-x$getinv()
        
        ##Checking if the invrese is alerady calculated.
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        ##If not, it calculates the inverse of the matrix using solve function.
        mat.data<-x$get()
        inv<-solve(mat.data, ...)
        x$setinv(inv)
        
        ##Return the inverse of the matrix.
        return(inv)
}
