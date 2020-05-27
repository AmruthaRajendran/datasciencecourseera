## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     m<-NULL
     set<-function(y){
       x<<-y
       m<<-NULL
     }
     get<-function() x
     setmatrix<-function(matrix) m <<-matrix
     getmatrix<-function() m
     list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)

}


## To create an object matrix that can compute its inverse

cacheSolve <- function(x, ...) {
     m<-x$getmatrix()
     if(!is.null(m)){
       message("Getting cached data")
       return(m)
     }
     data<-x$get()
     d<-1/(det(data,...))
     if(d!=Inf){
       m<-solve(data)
       x$setmatrix(m)
       m
     }
}
## This function returns the inverse of the given matrix
