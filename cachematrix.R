## makeCasheMatrix will create a a special matrix object that can cache its inverse
## cacheSolve will compute the inverse of the matrix returned by makeCacheMatrix, then retrieve
## the inverse from the cache

## Matrix inversion is usually a costly computation and there may be benifits in casching the 
## inverse of a matrix rather than compute it repeatedly.  This pair of functions will acheive this.

##makeCachMatrix will create the a matrix used to cache the inverse of the function

makeCacheMatrix <- function(x = matrix()) {
        ##makeCachMatrix will create the a matrix used to cache the inverse of the function
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function() inv
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
        
}


## cacheSolve will calculate/retrieve the inverse of the matrix,x.

cacheSolve <- function(x, ...) {
        ##This function will calculates or retreieves the inverse of the matrix,x and store
        ##it is in the matrix inv.
        
        inv <- x$getinverse()
        if(!is.null(inv)){
                ## if the inverse has already been found and cached, find it in cashed data
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        #find the inverse if not already cached
        x$setinverse(inv)
        inv
}
