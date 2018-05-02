## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that provide several functions:
#set matrix: set the matrix
#getMatrix get the matrix variable X
#setReverse set the reverse variable R
#getReverse: get the reverse varaible R

makeCacheMatrix <- function(x = matrix()) {
    r<-NULL #set reverse to NULL by default
    
    getMatrix <- function() { #Set getMatrix to x which is the function variable
        x
    }
    
    setMatrix <-function(y){ #Assign parent variable x y value and put r (reverse) to NULL
        x<<-y
        r <<- NULL
    }
    setReverse<-function(y){ #Assign global reverse value to y
        r <<- y
    }
    
    getReverse<-function(){#Return r reverse value
        r
    }
    
    list(setMatrix=setMatrix,getMatrix=getMatrix,setReverse=setReverse,getReverse=getReverse)
    
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#the inverse has already been calculated (and the matrix has not changed), then the 
#cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    t<-x$getReverse()
    
    if (!is.null(t)){
        print('getting from cache')
        return(t)
    }
    data <- x$getMatrix()
    x$setReverse(solve(data))
    
}


