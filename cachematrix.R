## The below functions calculates and caches the inverse of a matrix.If the inverse already have been calculated (and the matrix
## has not changed) then the inverse will be retrived from the cache together with the message "getting cached data". 


## This function defines x as a matrix and creates a empty space were the inversion of the inputed matrix can be stored.
## It then creates a list of the four created functions while storing  the value of the matrix and the inversion of the matrix in 
## a separate environment. 

makeCacheMatrix <- function(x = matrix()){              
        m <- NULL                                       
        list(set_matrix = function(new_x) {              
                x <<- new_x                             
                m <<- NULL                              
        }, 
        get_matrix = function() x, 
        set_inverse = function(inv) m <<- inv, 
        get_inverse = function() m
        )
}

## This function retrives the get_inverse function from the created list above. If this value is not NULL it will report back 
## "getting cached data" and retrive the inverse from the cache. If the cached value of the inverse is NULL the function will 
## calculate the inversion of the given matrix and cache the inverse. 


cacheSolve <- function(x, ...){
        m <- x$get_inverse()                            
        if(!is.null(m)){                                 
                print("getting cached data")         
                return(m)
        }
        y <- x$get_matrix()                            
        m <- solve(y, ...)                              
        x$set_inverse(m)                                
        return(m)
}