#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) { #initialize x as a numeric input
        v <- NULL
        set <- function(y) { #takes an argument that is named as y
                x <<- y #<<- assigns the value to an object in the parent environment
                v <<- NULL #clears any value of inv cached before
        }
        get <- function() x #get the value of the vector x from the parent environment
        setinverse <- function(solve) v <<- solve #set the value as the inverse of a matrix
        getinverse <- function() v #get the value of the inverse from the parent environment
        #So far we have the getters and setters defined for x and inv
        #Next, assigns each of these functions as an element within a list(), 
        #and returns it to the parent environment
        list(set = set, # gives the name 'set' to the set() function defined above
             get = get, # gives the name 'get' to the get() function defined above
             setinverse = setinverse, # gives the name 'setinv' to the setinv() function defined above
             getinverse = getinverse) # gives the name 'getinv' to the getinv() function defined above
}


#The next function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { #x is the cached result obtained from makeVector
        v <- x$getinverse() #retrieve the result from the inverse function above 
        if(!is.null(v)) { #checks to see whether the result is NULL
                message("getting cached data")
                return(v)
        } else {
                matX<- x$get() #calculates the inverse if the result is NULL using the cached getter
                v <- solve(matX)
                x$setinverse(v) #use the setter cached before
                v #returns the inverse matrix
        }
}