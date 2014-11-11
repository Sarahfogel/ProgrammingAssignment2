## These two functions allow the inverse of a matrix to be cached
## so that it does not need to be re-calculated every time it is called
## First, store the results of the makeCacheMatrix into an object, then pass
## that object to the cacheSolve function as an argument

## The makeCacheMatrix function creates a special list containing four entries
## All four entries are functions
## The first entry sets a particular matrix as the target matrix
## the second entry allows you to retrieve the current set matrix
## the third entry sets the inverse (it must be calculated 
##       elsewhere, it is only stored here)
## the fourth entry allows you to retrieve the set inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL  #start by erasing any previous inverse
    set <- function(y) {  #set the matrix with set(matrix)
        x<<-y            # replace the x in the makeCacheMatrix function with y
        inverse<<-NULL   # ensure no inverse is set in makeCacheMatrix function
    }
    get <- function() {x}  #function returns the current value of x
    setinv <- function(inv) inverse<<-inv #pass the function the matrix that 
                                          #serves as the inverse and set that 
                                          #in the larger function as the inverse
    getinv <- function() inverse #return current value of inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function checks if there is a value stored as the inverse of a matrix 
## in the list created by the makeCacheMatrix function.  If there is, it 
## retrieves and returns that. If there isn't, it calculates the inverse and 
## stores it into the list.

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()  #get whatever is stored as the inverse
    if(!is.null(inverse)){   #if that value is not null (i.e. an inverse stored)
        message("getting cached data")
        return(inverse)           #return the stored inverse and exit function
    }
    
    ## if there was no inverse stored, calculate one
    data <- x$get()   # get the target matrix
    inverse <- solve(data)  #invert matrix
    x$setinv(inverse)       # use function stored in the special list from 
                            # above to store the inverse
    inverse                 # print the inverse
}
