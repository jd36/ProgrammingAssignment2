## input x will be a vector
makeVector <- function(x = numeric()) {  
        ##  m will store our 'mean' and it's reset to NULL every
        ##    time makeVector is called
        
        ##  note these next three functions are not run when makeVector is called.
        ##   instead, they will be used by cachemean() to get values for x or for
        ##   m (mean) and for setting the mean.  These are usually called 
        ##      object 'methods'.
        m <- NULL 

        ## takes an input vector
        set <- function(y) {
                ## saves the input vector 
                x <<- y
                ## resets the mean to NULL, basically what happens when a new 
                ##  object is generated.
                m <<- NULL
        }
        
        ## this function returns the value of the original vector
        get <- function() x
        
        ## this is called by cachemean() during the first cachemean()
        ##  access and it will store the value using superassignment
        setmean <- function(mean) m <<- mean
        
        ## this will return the cached value to cachemean() on
        ##  subsequent accesses
        getmean <- function() m
        
        ##  This list is returned with the newly created object.
        list(set = set, get = get,
             ##   It lists all the functions ("methods") that are part of
             setmean = setmean,
             ##   the object.  If a function is not on the list then it cannot
             ##   be accessed externally.
             getmean = getmean)
}


## the input is an object created by makeVector
cachemean <- function(x, ...) {
        ## accesses the object 'x' and gets the value of the mean
        m <- x$getmean()
        ## if mean was already cached (not NULL) ...
        if(!is.null(m)) {
                ## ... send this message to the console
                message("getting cached data")
                ## ... and return the mean ... "return" ends
                ##   the function cachemean(), note
                return(m)
        }
        ## we reach this code only if x$getmean() returned NULL
        data <- x$get()
        ## if m was NULL then we have to calculate the mean
        m <- mean(data, ...)
        ## store the calculated mean value in x (see setmean() in makeVector)
        x$setmean(m)
        ## return the mean to the code that called this function
        m
}