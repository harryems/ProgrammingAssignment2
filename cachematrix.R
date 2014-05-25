## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

setClass("cachemean",representation(set = "function",get = "function", getmean = "function",setmean = "function"))

makeVector <- function(x) {
        data <- x
        datamean <- NULL
        get <- function() data
        set <- function(newvalue) {
                data <<- newvalue
                datamean <<- NULL
        }
        setmean <- function(newmean) datamean <<- newmean
        getmean <- function() datamean
        new("cachemean", set = set, get = get, setmean = setmean,
            getmean = getmean)
}

## An Object That Remembers its Mean
setMethod("show", "cachemean",
          function(object) {
                  x <- object$get()
                  show(x)
                  invisible(x)
          })
## An Object That set its Mean
setMethod("mean", "cachemean",
          function(x, ...) {
                  m <- x$getmean()
                  if(!is.null(m))
                          return(m)
                  data <- x$get()
                  m <- mean(data, ...)
                  x$setmean(m)
                  m
          })