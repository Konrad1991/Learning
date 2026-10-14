# Replacement functions with S3 classes
# =================================================
instance <- structure(list(width = 1L, height = 2L), class = "FooS3")
instance$width
instance$height

# Getter
width <- function(object) UseMethod("width")
width.FooS3 <- function(object) object$width

height <- function(object) UseMethod("height")
height.FooS3 <- function(object) object$height

width(instance)
height(instance)

# Setter (replacement functions)
`width<-` <- function(object, value) UseMethod("width<-")
`width<-.FooS3` <- function(object, value) {
  object$width <- value
  return(object) # Mandatory!!!
}

`height<-` <- function(object, value) UseMethod("height<-")
`height<-.FooS3` <- function(object, value) {
  object$height <- value
  return(object) # Mandatory!!!
}

width(instance) <- 15L
height(instance) <- 30L
instance

# Why is return(object) mandatory?
# =================================================
# R rewrites `width(instance) <- 15L` into:
instance <- `width<-`(instance, value = 15L)
# So it is just `<-` again. The modified copy of
# instance has to be returned (copy-on-modify).

# No type check in S3
width(instance) <- "fifteen" # Works
instance$width

# Built-in replacement functions work the same way
# =================================================
x <- c(1, 2)
names(x) <- c("a", "b")
# R rewrites it into:
x <- `names<-`(x, value = c("a", "b"))
x
`names<-` # function (x, value)  .Primitive("names<-")

# Dots in names
# =================================================
# S3 dispatch is purely based on names: generic.class
# So a function with a dot in its name can accidentally become a method
print.result <- function(x, ...) {
  cat("I am only a helper function\n")
}
res <- structure(1, class = "result")
res # I am only a helper function
# Auto-printing calls print(), which dispatches to print.result

# Same for t.test: it looks like the method of t() for class "test"
obj <- structure(list(), class = "test")
t.test <- function(x) "t.test was called"
t(obj) # "t.test was called"
rm(t.test)
# Therefore, avoid dots in names and use snake_case: print_result
