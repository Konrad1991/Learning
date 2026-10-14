# Copy on modification
x <- c(1, 2)
y <- x
tracemem(x)
y[1] <- 10 # copy happens only now
# Therefore, it is completely ok to pass large objects to functions
# as long as you do not modify them

# Reference semantics via environments
e <- new.env(parent = emptyenv())
e$x <- 1
f <- function(e) {
  e$x <- 100
}
f(e)
e$x

# delayed assignments. The assignment happens when the object is required
delayedAssign(
  "big", {
    cat("Assignment happens to big\n")
    numeric(1e6)
  }
)
exists("big")
length(big)
# > length(big)
# Assignment happens to big
# [1] 1000000
# This is for example used for all the datasets which can be directly used in R
# so stuff like: iris, CO2, mtcars are loaded when the data is required

# Make active binding: basically you say a function is called always when a
# variable is used.
e <- new.env(parent = emptyenv())
val <- NULL
makeActiveBinding(
  "x", function(value) {
    if (missing(value)) {
      print("x is read")
      val
    } else {
      print("x is set")
      val <<- value + 1
    }
  }, e
)
val # NULL
e$x <- 5
val # 6
e$x <- 6
val # 7
e$x # 7

# If you want to prevent the modification of a variable you can use lockBinding
secret <- 100L
secret <- 101L # still modifiable
lockBinding("secret", globalenv())
# secret <- 102L # Error
unlockBinding("secret", globalenv())
secret <- 102L
secret
