# Most languages do:
# =================================================
a = 1

# In R there is an alternative:
# =================================================
a <- 1

# You can write it the other way around:
# =================================================
2 -> a # The value 2 is assigned to a
# It is very rarely used. Even though it matches our
# reading direction better: `a <- 1` is read as
# "1 is assigned to a", so we look at the rhs first
# and then at the lhs. But we read from left to right.

# Another feature of having `<-`
# =================================================
f <- function(obj) {
  return(obj + 1)
}
f(obj <- 1) # 2
f(obj = a) # 3
# It is possible to differentiate between named arguments
# and it is also possible to use `<-` within a function call
# to define a variable.
# This looks strange and I would not recommend it to
# use it if you write R code. But one often sees this pattern:
# summary(fit <- lm(...))
#
# Moreover:
# f(j <- obj = 1) # Error: unexpected '='
f(obj = j <- 1) # Works

# Assignment via a function
# =================================================
assign("foo", 3)
foo

(a <- 5) # parentheses print the value
a <- 5 # plain <- returns the value invisibly

# Types can change: values have types, variables don't
a <- TRUE # logical
a <- 1L # integer
a <- 3.1415 # double
a <- "Hello world" # character
a <- 4 + 3i # complex

# The special values T and F
# By default T and F are short forms of TRUE and FALSE
# But they can be overwritten
T <- 3
T # Therefore, always prefer the long forms
# The same is true for some constants for example pi
pi
pi <- 3
pi
# Remove them to get the base versions back
rm(T, pi)
T # TRUE
pi # 3.141593

# It is possible to chain assignments
a <- b <- c <- 3
a
b
c
# c is now 3 but c() still works.
# R skips non-functions when it looks up a function name
c(1, 2) # 1 2

# You can use characters at the left side
# This is due to historical reasons. Very uncommon
"a" <- 5
a
# Backticks allow non-syntactic names
`my var` <- 4
`my var`
# If you want to get rid of a variable use 'rm'
rm(`my var`)
exists("my var")
