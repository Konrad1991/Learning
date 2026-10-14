# The simplest types in R are vectors
# ===============================================
#
# Creation of length 1 vectors
# ===============================================
# - scalar values are just vectors of length 1
a <- TRUE # logical
a <- 1L # integer; if you write 1 it is handled as a double. Add the L (literal) to define a scalar literal integer
a <- 3.1415 # double
a <- "Hello world" # character
a <- 4 + 3i # complex
a <- as.raw(255) # raw. But there is no literal definition for this
a <- factor("A", levels = LETTERS[1:4]) # A factor is a string represented by an integer.
# It has some similarities to enums
typeof(a) # integer. A factor is not a type of its own
class(a) # factor. The class attribute decides how it is printed and handled

a <- 1.5

# What type does my variable has?
typeof(a) # double
class(a) # numeric

# If you want to know how your data is structured
str(a) # num 1.5

# Create longer vectors
# ===============================================
# Preallocate
logis <- logical(5) # 5 values which are all set to FALSE
integers <- integer(3) # 3 values which are all 0
doubles <- double(2) # 2 values which are all 0.0
nums <- numeric(5) # same as doubles
cmps <- complex(2) # 2 values which are all 0 + 0i
raws <- raw(3) # 3 raws value which are all 00

# Short course about subsetting
# subsetting with []
logis[1] <- TRUE # set first value to TRUE. R starts counting at 1
logis[0] <- TRUE # This just does nothing. Also no error is thrown!
logis[0] # logical(0). Thus, it returns an empty vector holding 0 logical elements
logis[7] # reading outside of the preallocated vector results in NA. No Error is thrown!
length(logis) # 5 reading above 5 does not change the variable. It has still a length of 5
logis[7] <- FALSE
length(logis) # 7 now the vector has grown
# But the sixth element contains now NA

# Mechanisms to append
# ===============================================
v <- logical() # creates empty vector
v[1] <- TRUE # I personally find it dangerous
v <- c(v, FALSE) # the function 'c' means concatenate
v <- append(v, TRUE)
v

# append can also insert at any position
append(1:3, 99, after = 1) # 1 99 2 3
append(1:3, 99, after = 0) # 99 1 2 3
c(99, 1:3) # same as after = 0

# Growing by setting the length
v <- c(TRUE, FALSE)
length(v) <- 4
v # TRUE FALSE NA NA. New elements are filled with NA

# Named vectors grow silently
v <- c(a = 1, b = 2)
v["c"] <- 3 # "c" does not exist, so it is appended. No error is thrown!
v # a b c
# A typo in a name therefore creates a new element instead of an error
v["A"] <- 10
v # a b c A

# Alternative function 'vector'
# ===============================================
lgls <- vector(mode = "logical", length = 3)
ints <- vector(mode = "integer", length = 3)
ds <- vector(mode = "double", length = 3)
nums <- vector(mode = "numeric", length = 3)
cs <- vector(mode = "complex", length = 3)
rs <- vector(mode = "raw", length = 3)
chars <- vector(mode = "character", length = 3)

# What type do we have
# ===============================================
is.logical(lgls) # TRUE
is.numeric(lgls) # FALSE

is.numeric(ints) # TRUE
is.double(ints) # FALSE

is.numeric(ds) # TRUE
is.double(ds) # TRUE

is.numeric(cs) # FALSE
is.complex(cs) # TRUE

is.numeric(rs) # FALSE
is.raw(rs) # TRUE

is.numeric(nums) # TRUE
is.double(nums) # TRUE

is.character(chars) # TRUE

# R is weakly typed
# ===============================================
# Types are automatically casted if a more flexible type is required
a <- as.raw(12); str(a) # a is of type raw
a <- c(a, TRUE); str(a) # a is of type logical
a <- c(a, 1L); str(a) # a is of type integer
a <- c(a, 3.1415); str(a) # a is of type double
a <- c(a, 3 + 3i); str(a) # a is of type complex
a <- c(a, "Hello"); str(a) # a is now of type character
