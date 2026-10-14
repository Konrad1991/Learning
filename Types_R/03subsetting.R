# Preserved vs simplified subsetting
# ===============================================
v <- c(1, 2, 3)
v[4] <- 5 # We append to our vector
# Using [] is called preserving subsetting
# Thus, whatever [] is returning has the same type and keeps the attributes
v
# The alternative is simplifying subsetting which is done with [[]]
# For atomic vectors the type stays the same, but attributes like names are dropped
x <- c(a = 1)
x["a"] # a 1 --> preserved, the name is kept
x[["a"]] # 1 --> simplified, the name is dropped
# v[[6]] # Using [[]] outside of the boundaries will result in an error; But only for reading
v[[6]] <- 3 # In contrast this works, without any error
v # 1 2 3 5 NA 3. The fifth element is silently filled with NA
v[60] # This works but it does not extend the length of the vector

# [] can be used to get more than one element
v[1:3] # 1:3 is the same as c(1, 2, 3)
# v[[1:3]] # error as [[]] is used to get one element but not more

# drop: simplifying with [] for matrices
# ===============================================
m <- matrix(1:6, nrow = 3, ncol = 2)
m[1, ] # 1 4. A plain vector. The dim attribute is dropped silently
str(m[1, ]) # int [1:2] 1 4
m[1, , drop = FALSE] # stays a 1 x 2 matrix --> preserved
str(m[1, , drop = FALSE]) # int [1, 1:2] 1 4
# drop = TRUE is the default. So [] simplifies for matrices!

# drop: simplifying with [] for arrays
# ===============================================
a <- array(1:24, dim = c(2, 3, 4))
dim(a[1, , ]) # 3 4. One dimension is dropped --> a matrix
a[1, 2, ] # 3 9 15 21. Two dimensions are dropped --> a plain vector
dim(a[1, 2, ]) # NULL
a[1, , , drop = FALSE] # stays an array --> preserved
dim(a[1, , , drop = FALSE]) # 1 3 4
dim(a[1, 2, , drop = FALSE]) # 1 1 4
# Every dimension of length 1 is dropped, no matter how many there are

# Empty subsets fail silently
# ===============================================
a <- array(1:4, dim = c(2, 2))
# Logical indices are recycled: FALSE selects no row, TRUE selects every column
nonsense <- a[FALSE, TRUE] # This works without an error
length(nonsense) # 0
dim(nonsense) # 0 2. Still a matrix! drop only removes dimensions of length 1, not 0
nonsense + 1 # This works without an error and returns again an empty matrix
sum(nonsense) # 0
mean(nonsense) # NaN
# Thus, the empty result travels silently through the following calculations
# nonsense + a # Error: non-conformable arrays
other_nonsense <- array(dim = c(0, 2))
nonsense + other_nonsense # This works as both have the dimensions 0 2

# The difference between preserved and simplified is important for lists
# ===============================================
l <- list(1:3, 4:6, 7:10)
str(l[1]) # A list of length 1 which holds one element 1, 2, 3
str(l[[1]]) # an integer vector 1, 2, 3 --> simplified: list -> integer vector

# Assigning NULL deletes an element
# ===============================================
l <- list(x = 1, y = 2)
l$x <- NULL # x is removed. No NULL is stored!
str(l) # List of 1: $ y
# To store NULL as an element use preserving subsetting
l["x"] <- list(NULL)
str(l) # List of 2: $ y: num 2, $ x: NULL
