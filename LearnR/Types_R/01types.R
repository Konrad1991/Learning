# Matrices
# ===============================================
m <- matrix(
  data = 0, # a single value is recycled until all 2 * 2 elements are filled
  nrow = 2,
  ncol = 2,
  byrow = FALSE # anyway the default
)
m
storage.mode(m) <- "character"
m # m now stores characters "0"

# Recycling
# ===============================================
# If data is shorter than nrow * ncol, R repeats it
matrix(1:2, nrow = 2, ncol = 3) # 1 2 is repeated column by column
# If the length does not fit, R only warns
matrix(1:4, nrow = 2, ncol = 3) # Warning: data length [4] is not a sub-multiple ...

# What does byrow = FALSE mean? (column-major order)
# ===============================================
m <- matrix(
  data = 1:6,
  nrow = 3,
  ncol = 2
)
m
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# The data is filled column by column: first column 1 2 3, then 4 5 6.
# This is called column-major order (also used by Fortran, MATLAB, Julia).
# C, C++ and numpy use row-major order by default.
matrix(1:6, nrow = 3, ncol = 2, byrow = TRUE)
#      [,1] [,2]
# [1,]    1    2
# [2,]    3    4
# [3,]    5    6
# byrow = TRUE only changes how data is filled in.
# Internally the matrix is still stored column by column.

# Indexing a matrix
# ===============================================
m[] # you get the entire matrix
m[,] # same as m[]
m[1, ] # only the first row
m[, 2] # only the second col
m[3, 2] # The last element

# Internally 'm' has one buffer to store all elements
# in this example it is [1, 2, 3, 4, 5, 6]
c(m)
nrow(m) # get the number of rows here 3
ncol(m) # get the number of cols here 2
# As all elements are stored linearly, row and col can be
# converted into one index:
r <- 3
col <- 2
m[r, col] # 6
m[r + (col - 1) * nrow(m)] # 6. Same element

# Going through all elements this way:
# First column
m[1 + 0 * nrow(m)]
m[2 + 0 * nrow(m)]
m[3 + 0 * nrow(m)]
# Second column
m[1 + 1 * nrow(m)]
m[2 + 1 * nrow(m)]
m[3 + 1 * nrow(m)]

# Attributes: a matrix is just a vector with dimensions
# ===============================================
# Attributes are additional information attached to an object
attributes(m) # $dim 3 2
v <- 1:6
attributes(v) # NULL. A plain vector has no attributes
dim(v) <- c(3, 2) # adds the attribute dim
v # v is now a matrix
identical(v, m) # TRUE
# Removing the attribute turns it back into a plain vector
dim(v) <- NULL
v # 1 2 3 4 5 6

# Other attributes we have already seen:
x <- c(a = 1, b = 2)
attributes(x) # $names "a" "b"
f <- factor("A", levels = LETTERS[1:4])
attributes(f) # $levels "A" "B" "C" "D" and $class "factor"

# Arrays
# ===============================================
# Basically the same as matrices but dimensions can be everything
array(1:3, dim = 3) # Basically a vector
array(1:4, dim = c(2, 2)) # Basically a matrix
array(1:24, dim = c(2, 3, 4)) # An array often called tensor
