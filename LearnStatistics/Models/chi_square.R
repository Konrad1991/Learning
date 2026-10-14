set.seed(1234)

# chi^2 with k = 1
Z <- rnorm(100000)
U <- Z^2
mean(U) # 1 = k
var(U) # 2 = 2*k
hist(Z)
hist(U)

# chi^2 with k = 3
Z <- matrix(rnorm(300000), ncol = 3)
U <- rowSums(Z^2)
mean(U) # 3 = k
var(U) # 6 = 2*k
hist(Z)
hist(U)


# chi^2 with k = 1000
k <- 1000
Z <- matrix(rnorm(3000000), ncol = k)
U <- rowSums(Z^2)
mean(U) # 1000 = k
var(U) # 2000 = 2*k
hist(Z)
hist(U) # the higher k the more similar it looks like a normal distribution
