magic_boots <- function(n, R = 500) {
    xdf <- vector("numeric", R)
    x <- rnorm(n)
    for (i in 1:R) {
        x_b <- sample(x, n, T)
        xdf[i] <- sum(x %in% x_b)/n
    }
    return(xdf)
}

mean(magic_boots(1000)) 
