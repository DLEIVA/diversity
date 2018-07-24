blau.index <- function (X,categories)
{
   if (!is.character(X)) cat('ERROR: String vector should be specified in X.
      \n')
   else {
       blau.index <- 1-sum(prop.table(table(X))**2)
       n <- length(X)
       k <- categories
       a <- n - k * floor(n / k)
       blau.max <- (n**2*(k-1)+a*(a-k))/(k*n**2)
       blau.norm <- blau.index/blau.max
       res <- list(call = match.call(),categories = categories,
       blau.index = blau.index, blau.max = blau.max, blau.norm = blau.norm)
       class(res) <- "blau"
       res
   }
}

