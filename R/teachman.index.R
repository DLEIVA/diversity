teachman.index <- function (X,categories)
{
   if (!is.character(X)) cat('ERROR: String vector should be specified in X.
       \n')
   else {
       teachman.index <- -sum(prop.table(table(X))*log(prop.table(table(X))))
       n <- length(X)
       k <- categories
       a <- n - k * floor(n / k)  
       teachman.max <- -1/(k*n)*((k*n-k*a-n*a+a**2)*log(n-a)+(n*a+k*a-a**2)*
       log(n-a+k)-k*n*log(k*n))
       teachman.norm <- teachman.index/teachman.max
       res <- list(call = match.call(),categories = categories,
       teachman.index = teachman.index,teachman.max = teachman.max,
       teachman.norm = teachman.norm)
       class(res) <- "teachman"
       res
   }
}

