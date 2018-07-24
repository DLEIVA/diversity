gini.coefficient <- function(X,min,max)
{
   delta <- 0.
   n <- length(X)
   for (i in 1:(n-1))
      for (j in (i+1):n)
      {
         delta <- delta + abs(X[i]-X[j]);
      }
   gini.coefficient <- delta/(n^2*mean(X))
   gini.coefficient.max <- 1-1/n
   res <- list (call=match.call(),data=X,min=min,max=max,gini=gini.coefficient,
   gini.max=gini.coefficient.max)
   class(res) <- "gini"
   res
}

