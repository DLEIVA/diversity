print.gini <- function(x,digits=max(4,getOption("digits")-4),...)
{
  cat("\n")
  cat("Diversity as disparity: Gini's Coefficient")
  cat("\n\n Call: \n")
  cat("",deparse(x$call), "\n\n")
  cat(" Data: ")
  cat( if (length(x$data) >= 5) x$data[1:5] else x$data[1:length(x$data)])
  cat(if (length(x$data) <= 5) "" else " ...", "\n")
  cat(" Minimum: ", x$min,"\n")
  cat(" Maximum: ", x$max,"\n")
  cat(" Gini's Coefficient (G): ")
  cat(formatC(x$gini, digits = digits), "\n")
  cat(" Maximum value of Gini's Coefficient (Gmax): ")
  cat(formatC(x$gini.max, digits = digits), "\n")
  cat("\n\n")
  invisible(x)
}

