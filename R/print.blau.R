print.blau <- function (x,digits = max(4,getOption("digits") - 4),...)
{
  cat("\n")
  cat("Diversity as variety: Blau's Index")
  cat("\n\n Call: \n")
  cat("",deparse(x$call), "\n\n")
  cat(" Levels: ",x$categories,"\n")
  cat(" Blau's Index (B): ")
  cat(formatC(x$blau.index, digits = digits), "\n")
  cat(" Maximum value of Blau's Index (Bmax): ")
  cat(formatC(x$blau.max, digits = digits), "\n")  
  cat(" Normalized value of Blau's Index (Bs): ")
  cat(formatC(x$blau.norm, digits = digits), "\n")  
  cat("\n\n")
  invisible(x)
}

