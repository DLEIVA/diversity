print.teachman <- function (x,digits = max(4,getOption("digits") - 4),...)
{
  cat("\n")
  cat("Diversity as variety: Teachman's Index")
  cat("\n\n Call: \n")
  cat("",deparse(x$call), "\n\n")
  cat(" Levels: ",levels(factor(x$categories)),"\n")
  cat(" Teachman's Index (H): ")
  cat(formatC(x$teachman.index, digits = digits), "\n")
  cat(" Maximum value of Teachman's Index (Hmax): ")
  cat(formatC(x$teachman.max, digits = digits), "\n")  
  cat(" Normalized value of Teachman's Index (Hs): ")
  cat(formatC(x$teachman.norm, digits = digits), "\n")  
  cat("\n\n")
  invisible(x)
}

