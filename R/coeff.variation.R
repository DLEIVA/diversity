coeff.variation <- function(X,min,max)
{
   std.dev <- sqrt(sd(X)^2*(length(X)-1)/length(X))
   coef.var <- std.dev/mean(X)
   n <- length(X)
   coef.var.max <- sqrt(n-1)*(max-min)/((n-1)*min+max)
   coef.var.norm <- coef.var/coef.var.max
   res <- list (call=match.call(),data=X,min=min,max=max,cv=coef.var,
   cv.max=coef.var.max,cv.norm=coef.var.norm)
   class(res) <- "coeffvar"
   res
}

