std.deviation <- function (X,min,max)
{
  std.deviation <- sqrt(sd(X)^2*(length(X)-1)/length(X)) 
  if (length(X) %% 2 == 0) {std.dev.max <- (max-min)/2}
    else
       {
        n <- length(X);
        std.dev.max <- sqrt((n^2-1)/(n^2))*((max-min)/2);
       }
  std.dev.norm <- std.deviation/std.dev.max
  res <- list(call=match.call(), data=X, min=min, max=max, 
  std.dev= std.deviation, std.dev.max=std.dev.max, std.dev.norm=std.dev.norm)
  class(res) <- "deviation"
  res 
}

