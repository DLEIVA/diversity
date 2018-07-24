m.euclidean.dist <- function (X, min, max, method=c("biemann","harrison"))
{ 
  method <- match.arg(method)
  n <- length(X)
  if (method == "biemann")
  {
  sumdistances <- 0;
    for (i in 1:(n-1))
    {
      for (j in (i+1):n)
      {
        sumdistances <- sumdistances + abs(X[i]-X[j])
      }
    }
    med <- 2*sumdistances/(n**2-n);
    if (n %% 2 == 0) {med.max <- n*(max-min)/(2*(n-1))}
      else
      {
        med.max <- (n+1)*(max-min)/(2*n);
      }
  }
  if (method == "harrison")
  {
    sumdistances <- 0;
    for (i in 1:n)
    {
      sumdist <- 0.;
      for (j in 1:n)
      {
        sumdist <- sumdist + (X[i]-X[j])^2
      }
      sumdistances <- sumdistances + sqrt(sumdist);
    }
    med <- sumdistances/sqrt(n^3);
    if (n %% 2 == 0){med.max <- (max-min)/sqrt(2)}
      else
      {
        med.max <- (((sqrt((n**2-1)*(n+1))+sqrt((n**2-1)*(n-1))))/
         sqrt(4*n**3))*((max-min)/sqrt(2));
      }
   } 

   med.norm <- med/med.max
   res <- list(call=match.call(), method=method, data=X, min=min, max=max,
   med=med, med.max=med.max, med.norm=med.norm)
   class(res) <- "euclidean"
   res
}

