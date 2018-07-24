\name{coeff.variation}
\alias{coeff.variation}
\alias{print.coeffvar}

\title{
Diversity as disparity: Coefficient of Variation
}
\description{
This function computes Coefficient of Variation for quantifying diversity as disparity.
}
\usage{
coeff.variation(X, min, max)
}

\arguments{
  \item{X}{A numeric vector with group data.}
  \item{min}{The minimum value for the random variable.}
  \item{max}{The maximum value for the random variable.}
}

\details{

 Coefficient of Variation can be obtained by means of the ratio \eqn{sd(X)/mean(X)}. Coefficient of Variation is is lower and upper bounded, its minimum and maximum values being respectively equal to zero and \eqn{(n-1)^{1/2}}. This maximum value can be obtained only if minimum value equals 0. \code{coef.variation} provides proper maximum values for Coefficient of Variation taking into account any minimum value. Furthermore it computes normalized Coefficient of Variation that ranges from 0 to 1.
}

\value{
  The function returns a list of class \code{coeffvar} with following
  components: 
  \item{call}{Function call.}
  \item{data}{Original data vector.}
  \item{min}{Minimum value for the random variable.}
  \item{max}{Maximum value for the random variable.}
  \item{cv}{Coefficient of Variation.}
  \item{cv.max}{Maximum value of Coefficient of Variation.}
  \item{cv.norm}{Normalized value of Coefficient of Variation.}
}

\references{

Solanas, A., Selvam, R. M., Navarro, J., & Leiva, D. (2010). On the measurement of diversity in organizations. Unpublished manuscript.
}
\author{
Antonio Solanas, Rejina M. Selvam, Jose Navarro and David Leiva.
}

\examples{

ex.1 <- c(30,rep(60,3))
coeff.variation(ex.1,30,60)

ex.2 <- c(60,rep(30,3))
coeff.variation(ex.2,30,60)

ex.3 <- c(rep(30,2),rep(60,2))
coeff.variation(ex.3,30,60)

}

\keyword{misc}
