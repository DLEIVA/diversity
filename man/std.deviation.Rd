\name{std.deviation}
\alias{std.deviation}
\alias{print.deviation}

\title{
Diversity as separation: Standard Deviation
}
\description{
This function computes Standard Deviation for quantifying diversity as separation.
}
\usage{
std.deviation(X, min, max)
}

\arguments{
  \item{X}{A numeric vector with group data.}
  \item{min}{The minimum value for the random variable.}
  \item{max}{The maximum value for the random variable.}
}

\details{

 \code{std.deviation} computes Standard Deviation for a group. It also provides maximum values for the statistic and normalized Standard Deviation.
}

\value{
  The function returns a list of class \code{deviation} with following
  components: 
  \item{call}{Function call.}
  \item{data}{Original data vector.}
  \item{min}{Minimum value for the random variable.}
  \item{max}{Maximum value for the random variable.}
  \item{std.dev}{Standard Deviation.}
  \item{std.dev.max}{Maximum value of Standard Deviation.}
  \item{std.dev.norm}{Normalized value of Standard Deviation.}
}

\references{

Solanas, A., Selvam, R. M., Navarro, J., & Leiva, D. (2010). On the measurement of diversity in organizations. Unpublished manuscript.
}
\author{
Antonio Solanas, Rejina M. Selvam, Jose Navarro and David Leiva.
}

\examples{

g.3 <- c(1,1,9)
std.deviation(g.3,1,9)

g.4 <- c(2,4,5,6)
std.deviation(g.4,1,9)

g.5 <- c(rep(1,2),rep(9,3))
std.deviation(g.5,1,9)

g.10 <- c(rep(1,4),rep(9,5),2)
std.deviation(g.10,1,9)

}

\keyword{misc}
