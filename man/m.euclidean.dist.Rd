\name{m.euclidean.dist}
\alias{m.euclidean.dist}
\alias{print.euclidean}


\title{
Diversity as separation: mean Euclidean distance
}
\description{
This function computes mean Euclidean distance for quantifying diversity as separation.
}
\usage{
  m.euclidean.dist(X, min, max, method = c("biemann", "harrison"))
}

\arguments{
  \item{X}{A numeric vector with group data.}
  \item{min}{The minimum value for the random variable.}
  \item{max}{The maximum value for the random variable.}
  \item{method}{String vector. 2 possible values: \code{"biemann"} and \code{"harrison"}}
}

\details{

 Mean Euclidean distance (MED) is defined as the square root of the mean squared differences between the \emph{i}th member and all others in the group. The minimum value is zero and the maximum value depends on the parity of the group size. \code{m.euclideand.dist} provides statistic value computed by means of the empirical data, theoretical maximum value and normalized value.
 In order to compute statistic, the function uses the formula provided by Harrison and Klein (2007):
  \tabular{ll}{
	\tab \eqn{
            MED = {{\sum\limits_{i = 1}^n {\sqrt {\sum\limits_{j = 1}^n {\left( {x_i  - x_j } \right)^2 } /n} } } \over n} 
            }. \cr
  }
 
 The function also includes the formula proposed by Biemann and Kearney (2010):
 
  \tabular{ll}{
	\tab \eqn{ 
            MED = {{\sum\limits_{i = 1}^n {\sum\limits_{j = 1}^n {{{\sqrt {\left( {x_i  - x_j } \right)^2 } } \over {n - 1}}} } } \over n}
           }. \cr
  }
}

\value{
  The function returns a list of class \code{deviation} with following
  components: 
  \item{call}{Function call.}
  \item{method}{Method used for computing MED.}
  \item{data}{Original data vector.}
  \item{min}{Minimum value for the random variable.}
  \item{max}{Maximum value for the random variable.}
  \item{med}{Mean Euclidean distance.}
  \item{med.max}{Maximum value of mean Euclidean distance.}
  \item{med.norm}{Normalized value of mean Euclidean distance.}
}

\references{
Biemann, T., & Kearney, E. (2010). Size does matter: How varying group sizes in a sample affect the most common measures of group diversity, \emph{Organizational Research Methods}, \emph{3}, 582-599.

Harrison, D. A., & Klein, K. J. (2007). What's the difference? Diversity constructs as separation, variety, or disparity in organizations. \emph{Academy of Management Review}, \emph{32}, 1199-1228.
 
Solanas, A., Selvam, R. M., Navarro, J., & Leiva, D. (2010). On the measurement of diversity in organizations. Unpublished manuscript.
}
\author{
Antonio Solanas, Rejina M. Selvam, Jose Navarro and David Leiva.
}

\examples{

g.3 <- c(1,1,9)
m.euclidean.dist(g.3,1,9,'b')
m.euclidean.dist(g.3,1,9,'h')

g.4 <- c(2,4,5,6)
m.euclidean.dist(g.4,1,9,'biem')
m.euclidean.dist(g.4,1,9,'har')

g.5 <- c(rep(1,2),rep(9,3))
m.euclidean.dist(g.5,1,9,method='biemann')
m.euclidean.dist(g.5,1,9,method='harrison')

g.10 <- c(rep(1,4),rep(9,5),2)
m.euclidean.dist(g.10,1,9,'biemann')
m.euclidean.dist(g.10,1,9,'harrison')

}

\keyword{misc}

