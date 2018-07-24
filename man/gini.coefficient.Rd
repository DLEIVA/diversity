\name{gini.coefficient}
\alias{gini.coefficient}
\alias{print.gini}

\title{
Diversity as disparity: Gini's Coefficient
}
\description{
This function computes Gini's coefficient for quantifying diversity as disparity.
}
\usage{
gini.coefficient(X, min, max)
}

\arguments{
  \item{X}{A numeric vector with group data.}
  \item{min}{The minimum value for the random variable.}
  \item{max}{The maximum value for the random variable.}
}

\details{

 \code{gini.coefficient} computes Gini's Coefficient for a group according the following formula:
 \tabular{ll}{
	\tab \eqn{
  		G = {\Delta  \over {2\bar x}} = {{\sum\limits_{i = 1}^n {\sum\limits_{j = i + 1}^n {\left| {x_i  - x_j } \right|} } } \over {n^2 \bar x}}
	}. \cr
  }
  It also provides maximum values for Gini's Coefficient.
  }

\value{
  The function returns a list of class \code{gini} with following
  components: 
  \item{call}{Function call.}
  \item{data}{Original data vector.}
  \item{min}{Minimum value for the random variable.}
  \item{max}{Maximum value for the random variable.}
  \item{gini}{Gini's Coefficient.}
  \item{gini.max}{Maximum value of Gini's Coefficient.}
}

\references{

Solanas, A., Selvam, R. M., Navarro, J., & Leiva, D. (2010). On the measurement of diversity in organizations. Unpublished manuscript.
}
\author{
Antonio Solanas, Rejina M. Selvam, Jose Navarro and David Leiva.
}

\examples{

ex.1 <- c(30,rep(60,3))
gini.coefficient(ex.1,30,60)  

ex.2 <- c(60,rep(30,3))
gini.coefficient(ex.2,30,60)

ex.3 <- c(rep(30,2),rep(60,2))
gini.coefficient(ex.3,30,60)

# Gini coefficient does not behave as disparity index when minimum value is greater than 0. #
# Then, a translation of scale is suitable for solving this problem. #
ex.4 <- ex.1 - 30
gini.coefficient(ex.4,0,30) 

ex.5 <- ex.2 - 30
gini.coefficient(ex.5,0,30) 

ex.6 <- ex.3 - 30
gini.coefficient(ex.6,0,30)

}

\keyword{misc}
