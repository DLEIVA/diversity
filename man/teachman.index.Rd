\name{teachman.index}
\alias{teachman.index}
\alias{print.teachman}

\title{
Diversity as variety: Teachman's Index
}
\description{
This function computes Teachman's index for quantifying diversity as variety.
}
\usage{
teachman.index(X, categories)
}

\arguments{
  \item{X}{A string vector with categorical data.}
  \item{categories}{The number of posible categories for the random variable.}
}

\details{

 Teachman's index (1977) is defined as \eqn{- \sum\limits_{i = 1}^k {p_i }  \times \ln p_i}, where \eqn{p_i} corresponds to the proportion of group members in \emph{i}th category and \eqn{k} denotes the number of categories for an studied attribute. This index reaches its minimum value (\eqn{0}) when there are no differences among individuals regarding their classification in the variable of interest. The maximum value depends on the number of categories and on the fact that individuals can be evenly distributed in all categories. \code{teachman.index} also computes proper theoretical upper bound for Teachman's index as well as a normalized measure that allows researchers to get a measure that ranges from 0 to 1.
}

\value{
  The function returns a list of class \code{blau} with following
  components: 
  \item{call}{Function call.}
  \item{categories}{Levels of categorical variable.}
  \item{teachman.index}{Teachman's Index.}
  \item{teachman.max}{Maximum value of Teachman's Index.}
  \item{teachman.norm}{Normalized value of Teachman's Index.}
}

\references{
Teachman, J. D. (1980). Analysis of population diversity: Measures of qualitative variation. \emph{Sociological Methods & Research}, \emph{8}, 341-362.

Solanas, A., Selvam, R. M., Navarro, J., & Leiva, D. (2010). On the measurement of diversity in organizations. Unpublished manuscript.
}
\author{
Antonio Solanas, Rejina M. Selvam, Jose Navarro and David Leiva.
}

\examples{

g.3 <- c('F','A','P')
teachman.index(g.3,3)

g.4 <- c(rep('F',2),'A','P')
teachman.index(g.4,3)

g.5 <- c(rep('F',2),rep('A',2),'P')
teachman.index(g.5,3)

g.10 <- c(rep('F',6),rep('A',3),'P')
teachman.index(g.10,3)

}

\keyword{misc}
