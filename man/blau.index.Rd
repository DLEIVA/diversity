\name{blau.index}
\alias{blau.index}
\alias{print.blau}

\title{
Diversity as variety: Blau's Index
}
\description{
This function computes Blau's index for quantifying diversity as variety.
}
\usage{
blau.index(X, categories)
}

\arguments{
  \item{X}{A string vector with categorical data.}
  \item{categories}{The number of posible categories for the random variable.}

}

\details{

 Blau's index (1977) is computed by means of the well-known formula: \eqn{1 - \sum\limits_{i = 1}^k {p_i^2 }}, where \eqn{p_i} corresponds to the proportion of group members in \emph{i}th category and \eqn{k} denotes the number of categories for an attribute of interest. This index quantifies the probability that two members randomly selected from a group would be in different categories. This index reaches its minimum value (\eqn{0}) when there is no variety, that is to say, when all individuals are classified in the same category. The maximum value depends on the number of categories and on the fact that individuals can be evenly distributed in all categories. \code{blau.index} also computes proper theoretical upper bound for Blau's index as well as a normalized measure that allows researchers to get a measure that ranges from 0 to 1.
}

\value{
  The function returns a list of class \code{blau} with following components: 
  
  \item{call}{Function call.}
  \item{categories}{Levels of categorical variable.}
  \item{blau.index}{Blau's Index.}
  \item{blau.max}{Maximum value of Blau's Index.}
  \item{blau.norm}{Normalized value of Blau's Index.}
       
}
\references{
Blau, P. M. (1977). \emph{Inequality and heterogeneity}. New York: Free Press.

Solanas, A., Selvam, R. M., Navarro, J., & Leiva, D. (2010). On the measurement of diversity in organizations. Unpublished manuscript.
}
\author{
Antonio Solanas, Rejina M. Selvam, Jose Navarro and David Leiva.
}

\examples{

g.3 <- c('F','A','P')
blau.index(g.3,3)

g.4 <- c(rep('F',2),'A','P')
blau.index(g.4,3)

g.5 <- c(rep('F',2),rep('A',2),'P')
blau.index(g.5,3)

g.10 <- c(rep('F',6),rep('A',3),'P')
blau.index(g.10,3)
}

\keyword{misc}
