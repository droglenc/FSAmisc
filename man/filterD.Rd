% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filterD.R
\name{filterD}
\alias{filterD}
\title{Subsets/filters a data frame and drops the unused levels.}
\usage{
filterD(x, ..., except = NULL)
}
\arguments{
\item{x}{A data frame.}

\item{\dots}{further arguments to be passed to \code{\link[dplyr]{filter}}.}

\item{except}{Indices of columns from which NOT to drop levels.}
}
\value{
A data frame with the filtered rows.
}
\description{
Subsets/filters a data frame and drops the unused levels.
}
\details{
Newbie students using R expect that when a factor variable is filtered with \code{\link[dplyr]{filter}} that any original levels that are no longer used after the filtering will be ignored. This, however, is not the case and often results in tables with empty cells and figures with empty bars. One remedy is to use \code{\link[base]{droplevels}} immediately following \code{\link[dplyr]{filter}}. This generally becomes a repetitive sequence for most newbie students; thus, \code{filterD} incorporate these two functions into one function.

\code{filterD} is a wrapper for \code{\link[dplyr]{filter}} from \pkg{dplyr} followed by \code{\link[base]{droplevels}} just before the data.frame is returned. Otherwise, there is no new code here.

This function is only used for data frames.
}
\section{IFAR Chapter}{
 Basic Data Manipulations.
}

\examples{
## The problem -- note use of unused level in the final table.
levels(iris$Species)
iris.set1 <- subset(iris,Species=="setosa" | Species=="versicolor")
levels(iris.set1$Species)
xtabs(~Species,data=iris.set1)

## A fix using filterD
iris.set3 <- filterD(iris,Species=="setosa" | Species=="versicolor")
levels(iris.set3$Species)
xtabs(~Species,data=iris.set3)

}
\seealso{
See \code{subset} and \code{\link[dplyr]{filter}} from \pkg{dplyr} for similar functionality. See \code{drop.levels} in \pkg{gdata} and \code{droplevels} for related functionality.
}
\author{
Derek H. Ogle, \email{derek@derekogle.com}
}
\keyword{misc}
