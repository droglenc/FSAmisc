#' @name filterD
#'
#' @title Subsets/filters a data frame and drops the unused levels.
#'
#' @description Subsets/filters a data frame and drops the unused levels.
#'
#' @details Newbie students using R expect that when a factor variable is filtered with \code{\link[dplyr]{filter}} that any original levels that are no longer used after the filtering will be ignored. This, however, is not the case and often results in tables with empty cells and figures with empty bars. One remedy is to use \code{\link[base]{droplevels}} immediately following \code{\link[dplyr]{filter}}. This generally becomes a repetitive sequence for most newbie students; thus, \code{filterD} incorporate these two functions into one function.
#'
#' \code{filterD} is a wrapper for \code{\link[dplyr]{filter}} from \pkg{dplyr} followed by \code{\link[base]{droplevels}} just before the data.frame is returned. Otherwise, there is no new code here.
#'
#' This function is only used for data frames.
#'
#' @param x A data frame.
#' @param except Indices of columns from which NOT to drop levels.
#' @param \dots further arguments to be passed to \code{\link[dplyr]{filter}}.
#'
#' @return A data frame with the filtered rows.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @section IFAR Chapter: Basic Data Manipulations.
#'
#' @seealso See \code{subset} and \code{\link[dplyr]{filter}} from \pkg{dplyr} for similar functionality. See \code{drop.levels} in \pkg{gdata} and \code{droplevels} for related functionality.
#'
#' @keywords misc
#'
#' @examples
#' ## The problem -- note use of unused level in the final table.
#' levels(iris$Species)
#' iris.set1 <- subset(iris,Species=="setosa" | Species=="versicolor")
#' levels(iris.set1$Species)
#' xtabs(~Species,data=iris.set1)
#'
#' ## A fix using filterD
#' iris.set3 <- filterD(iris,Species=="setosa" | Species=="versicolor")
#' levels(iris.set3$Species)
#' xtabs(~Species,data=iris.set3)
#'
#' @rdname filterD
#' @export
filterD <- function(x,...,except=NULL) {
  .Deprecated(msg="'filterD' was removed from 'FSA'; please use 'droplevels' after 'subset' or 'dplyr::filter' for the same result (see fishR post from 26-May-2021).")
  res <- dplyr::filter(x,...)
  res <- droplevels(res,except)
  if (nrow(res)==0)
    FSA:::WARN("The resultant data.frame has 0 rows. Try str() on the result.\n")
  res
}
