#' @title Performs a hypothesis test that a linear model parameter is equal to a specific value.
#'
#' @description Performs a hypothesis test that a linear model parameter is equal to a specific value. Useful for testing that a parameter is equal to a value other than 0.
#'
#' @details The \dQuote{direction} of the alternative hypothesis is identified by a string in the \code{alt} argument.
#'
#' If the \code{lm} object is from a simple linear regression with an intercept then \code{term=1} will use the intercept and \code{term=2} will use the slope in the hypothesis test.
#'
#' @param object A \code{lm} object.
#' @param term A single numeric that indicates which term in the model to use in the hypothesis test.
#' @param bo The null hypothesized parameter value.
#' @param alt A string that identifies the \dQuote{direction} of the alternative hypothesis. The strings may be \code{"less"} for a \dQuote{less than} alternative, \code{"greater"} for a \dQuote{greater than} alternative, or \code{"two.sided"} (DEFAULT) for a \dQuote{not equals} alternative.
#'
#' @return A matrix that contains the term number, hypothesized value, parameter estimate, standard error of the parameter estimate, t test statistic, degrees-of-freedom, and corresponding p-value.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso \code{\link{htest.nlsBoot}}.
#'
#' @keywords htest
#'
#' @examples
#' data(Mirex)
#' # Simple linear regression test HA:slope!=0.1
#' lm1 <- lm(mirex~weight, data=Mirex)
#' hoCoef(lm1,2,0.1)
#'
#' @export
hoCoef <- function(object,term=2,bo=0,alt=c("two.sided","less","greater")) {
  alt <- match.arg(alt)
  if (!"lm" %in% class(object))
    FSA:::STOP("'object' must be from 'lm'.")
  if (!term>0)
    FSA:::STOP("'term' must be a positive number.")
  tmp <- summary(object)$coefficients
  if (term>length(rownames(tmp)))
    FSA:::STOP("'term' is greater than number of terms in the model.")
  est <- tmp[term,"Estimate"]
  se <- tmp[term,"Std. Error"]
  t <- (est-bo)/se
  df <- object$df.residual
  switch(alt,
         less=     { p.value <- stats::pt(t,df,lower.tail=TRUE) },
         greater=  { p.value <- stats::pt(t,df,lower.tail=FALSE) },
         two.sided={ p.value <- 2*stats::pt(abs(t),df,lower.tail=FALSE) }
  )
  res <- cbind(term,bo,est,se,t,df,p.value)
  colnames(res) <- c("term","Ho Value","Estimate","Std. Error","T","df","p value")
  rownames(res) <- ""
  res
}

