#' @title Extract diagonals from a matrix.
#'
#' @description Extract diagonals from a matrix.
#'
#' @param x A matrix with more than one row AND more than one column.
#' @param which A single numeric that indicates which diagonal to extract. A value of zero extracts the main diagonal, whereas negative values extract diagonals from the upper triangle and positive values extract diagonals from the lower triangle. Diagonals further from the main diagonal have \code{which} values further from zero. If \code{is.null(which)}, then a matrix of diagonal indices for \code{which} is shown.
#' @param incl.labels A single string that indicates whether \code{"row"}, \code{"column"}, or no (\code{"none"}) labels from \code{x} should be returned with the values on the diagonal. Will return numeric values if the labels are all diagonal, otherwise character labels are returned.
#' @param val.name A single string to name the variable that contains the values from the diagonal in the returned data.frame.
#' @param label.name A single string to name the variable that contains the labels in the returned data.frame (see \code{incl.labels})
#'
#' @return A data.frame with one variable that contains the values from the chosen diagonal of \code{x} and, optionally, a second variable that contains the chosen labels for those values.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}, but relied heavily on \url{https://stackoverflow.com/a/27935808/1123933/}.
#'
#' @keywords manip
#'
#' @examples
#' ## Square numeric matrix
#' mat1 <- matrix(1:16,nrow=4)
#' colnames(mat1) <- LETTERS[1:ncol(mat1)]
#' rownames(mat1) <- 1:nrow(mat1)
#' mat1
#' diags(mat1,which=NULL)
#' diags(mat1)
#' diags(mat1,which=-1)
#' diags(mat1,which=2)
#' diags(mat1,incl.labels="row")
#' diags(mat1,which=2,incl.labels="row")
#' diags(mat1,which=2,incl.labels="col")
#' ( tmp <- diags(mat1,which=2,incl.labels="row",val.name="Freq",label.name="age") )
#' str(tmp)
#'
#' ## Rectangular numeric matrix
#' mat2 <- matrix(1:20,nrow=4)
#' colnames(mat2) <- LETTERS[1:ncol(mat2)]
#' rownames(mat2) <- 1:nrow(mat2)
#' mat2
#' diags(mat2,which=NULL)
#' diags(mat2,which=-1,incl.labels="row")
#' diags(mat2,which=2,incl.labels="row")
#' diags(mat2,which=-4,incl.labels="col")
#'
#' ## Rectangular character matrix
#' mat3 <- matrix(LETTERS[1:24],nrow=3)
#' colnames(mat3) <- letters[1:ncol(mat3)]
#' rownames(mat3) <- 1:nrow(mat3)
#' mat3
#' diags(mat3,which=NULL)
#' diags(mat3,which=-1,incl.labels="row")
#' diags(mat3,which=2,incl.labels="row")
#' diags(mat3,which=-4,incl.labels="col")
#'
#' @export
diags <- function(x,which=0,incl.labels=c("none","row","column"),
                  val.name="value",label.name="label") {
  ## check if matrix
  if (!is.matrix(x))
    STOP("'diags' only works with matrices.")
  if (nrow(x)==1 | ncol(x)==1)
    STOP("'x' must have more than 1 row and more than 1 column.")
  ## find indices of diagonals for the matrix
  ## idea from https://stackoverflow.com/a/27935808/1123933/
  ind <- row(x)-col(x)
  if (is.null(which)) { # nocov start
    ## Simply show the matrix of indices
    cat("Indices matrix corresponding to 'x'.\n")
    rownames(ind) <- rownames(x)
    colnames(ind) <- colnames(x)
    print(ind)
    cat("\n") # nocov end
  } else {
    ## extract diagonal from x according to which
    if (which>max(ind) | which<min(ind))
      STOP("The 'which' diagonal does not exist in 'x'.")
    res <- x[ind==which]
    ## handle adding names
    incl.labels <- match.arg(incl.labels)
    if (incl.labels=="row")
      res2 <- rownames(x)[apply(ind,MARGIN=1,FUN=function(x) any(x==which))]
    else if (incl.labels=="column")
      res2 <- colnames(x)[apply(ind,MARGIN=2,FUN=function(x) any(x==which))]
    else res2 <- NULL
    ## put together as data.frame and return
    if (!is.null(res2)) {
      suppressWarnings(tmp <- as.numeric(res2))
      if (all(!is.na(tmp))) res2 <- tmp
      res <- data.frame(res2,res,stringsAsFactors=FALSE)
      names(res) <- c(label.name,val.name)
    } else {
      res <- data.frame(res)
      names(res) <- c(val.name)
    }
    res
  }
}
