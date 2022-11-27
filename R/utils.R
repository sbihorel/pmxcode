#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Multi-argument version of isTruthy
#'
#' See \code{shiny::\link[shiny:isTruthy]} for details.
#'
#' @name areTruthy
#'
#' @param ... Any object
#'
#' @return TRUE if all objects are "truthy", FALSE if at least one is not.

areTruthy <- function(...){

  all(sapply( list(...), shiny::isTruthy ))

}

#' Opposite of areTruthy
#'
#' @name notTruthy
#'
#' @param ... Any object
#'
#' @return TRUE if at least one object is not "truthy", FALSE if all are.


notTruthy <- function(...){

  !areTruthy(...)

}

#' Lower part of a matrix (including diagoncal element)
#'
#' @name bottom_triangle
#' @param x A matrix
#' @return A matrix of logicals identifying the elements of x which part of the
#' lower triangle and the diagonal of x

bottom_triangle <- function(x){
  lower.tri(x) | as.logical(diag(nrow(x)))
}

#' Convert of covariance matrix into a 0/1 correlation map table
#' @name get_correlation_table
#' @param x A covariance matrix
#' @param na_zero Logical indicating whether NA should be replaced by 0's
#' @return A matrix of 0 and 1

get_correlation_table <- function(x, na_zero = FALSE){

  if ( na_zero ){
    x[is.na(x)] <- 0
  }

  x[bottom_triangle(x)] <- ifelse(
    is.na(x[bottom_triangle(x)]),
    0,
    x[bottom_triangle(x)]
  )
  x[!is.na(x) & x != 0] <- 1
  x
}

#' Determine if a square matrix is of type block, band or error
#'
#' @param x A square matrix
#' @return Either "error", "band", or "block"
#

is_EDB <- function(x){

  if ( !inherits(x, "matrix") ){
    return("error")
  }
  if ( nrow(x) != ncol(x) ){
    return("error")
  }

  n <- nrow(x)

  if ( n == 1 ){
    return("diagonal")
  }

  x[lower.tri(x)] <- ifelse(
    x[lower.tri(x)] > 0,
    1,
    x[lower.tri(x)]
  )

  if ( all(x[lower.tri(x)] == 0) ){
    return("diagonal")
  }

  if ( all(x[lower.tri(x)] == 1) ){
    return("block")
  }

  # Check values by diagonal: NA indicates from than one value is present in a diagonal
  tmp <- vector("numeric", n-1)

  for ( i in 2:n ){
    values <- diag(x[i:n, 1:(n-i+1), drop = FALSE])
    tmp[i-1] <- ifelse(
      length(unique(values)) > 1,
      NA,
      unique(values)
    )
  }

  # There must be no NA and only decreasing values to be a valid block
  if ( any(is.na(tmp)) | any(diff(tmp) > 0) ){
    return("error")
  } else {
    return("block")
  }

}

