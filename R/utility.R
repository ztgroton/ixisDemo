
#' Check if vector contains valid column names
#'
#' @param data data.frame
#' @param x character
#' @param scalar logical
#'
#' @return logical
#' @export
#'
#' @examples
#' \dontrun{
#'  is.colname(data.frame(a = 1, b = 2), x = c('a', 'b'))
#'  is.colname(data.frame(z = 1), x = c('z'), scalar = TRUE)
#' }
is.colname <- function(data, x, scalar = FALSE) {
  
  # validate inputs 
  stopifnot(is.data.frame(data))
  stopifnot(is.character(x))
  stopifnot(isTRUE(identical(scalar, TRUE)) || isTRUE(identical(scalar, FALSE)))
  
  # check if 'x' is subset of 'colnames(data)'
  res <- isTRUE(all(x %in% colnames(data)))
  
  # optionally check if 'length(x)' equals one
  if (isTRUE(scalar)) {res <- res && isTRUE(length(x) == 1)}
  
  # return result
  return(res)
  
}
