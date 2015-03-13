

#' Merge some instrument curves
#' @param ... functions from duration to air speaker position wave numbers
#' @examples
#'   merge(curry(sine, 440), curry(sine, 220))(100)
merge <- function(...) {
  add <- function(a,b) a + b 
  function(duration) {
    f <- function(instrument) instrument(duration)
    Reduce(add, lapply(list(...), f))
  }
}
