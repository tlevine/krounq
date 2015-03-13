
#' Partially apply a function.
#' @param f The function
#' @param ... Arguments to the function
curry <- function(f, ...) {
  curried.args <- list(...)
  function(...) do.call(f, append(curried.args, list(...)))
}
