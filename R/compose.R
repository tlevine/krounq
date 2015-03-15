#' Generate frequencies in 12-tone equal temperment (12-TET)
#' @param n This note number
#' @param P.a Base frequency
#' @param a Base note number
#' @examples
#'   P.n(40)
#'   P.n(c(40, 42, 43))
P.n <- absolute.frequency <- function(n, P.a = 440, a = 49)
  P.a * (2^(1/12))^(n - a)


#' Invert a chord
#' @param inversion 0 for base, 1 for first inversion, 2 for second inversion
invert.chord <- function(chord, full.inversion) {
  inversion <- full.inversion %% 3
  octave.shift <- (full.inversion - inversion) / 3

  bottom <- chord[(1+inversion):length(chord)]
  top <- chord[0:inversion] + intervals$P8
  octave <- intervals$P8 * octave.shift

  # Notes stay in the same order but are shifted.
  # This makes it easy to select the root note.
  c(top, bottom) + octave
}


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
