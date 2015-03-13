library(tuneR)

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


#' Produce sounds with overtones
#' @param multiples Multiples of the base frequency for overtones
#' @param generator sine, sawtooth, square
#' @param frequency Frequency (440 is middle A.)
#' @param duration Duration in samples (probably 44100 samples per second)
#' @examples
#'   # A plain sine wave
#'   all(overtones(1, sine, 440, SECOND) == sine(440, SECOND))
#'
#'   # A harmonic series
#'   overtones(1:5, sine, 440, SECOND)
#'
#'   # An inharmonic series
#'   overtones(runif(5, 1, 3), sine, 440, SECOND)
overtones <- function(multiples, generator, frequency, duration) {
  add <- function(a, b) a + b
  f <- function(x) generator(frequency * x, duration)
  Reduce(add, lapply(multiples, f)) / length(multiples)
}
