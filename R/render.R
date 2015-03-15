SECOND <- 44100

#' Generate a tuneR wave from an instrument
#' @param duration Duration in samples (probably 44100 samples per second)
#' @param left channel wave
#' @param right channel wave
wave <- function(left = numeric(0), right = numeric(0),
                 sampling.rate = SECOND, bit = 16,
                 fade = fade.quadratic)
  tuneR::Wave(left, right,
              samp.rate = sampling.rate, bit = bit)

#' Play a one-second clip of the function, if no player is specified, or
#' pass the arguments to tuneR::play if a player is specified.
#' @examples
#'   play(merge(curry(sine, P.n(49)), curry(sawtooth, P.n(+49+3))))
#'   play(merge(curry(sawtooth, 440), curry(sine, 440)))
#'   play(2^8 * Wave(sawtooth(440, 44100), samp.rate = 44100, bit = 16), 'play')
play <- function(object, player = NULL, ...) {
  if (is.null(player))
    tuneR::play(tuneR::normalize(wave(left = object, bit = 16),
                                 unit = '16'),
                'play')
  else
    tuneR::play(object, player, ...)
}
