SECOND <- 44100

fade.polynomial <- function(power, duration)
  seq(1, 0, length.out = duration) ^ power
fade.sqrt <- curry(fade.polynomial, 1/2)
fade.linear <- curry(fade.polynomial, 1)
fade.quadratic <- curry(fade.polynomial, 2)
fade.cubic <- curry(fade.polynomial, 3)

empty.channel <- function(.) numeric(0)

#' Generate a tuneR wave from an instrument
#' @param duration Duration in samples (probably 44100 samples per second)
#' @param left channel wave
#' @param right channel wave
wave <- function(duration, left = empty.channel, right = empty.channel,
                 sampling.rate = SECOND, bit = 16,
                 fade = fade.quadratic)
  2^(bit/2) * fade(duration) *
    tuneR::Wave(left(duration), right(duration),
                samp.rate = sampling.rate, bit = bit)

#' Play a one-second clip of the function, if no player is specified, or
#' pass the arguments to tuneR::play if a player is specified.
#' @examples
#'   play(merge(curry(sine, P.n(49)), curry(sawtooth, P.n(+49+3))))
#'   play(merge(curry(sawtooth, 440), curry(sine, 440)))
#'   play(2^8 * Wave(sawtooth(440, 44100), samp.rate = 44100, bit = 16), 'play')
play <- function(object, player = NULL, ...) {
  if (is.null(player))
    tuneR::play(tuneR::normalize(wave(SECOND, left = object, bit = 16),
                                 unit = '16'),
                'play')
  else
    tuneR::play(object, player, ...)
}
