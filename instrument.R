library(tuneR)

SECOND <- 44100

#' Partially apply a function.
#' @param f The function
#' @param ... Arguments to the function
curry <- function(f, ...) {
  curried.args <- list(...)
  function(...) do.call(f, append(curried.args, list(...)))
}

#' Generate a sine wave.
#' @param frequency Frequency (440 is middle A.)
#' @param duration Duration in samples (probably 44100 samples per second)
sine <- function(frequency, duration)
  sin(frequency * (1:duration))

#' Generate a sawtooth wave.
#' @param frequency Frequency (440 is middle A.)
#' @param duration Duration in samples (probably 44100 samples per second)
sawtooth <- function(frequency, duration)
  seq(1, 2 * frequency * duration, length.out = duration) %% 2

#' Generate a square wave.
#' @param frequency Frequency (440 is middle A.)
#' @param duration Duration in samples (probably 44100 samples per second)
square <- function(frequency, duration)
  sign(seq(1, frequency * duration, length.out = duration) %% 1 - 0.5)

silence <- function(duration) rep(0, duration)

empty.channel <- function(.) numeric(0)

fade.polynomial <- function(power, duration)
  seq(1, 0, length.out = duration) ^ power
fade.sqrt <- curry(fade.polynomial, 1/2)
fade.linear <- curry(fade.polynomial, 1)
fade.quadratic <- curry(fade.polynomial, 2)
fade.cubic <- curry(fade.polynomial, 3)

#' Generate a tuneR wave from an instrument
#' @param duration Duration in samples (probably 44100 samples per second)
#' @param left channel wave
#' @param right channel wave
wave <- function(duration, left = empty.channel, right = empty.channel,
                 sampling.rate = SECOND, bit = 16,
                 fade = fade.quadratic)
  2^(bit/2) * fade(duration) *
    Wave(left(duration), right(duration),
         samp.rate = sampling.rate, bit = bit)

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

#' Play a one-second clip of the function, if no player is specified, or
#' pass the arguments to tuneR::play if a player is specified.
#' @examples
#'   play(merge(curry(sawtooth, 440), curry(sine, 440)))
#'   play(2^8 * Wave(sawtooth(440, 44100), samp.rate = 44100, bit = 16), 'play')
play <- function(object, player = NULL, ...) {
  if (is.null(player))
    tuneR::play(wave(SECOND, left = object), 'play')
  else
    tuneR::play(object, player, ...)
}
