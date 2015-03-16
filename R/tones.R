# Sampling rate is always a second

tau <- 2 * pi

#' Generate a sine wave.
#' @param frequency Frequency (440 is middle A.)
#' @param duration Duration in samples (probably 44100 samples per second)
sine <- function(frequency, duration, samp.rate = SECOND)
  sin(tau * frequency * (1:duration)/samp.rate)

#' Generate a sawtooth wave.
#' @param frequency Frequency (440 is middle A.)
#' @param duration Duration in samples (probably 44100 samples per second)
sawtooth <- function(frequency, duration, samp.rate = SECOND)
  seq(1, 2 * frequency * duration / samp.rate, length.out = duration) %% 2 - 1

#' Generate a square wave.
#' @param frequency Frequency (440 is middle A.)
#' @param duration Duration in samples (probably 44100 samples per second)
square <- function(frequency, duration)
  sign(seq(1, frequency * duration, length.out = duration) %% 1 - 0.5)

silence <- function(duration) rep(0, duration)

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
#'   overtones(1:5, sine, 440, SECOND/100)
#'
#'   # An inharmonic series
#'   overtones(runif(5, 1, 3), sine, 440, SECOND/100)
overtones <- function(multiples, generator, frequency, duration)
  multiple.frequencies(multiples * frequency, generator, duration)

#' Produce multiple frequencies.
#' @param generator sine, sawtooth, square
#' @param frequency Frequency (440 is middle A.)
#' @param duration Duration in samples (probably 44100 samples per second)
multiple.frequencies <- function(frequencies, generator, duration) {
  add <- function(a, b) a + b
  f <- function(frequency) generator(frequency, duration)
  Reduce(add, lapply(frequencies, f)) / length(frequencies)
}
