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
