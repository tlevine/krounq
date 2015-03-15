fade.polynomial <- function(power, duration)
  seq(1, 0, length.out = duration) ^ power
fade.none <- curry(fade.polynomial, 0)
fade.sqrt <- curry(fade.polynomial, 1/2)
fade.linear <- curry(fade.polynomial, 1)
fade.quadratic <- curry(fade.polynomial, 2)
fade.cubic <- curry(fade.polynomial, 3)

instrument <- function(tone, fade)
  function(frequency, duration)
    tone(frequency, duration) * fade(duration)

harpsicord <- instrument(curry(overtones, 1:13, sawtooth), fade.sqrt)
xylophone <- instrument(curry(overtones, c(1, 2, 4, 8), sine), fade.cubic)

