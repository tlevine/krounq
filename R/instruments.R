fade.polynomial <- function(power, duration)
  seq(1, 0, length.out = duration) ^ power
fade.none <- curry(fade.polynomial, 0)
fade.sqrt <- curry(fade.polynomial, 1/2)
fade.linear <- curry(fade.polynomial, 1)
fade.quadratic <- curry(fade.polynomial, 2)
fade.cubic <- curry(fade.polynomial, 3)

fade.elbow <- function(duration, elbow.position = 1/8, elbow.amplitude = 1/5) {
  elbow.position.absolute <- round(elbow.position * duration)
  c(seq(1, elbow.amplitude, length.out = elbow.position.absolute),
    seq(elbow.amplitude, 0, length.out = duration - elbow.position.absolute))
}

instrument <- function(tone, fade)
  function(frequency, duration)
    tone(frequency, duration) * fade(duration)

harpsicord <- instrument(curry(overtones, 1:13, sawtooth), fade.sqrt)
xylophone <- instrument(curry(overtones, c(1, 2, 4, 8), sine), fade.cubic)
clave <- instrument(square, fade.elbow)
saxaphone <- instrument(curry(overtones, seq(1, 2^4, 2), square), fade.sqrt)
sinesynth <- instrument(sine, fade.linear)
