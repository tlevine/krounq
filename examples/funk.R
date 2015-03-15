library(devtools)
unloadNamespace('devtools')
devtools::load_all()
source('generate-data.R')

# https://en.wikipedia.org/wiki/Funk
# https://en.wikipedia.org/wiki/Dorian_mode
# https://en.wikipedia.org/wiki/Mixolydian_mode
# https://en.wikipedia.org/wiki/Ostinato#Vamp
# https://en.wikipedia.org/wiki/Clave_%28rhythm%29
# https://en.wikipedia.org/wiki/Guajeo

onbeat.offbeat.23 <- track(110,
                           c(2, 3, 5, 6.5, 8),
                           durations = c(1, 1, 1.5, 0.5, 1) / 4,
                           instrument = clave)

sax <- track(P.n(scales$mixolydian[c(1, 3, 5)][c(3, 1, 3, 3, 2, 2, 1)], a = 24),
             c(1, 3, 5.5, 6.5, 7, 7.5, 8),
             durations = c(1, 1, 0.5, 0.5, 0.5, 0.5, 0.5),
             instrument = saxaphone)

arpeggios <- track(P.n(scales$mixolydian[c(1, 3, 5, 7, 3, 7, 5, 2, 3)], a = 0),
                   c(1, 2, 3, 3.5, 4.5, 5.5, 6.5, 7, 8),
                   c(1, 1, 0.5, 1, 1, 0.5, 1, 1, 1),
                   instrument = sinesynth)

play(rep(0.5 * onbeat.offbeat.23 + 2 * sax + arpeggios, 4))
