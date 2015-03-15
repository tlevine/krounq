# https://en.wikipedia.org/wiki/Funk
# https://en.wikipedia.org/wiki/Dorian_mode
# https://en.wikipedia.org/wiki/Mixolydian_mode
# https://en.wikipedia.org/wiki/Ostinato#Vamp
# https://en.wikipedia.org/wiki/Clave_%28rhythm%29
# https://en.wikipedia.org/wiki/Guajeo

# Eight beats
clave.starts <- c(2, 3, 5, 6.5, 8)
clave.durations <- c(1, 1, 1.5, 0.5, 1)
clave.pitches <- scales$mixolydian[c(1, 3, 5)]

sax.starts <- c(1, 3, 5.5, 6.5, 7, 7.5, 8)
sax.durations <- c(1, 1, 0.5, 0.5, 0.5, 0.5, 0.5)
sax.pitches <- scales$mixolydian[c(1, 3, 5)][c(3, 1, 3, 3, 2, 2, 1)

arpeggio.starts <- c(1, 2, 3, 3.5, 4.5, 5.5, 6.5, 7, 8)
arpeggio.durations <- c(1, 1, 0.5, 1, 1, 0.5, 1, 1)
