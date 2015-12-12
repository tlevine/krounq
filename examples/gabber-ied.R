library(devtools)
unloadNamespace('devtools')
load('examples/roland.rda')
devtools::load_all()
source('generate-data.R')

TEMPO <- 240 # multiple of 24, for easy division

norm <- function(x) {
  x / max(abs(x))
}

drumlike <- function(freq, duration) {
  base <- 0.8 * sawtooth(freq, duration) ^ 3 + runif(duration, -.2, .2)
  duration.left <- min(duration, round(SECOND/8))
  base.left <- base[1:duration.left]
  base.right <- silence(max(0, duration - duration.left))
  c(base.left, base.right)
}

sample.instrument <- function(the.sample) {
  function(., duration) {
    if (duration < length(the.sample)) {
      the.sample[1:duration]
    } else {
      c(the.sample, silence(duration - length(the.sample)))
    }
  }
}

kick <- sample.instrument(norm(roland$BD1@left[(1:length(roland$BD1@left)) %% 2 == 0]))
snare <- sample.instrument(norm(roland$SD0@left))
hihat <- sample.instrument(norm(roland$HHO@left))
rim <- sample.instrument(norm(roland$RIM@left))

phrase <- function(key = 30, speed = 0, pickup = NULL, drums = TRUE,
                   rhythm = c(1, 4)) {
  base.duration <- 2 ^ (4 - floor(speed))

  pounding <- sequence(durations = base.duration,
                       instrument = if (drums) snare else rim,
                       tempo = TEMPO, beats = 8)

  f <- rep(key, length(rhythm))
  if (!is.null(pickup) && length(pickup) > 0)
    f[floor(rhythm) %% 4 == 0] <- key + pickup
  melody <- sequence(frequencies = P.n(f),
                     starts = rhythm,
                     durations = 0.5,
                     instrument = drumlike,
                     tempo = TEMPO,
                     beats = 8)

  2.5 * pounding + melody
}

frame <- function(df, j) {
  PETAL <- c(-3, 12)
  SEPAL <- c(-1, 5)

  MAPPINGS <- 'Speed ~ Sepal Length
Pickup note ~ Sepal Width
Time ~ Petal Width
Drums ~ Petal Length > 3
Rhythm ~ Species'

  par(bg = 'black', fg = 'white', col = 'white', col.axis = 'white',
      col.main = 'white', col.sub = 'white', col.lab = 'white',
      font = 2, family = 'sans')
  df$density <- 5
  df[nrow(df),'density'] <- 20
  plot(0, 0, xlim = PETAL, ylim = SEPAL,
       type = 'n', bty = 'l', asp = 1,
       main = '', sub = 'Rectangles are sepal sizes.',
       xlab = 'Petal Length', ylab = 'Petal Width')

  jitter <- round(30 * (1:nrow(df) / nrow(df)))
  rect(xleft   = df$Petal.Length - 0.5 * df$Sepal.Length,
       xright  = df$Petal.Length + 0.5 * df$Sepal.Length,
       ytop    = df$Petal.Width - 0.5 * df$Sepal.Width,
       ybottom = df$Petal.Width + 0.5 * df$Sepal.Width,
       col = COLORS[df$Species],
       border = NA,
       density = df$density,
       angle = as.numeric(df$Species) * 15 + jitter - 45 * j)
  last.row <- df[nrow(df),]
  text(x = 0, y = max(SEPAL), pos = 1,
       label = MAPPINGS, col = COLORS[last.row$Species])
  text(x = last.row$Petal.Length, y = last.row$Petal.Width,
       label = last.row$Species)
# text(x = 0, y = 5,
#      label = paste0('Frame ', nrow(df), '-', j, '\n'))
}

COLORS <- c(setosa = 'violet', virginica = 'pink', versicolor = 'cyan')

RHYTHMS <- list(
  Weekday = c(1, 2, 3, 4.5, 5, 6, 7, 8, 8.5),
  Friday = c(1, 2, 3, 3 + 2/3, 4 + 1/3, 5, 6, 6.5, 7, 7.5, 8, 8.5),
  Saturday = c(1, 3, 5, 7)
)

p <- function(row)
  phrase(key = 30, speed = (row$kia + row$wia) / 20,
         pickup = scales$major[as.numeric(row$Region)],
         drums = row$kia + row$wia > 1 && (row$kia / (row$kia + row$wia)) > 1/3,
         rhythm = RHYTHMS[[row$weekday]])

# Subset
ied <- read.csv('examples/IED_Data.csv', stringsAsFactors = FALSE)
ied$wia <- rowSums(ied[c("FriendlyWIA", "HostNationWIA", "EnemyWIA", "CivilianWIA")])
ied$kia <- rowSums(ied[c("FriendlyKIA", "HostNationKIA", "EnemyKIA", "CivilianKIA")])
ied$date <- strptime(ied$DateOccurred, '%m/%d/%y 0:00')
ied$weekday <- factor(weekdays(ied$date), levels = c('Weekday', 'Friday', 'Saturday'))
ied$weekday[is.na(ied$weekday)] <- 'Weekday'
ied$kia[is.na(ied$kia)] <- 0
ied$wia[is.na(ied$wia)] <- 0
ied$Region <- factor(ied$Region)
ied <- subset(ied, Region == 'RC CAPITAL')

# Music
song <- do.call(c,lapply(1:nrow(ied), function(i) p(ied[i,])))
write.wave(wave(song), '/tmp/krounq.wav', do.normalize = TRUE)
print('woo')
quit()

# Video
for (i in 1:nrow(iris)) {
  for (j in 0:1) {
    fn <- sprintf('/tmp/krounq-%03d.png', (i-1)*2 + j)
    png(fn, width = 800, height = 450)
    frame(iris[1:i,], j)
    dev.off()
  }
}

system(paste(
  'avconv',
  '-r 1 -i /tmp/krounq-%03d.png -i /tmp/krounq.wav',
  '-y -pix_fmt yuv420p -r 1',
  '-strict -2',
  '/tmp/krounq.webm'))



# play(song)
