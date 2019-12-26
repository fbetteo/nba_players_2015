# Plots de Puntos VS Wins/Losses
# Cargamos lo anterior

library(dplyr)
library(stringr)
library(lubridate)

stats_standing = read_rds("working/stats_standing.rds")

## Analysis Wins explained by points.

plot(stats_standing$points[stats_standing$season != 2011], stats_standing$wins[stats_standing$season != 2011], type = "p")
abline(lm(stats_standing$wins[stats_standing$season != 2011] ~ stats_standing$points[stats_standing$season != 2011]), col = "red")

fit <- lm(stats_standing$wins[stats_standing$season != 2011] ~ stats_standing$points[stats_standing$season != 2011])

summary(fit)

# Mas o menos correlacionado pero bastante variabilidad

## Analysis Losses explained by points

plot(stats_standing$points[stats_standing$season != 2011] , stats_standing$loses[stats_standing$season != 2011], type = "p")
abline(lm(stats_standing$loses[stats_standing$season != 2011] ~ stats_standing$points[stats_standing$season != 2011]), col = "red")

fit <- lm(stats_standing$loses[stats_standing$season != 2011] ~ stats_standing$points[stats_standing$season != 2011] )

summary(fit)

# Mas o menos correlacionado pero bastante variabilidad (un poco menos que wins ~ points)

## Analysis Wins explained by points difference

temp <- stats_standing$points[stats_standing$season != 2011] - stats_standing$allowed_points_total[stats_standing$season != 2011]


plot(temp, stats_standing$wins[stats_standing$season != 2011], type = "p")
abline(lm(stats_standing$wins[stats_standing$season != 2011] ~ temp), col = "red")

fit <- lm(stats_standing$wins[stats_standing$season != 2011] ~ temp)

summary(fit)

# Relacion casi lineal



