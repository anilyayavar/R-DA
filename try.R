plot(sunspot.month)
lines(sunspots)
sunspots
all(tsp(sunspots)     [c(1,3)] ==
      tsp(sunspot.month)[c(1,3)]) ## Start & Periodicity are the same
n1 <- length(sunspots)
table(eq <- sunspots == sunspot.month[1:n1]) #>  132  are different !
i <- which(!eq)
rug(time(eq)[i])
s1 <- sunspots[i] ; s2 <- sunspot.month[i]
cbind(i = i, time = time(sunspots)[i], sunspots = s1, ss.month = s2,
      perc.diff = round(100*2*abs(s1-s2)/(s1+s2), 1))


library(forecast)

?wineind
plot(wineind, main = "Wine sales")
sm <- ma(wineind,order=9, centre = FALSE)
lines(sm,col="red")

forecast(sm, h=10) |> plot(col="red",
                           ylim = c(min(wineind), max(wineind)),
                           xlim(1979, 2000))
lines(wineind)
?forecast::forecast


?ma

library(tidyverse)

