## Six atomic data types

## boolean /logical
TRUE
FALSE
NA

typeof(NA)

class(Sys.time())
x <- factor(c("AAO", "SAO"))

typeof(x)
class(x)

?factor

ses(wineind, h = 10) %>% plot(col= "red")
lines(wineind)

ma(wineind, order = 9) %>% plot(ylim=c(min(wineind), max(wineind)), col="red")


beer2 <- window(ausbeer,start=1992,end=c(2007,4))
# Plot some forecasts
autoplot(beer2) +
  autolayer(meanf(beer2, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(beer2, h=11),
            series="Naïve", PI=FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))



naive(zoo::rollmean(wineind, k = 9, align = "right")) |>
  plot(ylim=c(min(wineind), max(wineind)),
       col="red",
       main = "Moving Average and Forecasting")

lines(wineind)


wineind


airp <- window(AirPassengers, 1953, c(1953,12))
plot(airp)
ses(airp, h=5, alpha = 0.9) |> plot()
lines(airp)
ets(airp, model = "ANN") |> fitted() |> lines()


ets(AirPassengers, model = 'ANN', alpha = 0.5) |> forecast(h=12) |> plot()
lines(AirPassengers, col= 'red')
ets(AirPassengers, model = "ANN", alpha = 0.5) |> fitted() |> lines(col= "red")
ses(AirPassengers, alpha = 0.5) |> plot()


airpses <- ses(AirPassengers, h=10, alpha = 0.1, level = c(0.8, 0.95))

ggplot2::autoplot(airpses, legend = TRUE) +
  autolayer(airpses$fitted)

?forecast::autoplot


jjholt <- holt(airmiles, h = 5, alpha = 0.3, beta = 0.1)
autoplot(jjholt) +
  autolayer(jjholt$fitted)

plot(airmiles)


holtf <- holt(airmiles, h = 10, alpha = 0.3, beta = 0.1, damped = TRUE, phi = 0.8)
autoplot(holtf)

fc <- holt(air, h=15)
fc2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(air) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))

holtfit <- holt(airmiles, h = 10, alpha = 0.3, beta = 0.1)
holtfit2 <- holt(airmiles, h = 10, alpha = 0.3, beta = 0.1, damped = TRUE, phi = 0.8)

autoplot(airmiles)+
  autolayer(holtfit, series="Holt's method", PI = FALSE) +
  autolayer(holtfit2, series="Holt's method with damped trend", PI = FALSE)+
  ggtitle("Holt's method") + xlab("Year") +
  ylab("Revenue passenger miles") +
  guides(colour=guide_legend(title="Method"))

?airmiles

## hw

hw1 <- hw(AirPassengers,seasonal="additive")
hw2 <- hw(AirPassengers, seasonal="multiplicative")
autoplot(AirPassengers) +
  autolayer(hw1, series="HW additive forecasts", PI=FALSE) +
  autolayer(hw2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Air Passengers") +
  ggtitle("Holt-Winters model") +
  guides(colour=guide_legend(title="Forecast"))
















