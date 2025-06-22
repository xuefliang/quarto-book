#
#
#| warning: false
source('scripts/utils.R')
#
#
#
#
#
#
#
#
#
#
#
#
data(AirPassengers,package = 'datasets')
AirPassengers |> 
  as_tibble() |> 
  rename('numbers' = 'x') |> 
  mutate(
    numbers = as.numeric(numbers),
    year = rep(1949:1960, each = 12),
    month = rep(1:12, 12),
    time = 1:n() 
  ) -> AirPassengers
#
#
#
cmp <- numbers ~
  Intercept(1) +
  trend(year, model = 'ar1',
        hyper = list(
          prec = list(prior = "pc.prec", param = c(1, 0.01)),
          rho = list(prior = "pc.cor1", param = c(0.7, 0.7))
        )) +
  seasonal(month, model = 'seasonal', season.length = 12,
           hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01))))
airPassengers_ar <- bru(cmp, data = AirPassengers, family = "gaussian",
                       control.family = list(
                         hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
                       ))
summary(airPassengers_ar)
#
#
#
#
#
# 带PC先验
cmp <- numbers ~ 
  Intercept(1) + 
  trend(year, 
        model = 'rw2',
        hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))) +
  seasonal(month, 
           model = 'seasonal', 
           season.length = 12,
           hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01))))

airPassengers_rw <- bru(
  cmp, 
  data = AirPassengers,
  family = "gaussian",
  control.family = list(
    hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
  )
)
summary(airPassengers_rw)
#
#
#
future_data <- data.frame(
  year = rep(1961, 12),
  month = 1:12,
  time = 145:156
)


predictions <- predict(airPassengers_ar, 
                      newdata = AirPassengers, 
                      ~ c(Intercept + trend + seasonal))
#
#
#
#
#
#
#
#
#
#
#
