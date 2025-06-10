library(spatstat)
library(sf)
library(tidyverse)
library(inlabru)
library(INLA)
library(brinla)

data(chorley, package = 'spatstat.data')

chorley_sf <- tibble(
  x = chorley$x,
  y = chorley$y,
  type = factor(chorley$marks)
) %>%
  st_as_sf(coords = c("x", "y"), crs = NA)

incinerator_sf <- tibble(
  x = 354.5,
  y = 413.6
) %>%
  slice(rep(1, 1036)) %>%  
  st_as_sf(coords = c("x", "y"), crs = NA)

chorley_sf <- chorley_sf |> 
  mutate(
    dist = st_distance(geometry, incinerator_sf, by_element = TRUE) %>%
           as.numeric(),
    dist_grouped = inla.group(dist, n = 20),
    log_dist = log(dist + 0.001),
    dist_band = cut(dist, 
      breaks = quantile(dist, probs = seq(0, 1, 0.2)), 
      include.lowest = TRUE,
      labels = c("Very_Close", "Close", "Medium", "Far", "Very_Far"))
  )

ggplot() +
  geom_sf(data = chorley_sf,  aes(colour = type),size = 2) +
  geom_sf(data = incinerator_sf, color = "blue", size = 2, shape = 17) +
  theme_minimal() +
  labs(title = "Larynx Cancer Cases and Incinerator Location")

boundary <- chorley_sf %>%
  st_union() %>%      
  st_convex_hull()  

coords <- st_coordinates(chorley_sf)
x_range <- diff(range(coords[, "X"]))
y_range <- diff(range(coords[, "Y"]))
max_range <- max(x_range, y_range)
max_edge <- max_range / 20 
offset <- max_range / 10     
mesh <- inla.mesh.2d(
  loc = coords,
  max.edge = c(max_edge, max_edge * 2),
  offset = c(offset, offset * 2),
  cutoff = max_edge / 2
)
plot(mesh)
mesh$n

matern <- inla.spde2.pcmatern(
  mesh,
  prior.sigma = c(2, 0.05),
  prior.range = c(2.5, 0.05)
)

# 定义模型组件
components <- geometry ~ 
  Intercept(1) + type(chorley_sf$type, model = "factor_full") +dist(dist_band, model = "rw2")+
    spatial(geometry, model = matern)

# 拟合LGCP模型
lgcp_fit <- lgcp(
  components,
  data = chorley_sf,
  samplers = boundary,
  domain = list(geometry = mesh),
  options = list(
    control.inla = list(
      strategy = "gaussian",
      int.strategy = "ccd"
    )
  )
)


# 模型摘要
summary(lgcp_fit)

bri.hyperpar.summary(lgcp_fit)

bri.fixed.plot(lgcp_fit)


