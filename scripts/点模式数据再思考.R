
library(INLA)
library(sf)
library(inlabru)
library(tidyverse)

data(chorley,package = 'spatstat.data')
chorley_sf <- tibble(
  x = chorley$x,
  y = chorley$y,
  type = factor(chorley$marks)  
) %>%
  st_as_sf(coords = c("x", "y"), crs = NA)  
# 创建焚烧炉位置点
incinerator <- st_point(c(354.5, 413.6)) 

# library(spatstat)
# # 转换为 ppp 对象
# coords <- st_coordinates(chorley_sf)
# marks <- chorley_sf$type
# window <- owin(xrange = range(coords[,1]) + c(-5, 5), yrange = range(coords[,2]) + c(-5, 5))
# chorley_ppp <- ppp(x = coords[,1], y = coords[,2], marks = marks, window = window)

# # 定义焚烧炉位置
# incinerator_ppp <- ppp(x = 354.5, y = 413.6, window = window)

# # 按标记分割点模式
# chorley_split <- split(chorley_ppp)  # 修正：使用 chorley_ppp 而非 chorley

# # 估算强度
# larynx_intensity <- density(chorley_split$larynx, sigma = 1)
# lung_intensity <- density(chorley_split$lung, sigma = 1)

# # 绘制强度图
# par(mfrow = c(1, 2))
# plot(larynx_intensity, main = "喉癌强度 (sigma = 1)")
# points(incinerator_ppp, col = "red", pch = 16, cex = 1.5)
# plot(lung_intensity, main = "对照组强度 (sigma = 1)")
# points(incinerator_ppp, col = "red", pch = 16, cex = 1.5)


incinerator <- st_point(c(354.5, 413.6))
chorley_dist_summary <- chorley_sf %>%
  group_by(geometry,type) %>%
  summarise(cases = n()) %>%
  pivot_wider(names_from = type, values_from = cases,values_fill = 0) |> 
  mutate(dist = as.numeric(st_distance(geometry, incinerator)) / 1000)


# 定义模型公式，使用样条函数
cmp_dist_spline <- geometry ~ Intercept(1) +
  dist(model = "rw2", scale.model = TRUE) 

fit_dist_spline <- bru(
  cmp_dist_spline,
  data = chorley_dist_summary,
  family='cp',
  domain = list(geometry = mesh)
)

# 查看模型结果
summary(fit_dist)