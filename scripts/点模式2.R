library(tidyverse)
library(sf)
library(INLA)
library(inlabru)
library(fmesher)
library(patchwork)

# 加载数据
data(chorley, package = 'spatstat.data')

data(southlancs)

chorley <- as.data.frame(southlancs.pts/1000) |> 
  st_as_sf(coords=c('arx','ary'), crs=27700) |> 
  mutate(
    type = case_when(
      row_number() <= 917 ~ 'lung',
      row_number() > 917 ~ 'larynx',
      .default = NA_character_
    )
  )

# 将spatstat点模式对象转换为sf数据框
chorley <- data.frame(
  x = chorley$x,
  y = chorley$y,
  type = factor(chorley$marks)
) %>%
  st_as_sf(coords = c("x", "y"), crs = 27700) 

chorley_extra <- data.frame(
  x = chorley.extra$incin$x,
  y = chorley.extra$incin$y
) %>%
  st_as_sf(coords = c("x", "y"), crs = 27700)

southlancs <- st_read('data/southlancs.geojson')

ggplot() +
  geom_sf(data = southlancs |> st_transform(crs=4326), 
          fill = 'white', color = 'grey60', linewidth = 0.4) +
  geom_sf(data = chorley |> st_transform(crs=4326), 
          aes(color = type), size = 2.2, alpha = 0.8) +
  geom_sf(data = chorley_extra |> st_transform(crs=4326), 
          color = '#d63031', size = 3.5, shape = 17) + 
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "right"
  )

# 创建网格
coords <- st_coordinates(southlancs)
initial_range <- diff(range(coords[, "X"])) / 3
max_edge <- initial_range / 5

mesh <- fm_mesh_2d(
  loc = st_coordinates(southlancs)[,1:2],
  max.edge = c(1, 2) * max_edge,
  cutoff = max_edge/10,
  crs = st_crs(chorley)
)

# 查看网格点数量
mesh$n

ggplot() +
  gg(data=mesh) 

plot(mesh)
plot(chorley["type"], add = TRUE, pch = 16)

# 定义改进的SPDE模型
# 调整先验以提高数值稳定性
prior_range <- initial_range
prior_sigma <- 1  # 调整sigma先验

spde <- inla.spde2.pcmatern(
  mesh = mesh,
  alpha = 2,
  prior.range = c(prior_range, 0.8),  # 第二个参数是概率
  prior.sigma = c(prior_sigma, 0.1)  # 第二个参数是概率
)

# 定义组件
cmp <- geometry ~
   Intercept(1) + spatialspde(geometry, model = spde)

fit.lun <- lgcp(
  components = cmp,
  data = chorley %>% filter(type=='lung'),
  domain = list(geometry = mesh),
  options = list(
    control.inla = list(
      reordering = "metis",
      int.strategy = "eb"
    ),
    verbose = FALSE
  )
)
