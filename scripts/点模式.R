library(sf)
library(INLA)
library(inlabru)
library(tidyverse)

data(chorley,package = 'spatstat.data')

# 将spatstat点模式对象转换为sf数据框
chorley_sf <- tibble(
  x = chorley$x,
  y = chorley$y,
  type = factor(chorley$marks)  
) %>%
  st_as_sf(coords = c("x", "y"), crs = NA)  
boundary <- chorley_sf %>%
  st_union() %>%      
  st_convex_hull()  

coords <- st_coordinates(chorley_sf)
x_range <- diff(range(coords[, "X"]))
y_range <- diff(range(coords[, "Y"]))
max_range <- max(x_range, y_range)
max_edge <- max_range / 15 
offset <- max_range / 10

mesh <- fm_mesh_2d_inla(
  loc = coords,
  max.edge = c(max_edge, max_edge * 2),
  offset = c(offset, offset * 2),
  cutoff = max_edge / 2
)
plot(mesh)
mesh$n
points(coords, col = as.numeric(chorley_sf$type) + 1, pch = 16)

##########################
spde <- inla.spde2.pcmatern(
    mesh, 
    alpha = 2.0,
    # 增加先验范围的不确定性
    prior.range = c(1.0, 0.1),  # 原来是(0.5, 0.01)
    # 增加sigma先验的不确定性
    prior.sigma = c(1, 0.1)     # 原来是(1, 0.01)
)
# 注意，参数 ν在马特尔协方差的定义中被设置为 1（因为 ν 等于 alpha 减去维度的一半，Lindgren et al. (2011))。

cmp <- geometry ~
   Intercept(1) + spatialspde(geometry, model = spde)

fit.lun <- lgcp(
  components = cmp,
  data = chorley_sf %>% filter(type=='lung'),
  domain = list(geometry = mesh),
  options = list(
    control.inla = list(
      int.strategy = "eb",     # 使用经验贝叶斯积分策略
      strategy = "gaussian",   # 使用高斯近似
      diagonal = 1e-6,         # 增加对角线稳定性
      tolerance = 1e-3         # 放宽收敛容忍度
    ),
    verbose = FALSE
  )
)

cmp <- geometry ~ -1 + Inter.con(1) + Inter.lar(1) +
  sharedspde(geometry, model = spde)

# Likelihood for the controls (lung cancer)
con.lik <- bru_obs(
  family = "cp",
  formula = geometry ~ Inter.con + sharedspde,
  data = chorley_sf |> filter(type=='lung'),
  domain = list(geometry = mesh)
)

# Likelihood for the cases (larynx cancer)
lar.lik <- bru_obs(
  family = "cp",
  formula = geometry ~ Inter.lar + sharedspde,
  data = chorley_sf |> filter(type=='larynx'),
  domain = list(geometry = mesh)
)

fit1 <- bru(cmp, con.lik, lar.lik)

cmp <- geometry ~ -1 + Inter.con(1) + Inter.lar(1) +
  sharedspde(geometry, model = spde) +
  larspde(geometry, model = spde)

# Likelihood of the controls (lung cancer)
con.lik <- bru_obs(
  family = "cp",
  formula = geometry ~ Inter.con + sharedspde,
  data = chorley_sf |> filter(type=='lung'),
  domain = list(geometry = mesh)
)

# Likelihood of the cases (larynx cancer)
lar.lik <- like(
  family = "cp",
  formula = geometry ~ Inter.lar + sharedspde + larspde,
  data = chorley_sf |> filter(type=='larynx'),
  domain = list(geometry = mesh)
)


fit2 <- bru(cmp, con.lik, lar.lik, 
            options = list(
              control.inla = list(int.strategy = "eb"),
              verbose = T
            ))

#############################33
# 定义SPDE模型的先验
# mesh <- inla.mesh.2d(
#   loc = sf::st_coordinates(chorley_sf),
#   max.edge = c(0.1, 0.5),  # Inner and outer max edge length
#   cutoff = 0.05,           # Minimum edge length
#   offset = c(0.1, 0.5)     # Inner and outer extension
# )

spde <- inla.spde2.pcmatern(
  mesh = mesh,
  alpha = 1.5,
  prior.range = c(0.5, 0.05),  # 范围小于 1 的概率为 0.05
  prior.sigma = c(1, 0.05)   # 标准差大于 1 的概率为 0.05
)


# 定义模型组件
cmp <- geometry ~ -1 + Inter.con(1) + Inter.lar(1) +
  sharedspde(geometry, model = spde) +
  larspde(geometry, model = spde)

# 肺癌的似然函数（controls）
con.lik <- bru_obs(
  family = "cp",
  formula = geometry ~ Inter.con + sharedspde,
  data = chorley_sf |> filter(type == 'lung'),
  domain = list(geometry = mesh)
)

# 喉癌的似然函数（cases）
lar.lik <- like(
  family = "cp",
  formula = geometry ~ Inter.lar + sharedspde + larspde,
  data = chorley_sf |> filter(type == 'larynx'),
  domain = list(geometry = mesh)
)

# 拟合模型
fit2 <- bru(
  cmp, 
  con.lik, 
  lar.lik, 
  options = list(
    control.inla = list(
      int.strategy = "eb"
    ),
    verbose = FALSE
  )
)


#######################
library(tidyverse)
library(sf)
library(INLA)
library(inlabru)
library(fmesher)
library(patchwork)

# 加载数据
data(chorley, package = 'spatstat.data')

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
  st_as_sf(coords = c("x", "y"), crs = 27700) |> 
  slice(rep(1, nrow(chorley)))

# 创建网格
coords <- st_coordinates(chorley)
initial_range <- diff(range(coords[, "X"])) / 3
max_edge <- initial_range / 5

mesh <- fm_mesh_2d(
  loc = st_coordinates(chorley),
  max.edge = c(1, 2) * max_edge,
  cutoff = max_edge/10,
  crs = st_crs(chorley)
)

# 查看网格点数量
mesh$n

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

# 输出摘要结果
summary(fit.lun)

# 病例组和对照组
cmp <- geometry ~ -1 + Inter.con(1) + Inter.lar(1) +
  sharedspde(geometry, model = spde) 

# Likelihood of the controls (lung cancer)
con.lik <- bru_obs(
  family = "cp",
  formula = geometry ~ Inter.con + sharedspde,
  data = chorley |> filter(type=='lung'),
  domain = list(geometry = mesh)
)

# Likelihood of the cases (larynx cancer)
lar.lik <- like(
  family = "cp",
  formula = geometry ~ Inter.lar + sharedspde,
  data = chorley |> filter(type=='larynx'),
  domain = list(geometry = mesh)
)


fit <- bru(cmp, con.lik, lar.lik, 
            options = list(
              control.inla = list(int.strategy = "eb"),
              verbose = FALSE
            ))

# 要拟合的模型将包括癌症的空间特异性效
cmp <- geometry ~ -1 + Inter.con(1) + Inter.lar(1) +
  sharedspde(geometry, model = spde) +
  larspde(geometry, model = spde)
# Likelihood of the controls (lung cancer)
con.lik <- bru_obs(
  family = "cp",
  formula = geometry ~ Inter.con + sharedspde,
  data = chorley |> filter(type=='lung'),
  domain = list(geometry = mesh)
)

# Likelihood of the cases (larynx cancer)
lar.lik <- bru_obs(
  family = "cp",
  formula = geometry ~ Inter.lar + sharedspde + larspde,
  data = chorley |> filter(type=='larynx'),
  domain = list(geometry = mesh)
)

fit2 <- bru(cmp, con.lik, lar.lik, 
            options = list(
              control.inla = list(int.strategy = "eb"),
              verbose = FALSE
            ))

summary(fit2)

# 展示了共同空间效应和特定空间效应的后验均值估计
grid <- fm_pixels(mesh,format='sf')
#Estimate the intensity of the lung cancer cases
lambda.con <- predict(fit2, grid, ~exp(Inter.con + sharedspde))
#Estimate the intensity of the larynx cancer cases
lambda.lar <- predict(fit2, grid, ~exp(Inter.lar + sharedspde + larspde))
#Estimate the common spatial effect
sp.eff.con <- predict(fit2, grid, ~sharedspde)
#Estimate the exposure effect of the old incinerator
cov.eff.lar <- predict(fit2, grid, ~larspde)

p1 <- ggplot() +
  geom_sf(data = lambda.con |> st_transform(crs=4326), aes(color = mean))

p2 <- ggplot() +
  geom_sf(data = lambda.lar |> st_transform(crs=4326), aes(color = mean))

p3 <- ggplot() +
  geom_sf(data = sp.eff.con |> st_transform(crs=4326), aes(color = mean))

p4 <- ggplot() +
  geom_sf(data = cov.eff.lar |> st_transform(crs=4326), aes(color = mean))

(p1+p2)/(p3+p4)


# Chorley-Ribble 数据集还包含一个废弃的焚烧厂的位置，这可能会影响病例的外观。因此，至少考虑焚烧厂对病例强度的影响是很方便的
# 将网格点转换为sf对象
mesh_pts <- st_as_sf(
  data.frame(x = mesh$loc[, 1], y = mesh$loc[, 2]),
  coords = c("x", "y"),
  crs = 27700
)

# 计算网格点到焚化炉的距离
distances <- st_distance(mesh_pts, chorley_extra)[, 1]

# 创建空间像素网格
pix_cov_grid <- fm_pixels(mesh)
pix_cov <- pix_cov_grid
pix_cov$cov1 <- fm_evaluate(mesh, loc = pix_cov_grid, field = as.numeric(distances))

# 定义协变量函数
f_cov1 <- function(where) {
  # 使用最近邻插值
  nearest_indices <- st_nearest_feature(where, pix_cov)
  v <- pix_cov$cov1[nearest_indices]
  if (any(is.na(v))) {
    v[is.na(v)] <- mean(pix_cov$cov1, na.rm = TRUE)
  }
  return(v)
}

f_cov1(chorley[1:5, ])

# 定义模型组件
cmp <- geometry ~ -1 + Inter.con(1) + Inter.lar(1) +
  sharedspde(geometry, model=spde) +
  larspde(f_cov1(geometry), model = "linear")

# 定义似然函数
con.lik <- bru_obs(
  family = "cp",
  formula = geometry ~ Inter.con + sharedspde,
  data = chorley |> filter(type == 'lung'),
  domain = list(geometry = mesh)
)

lar.lik <- bru_obs(
  family = "cp",
  formula = geometry ~ Inter.lar + sharedspde + larspde,
  data = chorley |> filter(type == 'larynx'),
  domain = list(geometry = mesh)
)

# 拟合模型
fit2 <- bru(cmp, con.lik, lar.lik,
  options = list(
    control.inla = list(
      reordering = "metis",
      int.strategy = "eb"
    ),
    verbose = F
  )
)

summary(fit2)

# 除了考虑到焚烧炉距离的线性项外， 其他非线性平滑项可以基于距离使用 单维或 一维中的随机游走或高斯过程（也可以使用 SPDE 方法进行估计）。接下来，将考虑一阶的高斯过程和二阶的随机游走来模拟距离的影响。除了考虑垃圾焚烧厂的距离线性项外，还可以根据距离使用一维 SPDE、随机游走或一维高斯过程（也可以使用 SPDE 方法进行估计）包含其他非线性平滑项。接下来，将考虑一阶高斯过程和二阶随机游走来建模距离的影响。

####一维SPDE 
library(tidyverse)
library(sf)
library(INLA)
library(inlabru)
library(fmesher)
library(patchwork)

# 加载数据
data(chorley, package = 'spatstat.data')

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
  st_as_sf(coords = c("x", "y"), crs = 27700) |> 
  slice(rep(1, nrow(chorley)))

# 创建网格
coords <- st_coordinates(chorley)
initial_range <- diff(range(coords[, "X"])) / 3
max_edge <- initial_range / 5

mesh <- fm_mesh_2d(
  loc = st_coordinates(chorley),
  max.edge = c(1, 2) * max_edge,
  cutoff = max_edge/10,
  crs = st_crs(chorley)
)

# 定义二维SPDE模型（共享空间效应）
prior_range <- initial_range
prior_sigma <- 1

spde <- inla.spde2.pcmatern(
  mesh = mesh,
  alpha = 2,
  prior.range = c(prior_range, 0.8),
  prior.sigma = c(prior_sigma, 0.1)
)

# 步骤1：计算网格点到焚化炉的距离
grid_pts <- mesh$loc[, 1:2]
grid_dist <- as.numeric(st_distance(
  st_as_sf(data.frame(x = grid_pts[,1], y = grid_pts[,2]), 
           coords = c("x", "y"), crs = 27700),
  chorley_extra[1,]
))

# 步骤2：创建空间像素网格并插值
pix_cov_grid <- fm_pixels(mesh)
pix_cov <- pix_cov_grid
pix_cov$cov1 <- fm_evaluate(mesh, loc = pix_cov_grid, field = grid_dist)

# 修正协变量函数
f_cov1 <- function(where) {
  # 使用最近邻插值
  nearest_indices <- st_nearest_feature(where, pix_cov)
  v <- pix_cov$cov1[nearest_indices]
  if (any(is.na(v))) {
    v[is.na(v)] <- mean(pix_cov$cov1, na.rm = TRUE)
  }
  return(v)
}

cov_dist <- chorley |> filter(type == 'larynx') |> f_cov1()

# 步骤4：建立一维网格和一维SPDE效应
mesh_cov <- fm_mesh_1d(
  seq(0, round(max(cov_dist)), length = 20),
  degree = 2,
  boundary = c("free", "dirichlet")
)

# 创建一维SPDE基函数
spde_cov <- inla.spde2.matern(
  mesh = mesh_cov, 
  alpha = 2, 
  constr = TRUE
)

# # a Random Walk of order 2
# spde_cov <- inla.spde2.pcmatern(
#   mesh = mesh_cov, alpha=2,
#   constr = FALSE,
#   prior.range = c(10, NA),
#   prior.sigma = c(1, 0.05))

# 步骤5：定义模型组件
cmp <- geometry ~ -1 + Inter.con(1) + Inter.lar(1) +
  conSpde(geometry, model = spde) +
  cov.lar(f_cov1(geometry), model = spde_cov)

# 定义似然函数
con.lik <- bru_obs(
  family = "cp",
  formula = geometry ~ Inter.con + conSpde,
  data = chorley |> filter(type == 'lung'),
  domain = list(geometry = mesh)
)

lar.lik <- bru_obs(
  family = "cp",
  formula = geometry ~ Inter.lar + conSpde + cov.lar,
  data = chorley |> filter(type == 'larynx'),
  domain = list(geometry = mesh)
)

# 步骤6：拟合模型
fit_1d_spde <- bru(cmp, con.lik, lar.lik,
  options = list(
    control.inla = list(
      reordering = "metis",
      int.strategy = "eb"
    ),
    verbose = TRUE  # 改为TRUE以查看更多信息
  )
)

summary(fit_1d_spde)

brinla::bri.hyperpar.summary(fit_1d_spde)
