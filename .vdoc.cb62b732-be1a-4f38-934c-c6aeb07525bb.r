#
#
#
#| warning: false
source('scripts/utils.R')
library(SpatialEpi)
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
#
#
data(scotland_sf,package = 'SpatialEpi')

var_desc <- data.frame(
  变量名 = names(scotland_sf),
  描述 = c(
    "每个县名称",
    "每个县观察到的唇癌病例数",
    "每个县预期的唇癌病例数",
    "每个县从事农业、渔业或林业人口的比例",
    "几何列"
  )
)

format_kable_table(var_desc)
#
#
#
# 创建INLA网格
mesh <- inla.mesh.2d(
  scotland_sf,
  max.edge = c(0.5, 1.25),
  offset = c(0.1, 1.5),
  cutoff = 0.1
)

# 可视化网格
plot(mesh, main = "INLA Mesh")

# 设置空间场的先验分布
pc_prior_range <- c(0.5, 0.01)  # P(range < 0.5) = 0.01
pc_prior_sigma <- c(1, 0.01)    # P(sigma > 1) = 0.01

# 定义SPDE模型
spde <- inla.spde2.pcmatern(mesh, prior.range = pc_prior_range, prior.sigma = pc_prior_sigma)

# 定义inlabru模型组件
components <- cases ~ 1 + AFF
  field1(geometry, model = spde)

fit <- bru(compo)
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
