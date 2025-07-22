# 加载必要的包
library(sf)
library(gstat)
library(sp)
library(dplyr)
library(ggplot2)
library(ggpubr)

# 读取数据
rongelap <- readRDS(file = "data/rongelap.rds") |> 
  st_as_sf(coords = c('cX', 'cY'), dim = "XY")
rongelap_coastline <- readRDS(file = "data/rongelap_coastline.rds") |> 
  st_as_sf(coords = c('cX', 'cY'), dim = "XY")

# 确保CRS一致
st_crs(rongelap) <- st_crs(rongelap_coastline)

head(rongelap)
summary(rongelap)

# 2. 转换为sp对象（gstat需要）
rongelap_sp <- as(rongelap, "Spatial")
coastline_sp <- as(rongelap_coastline, "Spatial")

# 3. 半变异函数分析
# 计算经验半变异函数
variogram_emp <- variogram(counts ~ 1, rongelap_sp)
plot(variogram_emp, main = "经验半变异函数")

fit_best_model <- function(vario_emp) {
  initial_sill <- max(vario_emp$gamma)
  initial_range <- max(vario_emp$dist) / 3
  initial_nugget <- min(vario_emp$gamma)
  
  models <- list(
    sph = vgm(psill = initial_sill, "Sph", range = initial_range, nugget = initial_nugget),
    exp = vgm(psill = initial_sill, "Exp", range = initial_range, nugget = initial_nugget),
    gau = vgm(psill = initial_sill, "Gau", range = initial_range, nugget = initial_nugget)
  )
  
  best_fit <- NULL
  best_sse <- Inf
  best_name <- ""
  
  for(name in names(models)) {
    tryCatch({
      fit <- fit.variogram(vario_emp, models[[name]])
      sse <- attr(fit, "SSErr")
      if(!is.null(sse) && sse < best_sse) {
        best_sse <- sse
        best_fit <- fit
        best_name <- name
      }
    }, error = function(e) {})
  }
  
  cat("最佳模型:", best_name, "SSE:", best_sse, "\n")
  return(best_fit)
}

# 拟合最佳模型
best_variogram_model <- fit_best_model(variogram_emp)
print(best_variogram_model)

# 绘制最终结果
plot(variogram_emp, best_variogram_model, 
     main = "最佳拟合的半变异函数模型",
     xlab = "距离 (m)", ylab = "半方差")

best_variogram_model <- vgm(psill = 22706501, model = "Gau", range = 1657.995, nugget = 1411944)


# 3. 创建预测网格
create_prediction_grid <- function(boundary, grid_size = 50) {
  # 获取边界框
  bbox <- st_bbox(boundary)
  
  # 创建规则网格
  x_seq <- seq(bbox["xmin"], bbox["xmax"], length.out = grid_size)
  y_seq <- seq(bbox["ymin"], bbox["ymax"], length.out = grid_size)
  
  # 创建网格点
  grid_points <- expand.grid(x = x_seq, y = y_seq)
  grid_sf <- st_as_sf(grid_points, coords = c("x", "y"), 
                      crs = st_crs(boundary))
  
  # 只保留边界内的点
  grid_sf <- st_intersection(grid_sf, boundary)
  
  return(grid_sf)
}

# 4. 创建预测网格
rongelap_coastline_sfp <- rongelap_coastline %>%
  st_combine() %>%
  st_cast("POLYGON")
grid_size <- 200  
grid_sf <- create_prediction_grid(rongelap_coastline_sfp, grid_size)
plot(grid_sf)
grid_sp <- as(grid_sf, "Spatial")

# 6. 执行普通克里金插值
kriging_result <- krige(counts ~ 1, rongelap_sp, grid_sp, model = best_variogram_model)

# 7. 转换结果为sf对象用于可视化
kriging_sf <- st_as_sf(kriging_result)

# 8. 可视化克里金预测结果
ggplot() +
    geom_sf(data = kriging_sf, aes(color = var1.pred), size = 1.0)

ggplot() +
  geom_sf(data = kriging_sf, aes(color = var1.var), size = 1.0)
