library(fdmr)
library(sf)
library(fmesher)
library(tidyverse)
library(overedge)
library(sp)

# 方法一
# 读取空间数据
gs <- st_read('data/省.geojson') %>%
  filter(省=='甘肃省') %>%
  st_set_crs(4326)

boundary_coords <- st_coordinates(gs)
boundary_df <- data.frame(
  LONG = boundary_coords[,1],
  LAT = boundary_coords[,2]
)
# 使用fdmr包构建网格
fdmr::mesh_builder(spatial_data = boundary_df)


#方法二

# 如果你想手动创建INLA网格，需要先提取坐标
# 从sf对象提取坐标
coords <- st_coordinates(gs)  # 获取坐标

# 计算网格参数 - 基于经度和纬度范围
lon_range <- diff(range(coords[, 1]))/4  # 经度范围
lat_range <- diff(range(coords[, 2]))/2  # 纬度范围

# 使用较大的范围作为主要参考
max_range <- max(lon_range, lat_range)
mean_range <- mean(c(lon_range, lat_range))

# 计算网格参数
initial_range <- mean_range / 4  # 使用平均范围
# 如果想要更精确地考虑经纬度差异：
max_edge <- min(lon_range, lat_range) / 5 # 使用较小范围避免网格过粗


# 创建INLA网格
mesh <- fmesher::fm_mesh_2d_inla(
  loc = coords[,1:2],  # 使用提取的坐标
  max.edge = c(1, 2) * max_edge,
  offset = c(initial_range * 1.2, max_range),  # 内外边界偏移
  cutoff = max_edge / 7
)

plot(mesh)
fdmr::retrieve_tutorial_data(dataset = "covid")
sp_data <- fdmr::load_tutorial_data(dataset = "covid", filename = "spatial_data.rds")
covid19_data <- fdmr::load_tutorial_data(dataset = "covid", filename = "covid19_data.rds")

breaks_vec <- c(seq(as.Date("2020-03-07"),
                    as.Date("2022-03-26"),
                    by = "3 week"
), as.Date("2022-03-26"))

cases_week <- dplyr::group_by(covid19_data, date) %>% dplyr::summarize(cases = sum(cases))

fdmr::plot_barchart(data = cases_week, x = cases_week$date, y = cases_week$cases, breaks = breaks_vec, x_label = "Date", y_label = "Number of cases")
sp_data@data$mapp <- 0
domain <- sp_data@data$mapp

fdmr::plot_map(polygon_data = sp_data, domain = domain, add_scale_bar = TRUE, polygon_fill_opacity = 0.5, palette = "YlOrRd")

initial_range <- diff(range(sp_data@data[, "LONG"])) / 5

max_edge <- initial_range / 8

mesh <- fmesher::fm_mesh_2d_inla(
  loc = sp_data@data[, c("LONG", "LAT")],
  max.edge = c(1, 2) * max_edge,
  offset = c(initial_range / 4, initial_range),
  cutoff = max_edge / 7
)
point_data <- sp_data@data[, c("LONG", "LAT")]

fdmr::plot_mesh(mesh = mesh)

prior_range <- initial_range
spde <- INLA::inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(prior_range, 0.5),
  prior.sigma = c(1, 0.01)
)

rhoprior <- base::list(theta = list(prior = "pccor1", param = c(0, 0.9)))
group_index <- covid19_data$week
n_groups <- length(unique(covid19_data$week))
covid19_data <- covid19_data@data

formula <- cases ~ 0 + Intercept(1) + IMD +
  carebeds.ratio + AandETRUE +
  perc.chinese + perc.indian + perc.bangladeshi + perc.pakistani + perc.ba + perc.bc + perc.wb +
  age1 + age2 + age3 + age4 +
  pm25 + no2 +
  f(
    main = coordinates,
    model = spde,
    group = group_index,
    ngroup = n_groups,
    control.group = list(
      model = "ar1",
      hyper = rhoprior
    )
  )

inlabru_model <- inlabru::bru(formula,
                              data = covid19_data,
                              family = "poisson",
                              E = covid19_data$Population,
                              control.family = list(link = "log"),
                              options = list(
                                control.inla = list(
                                  reordering = "metis",
                                  int.strategy = "eb"
                                ),
                                verbose = TRUE,
                                inla.mode = "experimental"
                              )
)
model_summary <- summary(inlabru_model)
model_summary_fixed <- inlabru_model$summary.fixed
model_hyperparams <- inlabru_model$marginals.hyperpar

print(model_summary_fixed[2:nrow(model_summary_fixed), 1:5])

fdmr::model_viewer(inlabru_model)


###################
# 方法1: 使用边界约束创建mesh
# 从sf对象提取边界坐标
boundary_coords <- st_coordinates(st_boundary(gs))

# 如果是多边形，提取外边界
if(st_geometry_type(gs)[1] %in% c("POLYGON", "MULTIPOLYGON")) {
  # 对于多边形，提取边界点
  boundary_coords <- st_coordinates(st_cast(st_boundary(gs), "POINT"))
}

# 计算网格参数
bbox <- st_bbox(gs)
lon_range <- bbox[3] - bbox[1]  # xmax - xmin
lat_range <- bbox[4] - bbox[2]  # ymax - ymin

# 基于边界范围计算mesh参数
mean_range <- mean(c(lon_range, lat_range))
max_edge <- min(lon_range, lat_range) / 5  # 调整为更精细的网格

# 创建沿边界的mesh
mesh <- fmesher::fm_mesh_2d_inla(
  boundary = list(boundary_coords[,1:2]),  # 使用边界约束
  max.edge = c(max_edge, max_edge * 2),    # 内部和外部最大边长
  offset = c(max_edge * 0.5, mean_range * 0.3),  # 减小偏移量，更贴合边界
  cutoff = max_edge / 5                    # 最小距离
)

# 绘制mesh和边界
plot(mesh)
plot(st_geometry(gs), add = TRUE, border = "red", lwd = 2)


#####################
library(fmesher)
library(sf)
library(fmesher)
gs <- st_read('data/省.geojson') %>%
  filter(省=='甘肃省') %>%
  st_set_crs(4326)

# 直接从sf对象创建边界
boundary <- fm_as_segm(gs)

# 计算适应性网格参数
coords <- st_coordinates(gs)
lon_range <- diff(range(coords[, 1]))
lat_range <- diff(range(coords[, 2]))
mean_range <- mean(c(lon_range, lat_range))

# 创建紧贴边界的mesh
mesh <- fm_mesh_2d(
  boundary = boundary,
  max.edge = c(mean_range/10, mean_range/5),  # 内外边界的最大边长
  offset = c(mean_range/20, mean_range/8),    # 更小的偏移量
  cutoff = mean_range/50                     # 更精细的cutoff
)


mesh <- fm_mesh_2d(
  boundary = boundary,
  max.edge = c(mean_range/20, mean_range/10),  # 减小最大边长（原来的1/2）
  offset = c(mean_range/40, mean_range/15),    # 减小偏移量
  cutoff = mean_range/100                     # 减小cutoff（原来的1/2）
)


mesh <- fm_mesh_2d(
  boundary = boundary,
  max.edge = c(mean_range/30, mean_range/12),  # 更小的边长
  offset = c(mean_range/50, mean_range/18),    # 更小的偏移
  cutoff = mean_range/150,                    # 更小的cutoff
  min.angle = 20                              # 添加最小角度约束
)

# 可视化结果
plot(mesh, asp = 1)
plot(st_geometry(gs), add = TRUE, border = "red", lwd = 3, fill = NA)


#################################
library(fmesher)
library(sf)
library(ggplot2)

create_adaptive_mesh <- function(sf_object,
                                 inner_edge_ratio = 30,    # 增大内部边长比例（更密集）
                                 outer_edge_ratio = 5,     # 减小外部边长比例（更稀疏）
                                 inner_offset_ratio = 8,   # 减小内部偏移比例（内部区域更大）
                                 outer_offset_ratio = 20,  # 增大外部偏移比例（外部区域相对更小）
                                 cutoff_ratio = 80,        # 增大cutoff比例（避免过小三角形）
                                 crs_transform = NULL) {   # 可选的坐标转换

  # 坐标系转换（如果需要）
  if (!is.null(crs_transform)) {
    sf_object <- st_transform(sf_object, crs_transform)
  }

  # 创建边界
  boundary <- fm_as_segm(sf_object)

  # 计算自适应参数
  coords <- st_coordinates(sf_object)
  lon_range <- diff(range(coords[, 1]))
  lat_range <- diff(range(coords[, 2]))
  mean_range <- mean(c(lon_range, lat_range))

  # 关键修改：调整max.edge参数
  # 第一个参数控制边界内部，第二个参数控制边界外部
  inner_max_edge <- mean_range/inner_edge_ratio  # 内部：更小的边长 = 更密集
  outer_max_edge <- mean_range/outer_edge_ratio  # 外部：更大的边长 = 更稀疏

  # 创建mesh
  mesh <- fm_mesh_2d(
    boundary = boundary,
    max.edge = c(inner_max_edge, outer_max_edge),  # 内部小，外部大
    offset = c(mean_range/inner_offset_ratio, mean_range/outer_offset_ratio),
    cutoff = mean_range/cutoff_ratio
  )

  # 打印网格信息
  cat("网格参数信息:\n")
  cat("- 内部最大边长:", round(inner_max_edge, 4), "\n")
  cat("- 外部最大边长:", round(outer_max_edge, 4), "\n")
  cat("- 密度比例 (外部/内部):", round(outer_max_edge/inner_max_edge, 2), "\n")
  cat("- 顶点总数:", mesh$n, "\n")
  cat("- 三角形总数:", nrow(mesh$graph$tv), "\n")

  # 返回mesh和元信息
  list(
    mesh = mesh,
    boundary = boundary,
    range_info = c(lon_range = lon_range, lat_range = lat_range, mean_range = mean_range),
    vertices_count = mesh$n,
    triangles_count = nrow(mesh$graph$tv),
    edge_ratio = outer_max_edge/inner_max_edge
  )
}

# 可视化函数
plot_mesh_density <- function(mesh_result) {
  mesh <- mesh_result$mesh

  # 转换为数据框用于ggplot
  vertices_df <- data.frame(
    x = mesh$loc[,1],
    y = mesh$loc[,2]
  )

  triangles_df <- data.frame(
    triangle_id = 1:nrow(mesh$graph$tv),
    mesh$graph$tv
  )

  ggplot() +
    geom_point(data = vertices_df, aes(x = x, y = y),
               size = 0.3, alpha = 0.6, color = "blue") +
    theme_minimal() +
    coord_equal() +
    labs(title = "自适应网格分布图",
         subtitle = paste("内部密集，外部稀疏 | 顶点数:", mesh_result$vertices_count),
         x = "经度", y = "纬度") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}


# 使用示例
gs <- st_read('data/省.geojson') %>%
  filter(省=='甘肃省') %>%
  st_set_crs(4326)

# 创建mesh - 参数已经按照内密外疏原则设置
mesh_result <- create_adaptive_mesh(
  gs,
  inner_edge_ratio = 35,   # 内部更密集（增大比例）
  outer_edge_ratio = 4,    # 外部更稀疏（减小比例）
  inner_offset_ratio = 6,  # 内部区域更大
  outer_offset_ratio = 15, # 外部区域适中
  cutoff_ratio = 100       # 避免过细小的三角形
)

mesh <- mesh_result$mesh

plot(mesh_result$mesh)

# 可视化网格
plot_mesh_density(mesh_result)

# 2. 高级可视化
plot_mesh_advanced <- function(mesh, sf_boundary = NULL) {
  # 转换为数据框
  mesh_df <- data.frame(
    x = mesh$loc[, 1],
    y = mesh$loc[, 2]
  )

  # 提取三角形
  triangles <- mesh$graph$tv
  triangle_list <- list()
  for (i in 1:nrow(triangles)) {
    triangle_list[[i]] <- data.frame(
      x = mesh$loc[triangles[i, ], 1],
      y = mesh$loc[triangles[i, ], 2],
      group = i
    )
  }
  triangle_df <- do.call(rbind, triangle_list)

  p <- ggplot() +
    geom_polygon(data = triangle_df,
                 aes(x = x, y = y, group = group),
                 fill = "lightblue", color = "white", alpha = 0.7, size = 0.2) +
    geom_point(data = mesh_df, aes(x = x, y = y),
               color = "red", size = 0.5, alpha = 0.8)

  # 添加边界
  if (!is.null(sf_boundary)) {
    p <- p + geom_sf(data = sf_boundary, fill = NA, color = "black", size = 1)
  }

  p + coord_sf(expand = FALSE) +
    theme_minimal() +
    labs(title = "INLA Mesh Visualization",
         subtitle = paste("Vertices:", nrow(mesh_df), "Triangles:", nrow(triangles)))
}

# 绘制
plot_mesh_advanced(mesh_result$mesh, gs)


# 2. 参数优化函数
optimize_mesh_parameters <- function(sf_object, target_vertices = 1000) {
  # 调整参数范围：确保内部密集（高比例），外部稀疏（低比例）
  param_grid <- expand.grid(
    inner_edge_ratio = c(25, 30, 35, 40, 45),      # 内部密集：增大比例范围
    outer_edge_ratio = c(2, 3, 4, 5, 6),           # 外部稀疏：减小比例范围
    inner_offset_ratio = c(5, 8, 10, 12, 15),      # 内部区域大小
    outer_offset_ratio = c(12, 15, 18, 20, 25),    # 外部区域大小
    cutoff_ratio = c(80, 100, 120, 140, 160)       # 避免过小三角形
  )

  cat("总共需要测试", nrow(param_grid), "种参数组合...\n")
  cat("参数设置原则：内部密集网格，外部稀疏网格\n")

  results <- list()
  successful_count <- 0

  for (i in 1:nrow(param_grid)) {
    # 显示进度
    if (i %% 200 == 0 || i == nrow(param_grid)) {
      cat("已完成:", i, "/", nrow(param_grid), "种组合\n")
    }

    # 验证参数合理性：确保内部比外部密集
    inner_ratio <- param_grid$inner_edge_ratio[i]
    outer_ratio <- param_grid$outer_edge_ratio[i]

    # 跳过不合理的参数组合（内部应该比外部密集）
    if (inner_ratio <= outer_ratio) {
      next
    }

    tryCatch({
      mesh_result <- create_adaptive_mesh(
        sf_object,
        inner_edge_ratio = inner_ratio,
        outer_edge_ratio = outer_ratio,
        inner_offset_ratio = param_grid$inner_offset_ratio[i],
        outer_offset_ratio = param_grid$outer_offset_ratio[i],
        cutoff_ratio = param_grid$cutoff_ratio[i]
      )

      quality <- assess_mesh_quality(mesh_result$mesh)

      # 计算密度对比度（外部/内部边长比）
      coords <- st_coordinates(sf_object)
      mean_range <- mean(c(diff(range(coords[, 1])), diff(range(coords[, 2]))))
      inner_edge <- mean_range / inner_ratio
      outer_edge <- mean_range / outer_ratio
      density_contrast <- outer_edge / inner_edge

      # 综合评分
      # 1. 顶点数与目标的差异（标准化）
      vertex_penalty <- abs(quality$vertices_count - target_vertices) / target_vertices
      # 2. 面积变异系数惩罚
      area_penalty <- quality$area_cv
      # 3. 密度对比度奖励（密度对比越大越好）
      density_bonus <- max(0, (density_contrast - 2) / 10)  # 当对比度>2时给予奖励
      # 4. 综合评分（越小越好）
      total_score <- vertex_penalty + area_penalty * 1.5 - density_bonus

      results[[successful_count + 1]] <- data.frame(
        inner_edge_ratio = inner_ratio,
        outer_edge_ratio = outer_ratio,
        inner_offset_ratio = param_grid$inner_offset_ratio[i],
        outer_offset_ratio = param_grid$outer_offset_ratio[i],
        cutoff_ratio = param_grid$cutoff_ratio[i],
        vertices = quality$vertices_count,
        area_cv = quality$area_cv,
        density_contrast = density_contrast,
        vertex_penalty = vertex_penalty,
        area_penalty = area_penalty,
        density_bonus = density_bonus,
        score = total_score
      )

      successful_count <- successful_count + 1

    }, error = function(e) {
      # 静默处理错误，继续下一个参数组合
      NULL
    })
  }

  if (length(results) == 0) {
    stop("没有成功的参数组合，请检查输入数据或扩大参数范围")
  }

  # 合并所有结果
  all_results <- do.call(rbind, results)

  # 按综合评分排序
  all_results <- all_results[order(all_results$score), ]

  # 打印最优参数信息
  best <- all_results[1, ]
  cat("\n=== 最优参数组合 ===\n")
  cat("内部边长比例:", best$inner_edge_ratio, "\n")
  cat("外部边长比例:", best$outer_edge_ratio, "\n")
  cat("密度对比度:", round(best$density_contrast, 2), "\n")
  cat("顶点数量:", best$vertices, "\n")
  cat("面积变异系数:", round(best$area_cv, 4), "\n")

  # 返回最优参数和所有结果
  list(
    best_params = all_results[1, ],
    top_10 = head(all_results, 10),
    all_results = all_results,
    summary = list(
      total_combinations = nrow(param_grid),
      successful_combinations = nrow(all_results),
      success_rate = nrow(all_results) / nrow(param_grid),
      avg_density_contrast = mean(all_results$density_contrast)
    )
  )
}


# 专门的密度对比分析函数
analyze_mesh_density_contrast <- function(optimization_result) {
  results <- optimization_result$all_results

  cat("=== 网格密度对比分析 ===\n")
  cat("平均密度对比度:", round(mean(results$density_contrast), 2), "\n")
  cat("密度对比度范围:", round(range(results$density_contrast), 2), "\n")

  # 按密度对比度分组分析
  high_contrast <- results[results$density_contrast >= 5, ]
  medium_contrast <- results[results$density_contrast >= 3 & results$density_contrast < 5, ]
  low_contrast <- results[results$density_contrast < 3, ]

  cat("\n高密度对比(≥5倍)组合:", nrow(high_contrast), "个\n")
  cat("中密度对比(3-5倍)组合:", nrow(medium_contrast), "个\n")
  cat("低密度对比(<3倍)组合:", nrow(low_contrast), "个\n")

  if (nrow(high_contrast) > 0) {
    cat("\n推荐高密度对比参数:\n")
    print(head(high_contrast[order(high_contrast$score), ], 3))
  }
}

# 运行优化
opt_result <- optimize_mesh_parameters(gs, target_vertices = 800)

# 分析密度对比
analyze_mesh_density_contrast(opt_result)

# 使用最优参数创建最终网格
best_mesh <- create_adaptive_mesh(
  gs,
  inner_edge_ratio = opt_result$best_params$inner_edge_ratio,
  outer_edge_ratio = opt_result$best_params$outer_edge_ratio,
  inner_offset_ratio = opt_result$best_params$inner_offset_ratio,
  outer_offset_ratio = opt_result$best_params$outer_offset_ratio,
  cutoff_ratio = opt_result$best_params$cutoff_ratio
)

# 可视化最优结果
plot(best_mesh$mesh)
points(best_mesh$mesh$loc, pch = 16, cex = 0.3)
title(sprintf("最优网格 (顶点数: %d, 面积CV: %.3f)",
              best_quality$vertices_count, best_quality$area_cv))



# 主评估函数
comprehensive_mesh_assessment <- function(mesh, original_sf = NULL) {

  # 基本信息
  n_vertices <- nrow(mesh$loc)
  n_triangles <- nrow(mesh$graph$tv)

  # 1. 几何质量指标
  geometric_quality <- assess_geometric_quality(mesh)

  # 2. 拓扑质量指标
  topological_quality <- assess_topological_quality(mesh)

  # 3. 数值稳定性指标
  numerical_quality <- assess_numerical_quality(mesh)

  # 4. 应用特定指标（如果提供原始边界）
  application_quality <- NULL
  if (!is.null(original_sf)) {
    application_quality <- assess_application_quality(mesh, original_sf)
  }

  # 综合得分 (0-100, 100最好)
  overall_score <- calculate_overall_score(
    geometric_quality, topological_quality, numerical_quality
  )

  return(list(
    basic_info = list(
      vertices = n_vertices,
      triangles = n_triangles,
      density = n_vertices / (max(mesh$loc[,1]) - min(mesh$loc[,1])) /
        (max(mesh$loc[,2]) - min(mesh$loc[,2]))
    ),
    geometric = geometric_quality,
    topological = topological_quality,
    numerical = numerical_quality,
    application = application_quality,
    overall_score = overall_score,
    quality_level = get_quality_level(overall_score)
  ))
}

# 1. 几何质量评估
assess_geometric_quality <- function(mesh) {
  triangles <- mesh$graph$tv
  vertices <- mesh$loc

  # 计算每个三角形的几何属性
  areas <- c()
  aspect_ratios <- c()
  min_angles <- c()
  max_angles <- c()

  for (i in 1:nrow(triangles)) {
    # 获取三角形顶点
    v1 <- vertices[triangles[i,1], ]
    v2 <- vertices[triangles[i,2], ]
    v3 <- vertices[triangles[i,3], ]

    # 面积
    area <- abs((v2[1]-v1[1])*(v3[2]-v1[2]) - (v3[1]-v1[1])*(v2[2]-v1[2])) / 2
    areas <- c(areas, area)

    # 边长
    edge1 <- sqrt(sum((v2-v1)^2))
    edge2 <- sqrt(sum((v3-v2)^2))
    edge3 <- sqrt(sum((v1-v3)^2))

    # 纵横比 (最长边/最短边)
    aspect_ratio <- max(edge1, edge2, edge3) / min(edge1, edge2, edge3)
    aspect_ratios <- c(aspect_ratios, aspect_ratio)

    # 角度计算
    angles <- calculate_triangle_angles(v1, v2, v3)
    min_angles <- c(min_angles, min(angles))
    max_angles <- c(max_angles, max(angles))
  }

  return(list(
    area_stats = list(
      mean = mean(areas),
      cv = sd(areas) / mean(areas),  # 变异系数
      min = min(areas),
      max = max(areas),
      uniformity_score = max(0, 1 - (sd(areas) / mean(areas)))  # 越接近1越均匀
    ),
    shape_quality = list(
      mean_aspect_ratio = mean(aspect_ratios),
      max_aspect_ratio = max(aspect_ratios),
      aspect_ratio_score = max(0, min(1, 2 / mean(aspect_ratios))),  # 越接近1越好
      min_angle_mean = mean(min_angles),
      min_angle_worst = min(min_angles),
      angle_quality_score = max(0, min(1, mean(min_angles) / 60))  # 理想最小角度60度
    )
  ))
}

# 2. 拓扑质量评估
assess_topological_quality <- function(mesh) {
  vertices <- mesh$loc
  triangles <- mesh$graph$tv

  # 计算每个顶点的度数（连接的三角形数量）
  vertex_degrees <- table(as.vector(triangles))

  # 边界检测
  edges <- rbind(
    triangles[, c(1,2)],
    triangles[, c(2,3)],
    triangles[, c(1,3)]
  )
  edge_counts <- table(paste(pmin(edges[,1], edges[,2]),
                             pmax(edges[,1], edges[,2])))
  boundary_edges <- sum(edge_counts == 1)
  internal_edges <- sum(edge_counts == 2)
  irregular_edges <- sum(edge_counts > 2)  # 不应该存在

  return(list(
    connectivity = list(
      mean_vertex_degree = mean(vertex_degrees),
      vertex_degree_cv = sd(vertex_degrees) / mean(vertex_degrees),
      isolated_vertices = sum(vertex_degrees == 0),  # 不应该存在
      irregular_vertices = sum(vertex_degrees > 8)   # 度数过高的顶点
    ),
    topology = list(
      boundary_edges = boundary_edges,
      internal_edges = internal_edges,
      irregular_edges = irregular_edges,
      topology_score = ifelse(irregular_edges == 0, 1, 0)
    )
  ))
}

# 3. 数值稳定性评估
assess_numerical_quality <- function(mesh) {
  # 计算刚度矩阵的条件数（简化版本）
  triangles <- mesh$graph$tv
  vertices <- mesh$loc

  # 最小三角形面积（影响数值稳定性）
  min_area <- Inf
  areas <- c()

  for (i in 1:nrow(triangles)) {
    v1 <- vertices[triangles[i,1], ]
    v2 <- vertices[triangles[i,2], ]
    v3 <- vertices[triangles[i,3], ]
    area <- abs((v2[1]-v1[1])*(v3[2]-v1[2]) - (v3[1]-v1[1])*(v2[2]-v1[2])) / 2
    areas <- c(areas, area)
    min_area <- min(min_area, area)
  }

  # 最小边长
  min_edge_length <- Inf
  for (i in 1:nrow(triangles)) {
    v1 <- vertices[triangles[i,1], ]
    v2 <- vertices[triangles[i,2], ]
    v3 <- vertices[triangles[i,3], ]

    edge1 <- sqrt(sum((v2-v1)^2))
    edge2 <- sqrt(sum((v3-v2)^2))
    edge3 <- sqrt(sum((v1-v3)^2))

    min_edge_length <- min(min_edge_length, edge1, edge2, edge3)
  }

  # 面积比值（最大面积/最小面积）
  area_ratio <- max(areas) / min(areas)

  return(list(
    stability = list(
      min_area = min_area,
      min_edge_length = min_edge_length,
      area_ratio = area_ratio,
      degeneracy_risk = ifelse(min_area < 1e-10, "High",
                               ifelse(min_area < 1e-6, "Medium", "Low")),
      numerical_score = max(0, min(1, log10(min_area + 1e-12) + 12) / 12)
    )
  ))
}

# 4. 应用特定质量评估
assess_application_quality <- function(mesh, original_sf) {
  # 边界拟合质量
  boundary_quality <- assess_boundary_fitting(mesh, original_sf)

  # 分辨率适应性
  resolution_quality <- assess_resolution_adaptation(mesh, original_sf)

  return(list(
    boundary_fitting = boundary_quality,
    resolution_adaptation = resolution_quality
  ))
}

# 边界拟合质量评估
assess_boundary_fitting <- function(mesh, original_sf) {
  # 这是一个简化的边界拟合评估
  # 在实际应用中可能需要更复杂的几何计算

  tryCatch({
    # 获取网格边界点
    boundary_vertices <- get_boundary_vertices(mesh)

    # 简单的边界覆盖评估
    mesh_bbox <- list(
      xmin = min(mesh$loc[,1]),
      xmax = max(mesh$loc[,1]),
      ymin = min(mesh$loc[,2]),
      ymax = max(mesh$loc[,2])
    )

    sf_bbox <- sf::st_bbox(original_sf)

    # 计算边界框匹配度
    bbox_match <- 1 - (
      abs(mesh_bbox$xmin - sf_bbox$xmin) +
        abs(mesh_bbox$xmax - sf_bbox$xmax) +
        abs(mesh_bbox$ymin - sf_bbox$ymin) +
        abs(mesh_bbox$ymax - sf_bbox$ymax)
    ) / (sf_bbox$xmax - sf_bbox$xmin + sf_bbox$ymax - sf_bbox$ymin)

    return(list(
      boundary_vertices_count = length(boundary_vertices),
      bbox_match_score = max(0, min(1, bbox_match)),
      fitting_quality = "Estimated"  # 标记这是估算值
    ))

  }, error = function(e) {
    return(list(
      boundary_vertices_count = NA,
      bbox_match_score = 0.5,  # 默认中等分数
      fitting_quality = "Cannot assess",
      error = as.character(e)
    ))
  })
}

# 分辨率适应性评估
assess_resolution_adaptation <- function(mesh, original_sf) {
  tryCatch({
    # 简化的分辨率评估
    n_vertices <- nrow(mesh$loc)

    # 根据几何复杂度评估顶点密度是否合适
    area_total <- as.numeric(sf::st_area(original_sf))
    vertex_density <- n_vertices / area_total

    # 评估网格分辨率是否适中
    # 这个阈值可能需要根据具体应用调整
    resolution_score <- ifelse(vertex_density > 1e-6 & vertex_density < 1e-2,
                               1, 0.5)

    return(list(
      vertex_density = vertex_density,
      total_area = area_total,
      resolution_score = resolution_score,
      adaptation_quality = ifelse(resolution_score > 0.8, "Good", "Moderate")
    ))

  }, error = function(e) {
    return(list(
      vertex_density = NA,
      total_area = NA,
      resolution_score = 0.5,
      adaptation_quality = "Cannot assess",
      error = as.character(e)
    ))
  })
}

# 获取边界顶点（简化版本）
get_boundary_vertices <- function(mesh) {
  triangles <- mesh$graph$tv

  # 找到所有边
  edges <- rbind(
    cbind(triangles[,1], triangles[,2]),
    cbind(triangles[,2], triangles[,3]),
    cbind(triangles[,3], triangles[,1])
  )

  # 标准化边（小索引在前）
  edges_normalized <- t(apply(edges, 1, function(x) sort(x)))

  # 找到只出现一次的边（边界边）
  edge_counts <- table(apply(edges_normalized, 1, paste, collapse = "-"))
  boundary_edges <- names(edge_counts[edge_counts == 1])

  # 提取边界顶点
  boundary_vertices <- unique(unlist(lapply(boundary_edges, function(e) {
    as.numeric(strsplit(e, "-")[[1]])
  })))

  return(boundary_vertices)
}

# 辅助函数
calculate_triangle_angles <- function(v1, v2, v3) {
  # 计算三角形的三个角degree（度）
  edge1 <- sqrt(sum((v2-v1)^2))
  edge2 <- sqrt(sum((v3-v2)^2))
  edge3 <- sqrt(sum((v1-v3)^2))

  # 防止数值错误
  edge1 <- max(edge1, 1e-10)
  edge2 <- max(edge2, 1e-10)
  edge3 <- max(edge3, 1e-10)

  # 余弦定理
  cos_angle1 <- (edge1^2 + edge3^2 - edge2^2) / (2*edge1*edge3)
  cos_angle2 <- (edge1^2 + edge2^2 - edge3^2) / (2*edge1*edge2)
  cos_angle3 <- (edge2^2 + edge3^2 - edge1^2) / (2*edge2*edge3)

  # 限制余弦值范围以避免数值错误
  cos_angle1 <- max(-1, min(1, cos_angle1))
  cos_angle2 <- max(-1, min(1, cos_angle2))
  cos_angle3 <- max(-1, min(1, cos_angle3))

  angle1 <- acos(cos_angle1) * 180/pi
  angle2 <- acos(cos_angle2) * 180/pi
  angle3 <- acos(cos_angle3) * 180/pi

  return(c(angle1, angle2, angle3))
}

calculate_overall_score <- function(geom, topo, numer) {
  # 综合评分权重
  weights <- c(0.4, 0.3, 0.3)  # 几何、拓扑、数值

  geom_score <- (geom$area_stats$uniformity_score * 0.4 +
                   geom$shape_quality$aspect_ratio_score * 0.3 +
                   geom$shape_quality$angle_quality_score * 0.3)

  topo_score <- topo$topology$topology_score

  numer_score <- numer$stability$numerical_score

  overall <- weights[1] * geom_score +
    weights[2] * topo_score +
    weights[3] * numer_score

  return(max(0, overall * 100))  # 转换为0-100分
}

get_quality_level <- function(score) {
  if (score >= 80) return("优秀 (Excellent)")
  if (score >= 70) return("良好 (Good)")
  if (score >= 60) return("可接受 (Acceptable)")
  if (score >= 50) return("需要改进 (Needs Improvement)")
  return("不合格 (Poor)")
}

# 网格质量可视化
visualize_mesh_quality <- function(mesh, quality_assessment) {
  par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

  # 1. 基本网格结构
  plot(mesh, main = "网格结构")
  points(mesh$loc, pch = 16, cex = 0.3, col = "red")

  # 2. 顶点度数分布
  triangles <- mesh$graph$tv
  vertex_degrees <- as.vector(table(as.vector(triangles)))
  hist(vertex_degrees, main = "顶点度数分布",
       xlab = "度数", ylab = "频率", col = "lightblue")

  # 3. 三角形面积分布
  triangles <- mesh$graph$tv
  vertices <- mesh$loc
  areas <- c()
  for (i in 1:nrow(triangles)) {
    v1 <- vertices[triangles[i,1], ]
    v2 <- vertices[triangles[i,2], ]
    v3 <- vertices[triangles[i,3], ]
    area <- abs((v2[1]-v1[1])*(v3[2]-v1[2]) - (v3[1]-v1[1])*(v2[2]-v1[2])) / 2
    areas <- c(areas, area)
  }
  hist(areas, main = "三角形面积分布",
       xlab = "面积", ylab = "频率", col = "lightgreen")

  # 4. 质量指标柱状图
  scores <- c(
    quality_assessment$geometric$area_stats$uniformity_score * 100,
    quality_assessment$geometric$shape_quality$aspect_ratio_score * 100,
    quality_assessment$topological$topology$topology_score * 100,
    quality_assessment$numerical$stability$numerical_score * 100
  )

  barplot(scores,
          names.arg = c("面积\n均匀性", "形状\n质量", "拓扑\n质量", "数值\n稳定性"),
          main = "质量指标分解", ylim = c(0, 100),
          col = c("skyblue", "lightcoral", "lightgreen", "orange"))
  abline(h = c(60, 80), lty = 2, col = "red")

  # 5. 整体质量摘要
  plot.new()
  text(0.5, 0.8, "网格质量摘要", cex = 1.5, adj = 0.5, font = 2)
  text(0.5, 0.65, paste("整体得分:", round(quality_assessment$overall_score, 1)),
       cex = 1.8, adj = 0.5, col = "blue")
  text(0.5, 0.5, quality_assessment$quality_level, cex = 1.3, adj = 0.5,
       col = ifelse(quality_assessment$overall_score >= 70, "green", "orange"))
  text(0.5, 0.35, paste("顶点数:", quality_assessment$basic_info$vertices),
       cex = 1.1, adj = 0.5)
  text(0.5, 0.25, paste("三角形数:", quality_assessment$basic_info$triangles),
       cex = 1.1, adj = 0.5)
  text(0.5, 0.1, paste("平均纵横比:", round(quality_assessment$geometric$shape_quality$mean_aspect_ratio, 2)),
       cex = 1, adj = 0.5)

  # 6. 角度质量分布
  triangles <- mesh$graph$tv
  vertices <- mesh$loc
  all_angles <- c()

  for (i in 1:nrow(triangles)) {
    v1 <- vertices[triangles[i,1], ]
    v2 <- vertices[triangles[i,2], ]
    v3 <- vertices[triangles[i,3], ]
    angles <- calculate_triangle_angles(v1, v2, v3)
    all_angles <- c(all_angles, angles)
  }

  hist(all_angles, main = "角度分布",
       xlab = "角度 (度)", ylab = "频率", col = "lightyellow",
       xlim = c(0, 180))
  abline(v = c(30, 60, 120), col = "red", lty = 2)
  legend("topright", c("30°", "60°", "120°"), col = "red", lty = 2, cex = 0.8)

  par(mfrow = c(1, 1))
}

# 简化版质量评估（快速版本）
quick_mesh_assessment <- function(mesh) {
  triangles <- mesh$graph$tv
  vertices <- mesh$loc

  # 快速计算关键指标
  areas <- c()
  aspect_ratios <- c()
  min_angles <- c()

  for (i in 1:min(100, nrow(triangles))) {  # 只采样前100个三角形
    v1 <- vertices[triangles[i,1], ]
    v2 <- vertices[triangles[i,2], ]
    v3 <- vertices[triangles[i,3], ]

    # 面积
    area <- abs((v2[1]-v1[1])*(v3[2]-v1[2]) - (v3[1]-v1[1])*(v2[2]-v1[2])) / 2
    areas <- c(areas, area)

    # 边长和纵横比
    edge1 <- sqrt(sum((v2-v1)^2))
    edge2 <- sqrt(sum((v3-v2)^2))
    edge3 <- sqrt(sum((v1-v3)^2))
    aspect_ratio <- max(edge1, edge2, edge3) / min(edge1, edge2, edge3)
    aspect_ratios <- c(aspect_ratios, aspect_ratio)

    # 最小角度
    angles <- calculate_triangle_angles(v1, v2, v3)
    min_angles <- c(min_angles, min(angles))
  }

  # 快速评分
  area_cv <- sd(areas) / mean(areas)
  mean_aspect_ratio <- mean(aspect_ratios)
  mean_min_angle <- mean(min_angles)

  # 综合快速评分
  quick_score <- (
    max(0, 1 - area_cv) * 40 +               # 面积均匀性 (40%)
      max(0, min(1, 2/mean_aspect_ratio)) * 30 + # 形状质量 (30%)
      max(0, min(1, mean_min_angle/30)) * 30    # 角度质量 (30%)
  )

  quality_level <- ifelse(quick_score >= 70, "良好",
                          ifelse(quick_score >= 50, "可接受", "需要改进"))

  return(list(
    vertices = nrow(vertices),
    triangles = nrow(triangles),
    area_cv = area_cv,
    mean_aspect_ratio = mean_aspect_ratio,
    mean_min_angle = mean_min_angle,
    quick_score = quick_score,
    quality_level = quality_level
  ))
}

# 生成质量报告
generate_quality_report <- function(quality_result) {
  cat("=== 网格质量评估详细报告 ===\n\n")

  # 基本信息
  cat("【基本信息】\n")
  cat("顶点数量:", quality_result$basic_info$vertices, "\n")
  cat("三角形数量:", quality_result$basic_info$triangles, "\n")
  cat("顶点密度:", round(quality_result$basic_info$density, 6), "\n\n")

  # 整体评分
  cat("【整体评估】\n")
  cat("综合得分:", round(quality_result$overall_score, 1), "/100\n")
  cat("质量等级:", quality_result$quality_level, "\n\n")

  # 几何质量
  cat("【几何质量】\n")
  cat("面积统计:\n")
  cat("  - 平均面积:", format(quality_result$geometric$area_stats$mean, scientific = TRUE), "\n")
  cat("  - 面积变异系数:", round(quality_result$geometric$area_stats$cv, 3), "\n")
  cat("  - 面积均匀性得分:", round(quality_result$geometric$area_stats$uniformity_score, 3), "\n")
  cat("形状质量:\n")
  cat("  - 平均纵横比:", round(quality_result$geometric$shape_quality$mean_aspect_ratio, 2), "\n")
  cat("  - 最大纵横比:", round(quality_result$geometric$shape_quality$max_aspect_ratio, 2), "\n")
  cat("  - 平均最小角度:", round(quality_result$geometric$shape_quality$min_angle_mean, 1), "度\n")
  cat("  - 最小角度:", round(quality_result$geometric$shape_quality$min_angle_worst, 1), "度\n\n")

  # 拓扑质量
  cat("【拓扑质量】\n")
  cat("连接性:\n")
  cat("  - 平均顶点度数:", round(quality_result$topological$connectivity$mean_vertex_degree, 1), "\n")
  cat("  - 孤立顶点数:", quality_result$topological$connectivity$isolated_vertices, "\n")
  cat("拓扑结构:\n")
  cat("  - 边界边数:", quality_result$topological$topology$boundary_edges, "\n")
  cat("  - 内部边数:", quality_result$topological$topology$internal_edges, "\n")
  cat("  - 异常边数:", quality_result$topological$topology$irregular_edges, "\n\n")

  # 数值稳定性
  cat("【数值稳定性】\n")
  cat("最小面积:", format(quality_result$numerical$stability$min_area, scientific = TRUE), "\n")
  cat("最小边长:", format(quality_result$numerical$stability$min_edge_length, scientific = TRUE), "\n")
  cat("退化风险:", quality_result$numerical$stability$degeneracy_risk, "\n")
  cat("面积比值:", round(quality_result$numerical$stability$area_ratio, 1), "\n\n")

  # 应用质量（如果有）
  if (!is.null(quality_result$application)) {
    cat("【应用适应性】\n")
    if (!is.null(quality_result$application$boundary_fitting)) {
      cat("边界拟合:\n")
      cat("  - 边界顶点数:", quality_result$application$boundary_fitting$boundary_vertices_count, "\n")
      cat("  - 边界匹配度:", round(quality_result$application$boundary_fitting$bbox_match_score, 3), "\n")
    }
    if (!is.null(quality_result$application$resolution_adaptation)) {
      cat("分辨率适应:\n")
      if (!is.na(quality_result$application$resolution_adaptation$vertex_density)) {
        cat("  - 顶点密度:", format(quality_result$application$resolution_adaptation$vertex_density, scientific = TRUE), "\n")
        cat("  - 总面积:", format(quality_result$application$resolution_adaptation$total_area, scientific = TRUE), "\n")
      }
      cat("  - 分辨率评价:", quality_result$application$resolution_adaptation$adaptation_quality, "\n")
    }
    cat("\n")
  }

  # 改进建议
  cat("【改进建议】\n")
  if (quality_result$overall_score < 60) {
    cat("- 网格质量较差，建议重新生成\n")
    if (quality_result$geometric$area_stats$cv > 0.5) {
      cat("- 面积分布不均，建议调整edge ratio参数\n")
    }
    if (quality_result$geometric$shape_quality$mean_aspect_ratio > 3) {
      cat("- 存在过度细长三角形，建议增加inner_edge_ratio\n")
    }
    if (quality_result$geometric$shape_quality$min_angle_mean < 20) {
      cat("- 角度质量差，建议调整offset参数\n")
    }
  } else if (quality_result$overall_score < 80) {
    cat("- 网格质量可接受，可考虑微调参数进一步优化\n")
  } else {
    cat("- 网格质量优秀，无需特殊调整\n")
  }
}


# 评估网格质量
quality_result <- comprehensive_mesh_assessment(best_mesh$mesh, gs)

# 打印详细报告
cat("=== 网格质量评估报告 ===\n")
cat("整体得分:", round(quality_result$overall_score, 1), "/100\n")
cat("质量等级:", quality_result$quality_level, "\n\n")

cat("几何质量:\n")
cat("- 面积均匀性:", round(quality_result$geometric$area_stats$uniformity_score, 3), "\n")
cat("- 平均纵横比:", round(quality_result$geometric$shape_quality$mean_aspect_ratio, 2), "\n")
cat("- 最小角度均值:", round(quality_result$geometric$shape_quality$min_angle_mean, 1), "度\n\n")

cat("拓扑质量:\n")
cat("- 拓扑正确性:", quality_result$topological$topology$topology_score, "\n")
cat("- 平均顶点度数:", round(quality_result$topological$connectivity$mean_vertex_degree, 1), "\n\n")

cat("数值稳定性:\n")
cat("- 退化风险:", quality_result$numerical$stability$degeneracy_risk, "\n")
cat("- 最小面积:", format(quality_result$numerical$stability$min_area, scientific = TRUE), "\n")

# 可视化质量
visualize_mesh_quality(best_mesh$mesh, quality_result)


