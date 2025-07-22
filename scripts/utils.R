library(tidyverse)
library(brms)
library(INLA)
library(inlabru)
library(sf)
# remotes::install_github("julianfaraway/brinla")
library(brinla)
library(bayesplot)
library(INLAspacetime)
library(haven)
library(knitr)
library(kableExtra)
library(patchwork)
library(viridis)
library(SpatialEpi)
library(flextable)
library(showtext)
# 自动使用系统字体
showtext_auto(enable = TRUE)

# format_kable_table <- function(data, width = NULL, align = NULL) {
#   # 导入必要的包（如果尚未加载）
#   require(kableExtra)

#   创建并格式化kable表格
#   data %>%
#     kable(align = align) %>%
#     kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
#     column_spec(1, bold = TRUE, monospace = TRUE) %>%
#     column_spec(2, width = width)

#   data %>%
#     kable(align = align) %>%
#     kable_styling(full_width = FALSE) %>%
#     column_spec(1, bold = TRUE) %>%
#     column_spec(2, width = width)
# }

# format_kable_table <- function(data) {
#   required_cols <- c("变量名", "描述")
#   if (!all(required_cols %in% names(data))) {
#     stop("数据框必须包含 '变量名', 和 '描述' 列")
#   }
#
#   # 创建markdown表格
#   cat("| 变量名 | 描述 |\n")  # 修改：添加|作为列结束符
#   cat("|:--|:--|\n")  # 修改：添加|作为列结束符
#
#   # 添加每一行数据
#   for (i in 1:nrow(data)) {
#     cat("|", data$变量名[i], "|",  # 修改：使用正确的列名和分隔符
#         data$描述[i], "|\n")  # 修改：添加|作为列结束符
#   }
# }

format_kable_table <- function(data){
  flextable(data) %>%
    autofit() %>%
    colformat_num(big.mark = "")
}


