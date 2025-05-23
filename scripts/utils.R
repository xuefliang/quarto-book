library(tidyverse)
library(brms)
library(INLA)
library(inlabru)
# remotes::install_github("julianfaraway/brinla")
library(brinla)
library(bayesplot)
library(INLAspacetime)
library(haven)
library(knitr)
library(kableExtra)

format_kable_table <- function(data, width = NULL, align = NULL) {
  # 导入必要的包（如果尚未加载）
  require(kableExtra)
  
  # 创建并格式化kable表格
  data %>%
    kable(align = align) %>%
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
    column_spec(1, bold = TRUE, monospace = TRUE) %>%
    column_spec(2, width = width)
}


