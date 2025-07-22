


# Load required packages
library(haven)      # For read_dta
library(R2OpenBUGS) # For bugs()

# Read data and ensure numeric variables
coronary <- read_dta("data/coronary.dta") %>%
  mutate(chol=as.numeric(chol),dbp=as.numeric(dbp)) %>%
  filter(!is.na(chol))

# Check for missing values and remove them
coronary <- na.omit(coronary[, c("chol", "dbp")])

# # 创建 BUGS 模型
model_string <- "
model {
    # Likelihood
    for (i in 1:N) {
       chol[i] ~ dnorm(mu[i], tau)
       mu[i] <- alpha + beta * dbp[i]
    }

    # Priors
    alpha ~ dnorm(0, 0.001)      # Prior for intercept
    beta ~ dnorm(0, 0.001)       # Prior for slope
    tau ~ dgamma(0.001, 0.001)   # Prior for precision

    # Calculate standard deviation
    sigma <- 1/sqrt(tau)
}
"

# Write model to a temporary file
model_file <- tempfile(fileext = ".txt")
writeLines(model_string, model_file)

# Prepare BUGS data list
bugs_data <- list(
  N = nrow(coronary),
  chol = coronary$chol,
  dbp = coronary$dbp
)

# Initial values (more stable tau initialization)
inits <- function() {
  list(
    alpha = rnorm(1, mean = 0, sd = 1),
    beta = rnorm(1, mean = 0, sd = 1),
    tau = rgamma(1, shape = 2, rate = var(coronary$chol)) # Based on data variance
  )
}

# Parameters to monitor
parameters <- c("alpha", "beta", "sigma")

# Run BUGS model
results <- bugs(
  data = bugs_data,
  inits = inits,
  parameters.to.save = parameters,
  model.file = model_file,
  n.chains = 3,
  n.iter = 10000,
  n.burnin = 2000,
  n.thin = 2, # Increased thinning to reduce autocorrelation
)

# Print results and check convergence
print(results)
print(results$summary[, "Rhat"]) # Check Gelman-Rubin statistic for convergence

# Plot results
plot(results)



# BUGS模型定义
model_string <- "
model {
    # 似然函数
    for (i in 1:N) {
        resp[i] ~ dbern(p[i])
        logit(p[i]) <- alpha + beta_age * age[i] + beta_smoke * smoke[i] + u[id[i]]
    }

    # 随机效应
    for (j in 1:n_id) {
        u[j] ~ dnorm(0, tau_u)
    }

    # 先验分布
    alpha ~ dnorm(0, 0.001)          # 截距先验
    beta_age ~ dnorm(0, 0.001)       # 年龄效应先验
    beta_smoke ~ dnorm(0, 0.001)     # 吸烟效应先验
    tau_u ~ dgamma(0.001, 0.001)     # 随机效应精度先验

    # 计算随机效应标准差
    sigma_u <- 1/sqrt(tau_u)
}
"

# 将模型写入临时文件
model_file <- tempfile(fileext = ".txt")
writeLines(model_string, model_file)

# 定义初始值以改善收敛
inits <- function() {
  list(
    alpha = rnorm(1, 0, 1),
    beta_age = rnorm(1, 0, 1),
    beta_smoke = rnorm(1, 0, 1),
    tau_u = rgamma(1, 1, 1),
    u = rnorm(data_list$n_id, 0, 1)
  )
}

# 运行OpenBUGS
ohio_bugs <- bugs(
  data = data_list,
  inits = inits,
  model.file = model_file,  # 使用临时文件路径
  parameters.to.save = c("alpha", "beta_age", "beta_smoke", "sigma_u", "u"),
  n.chains = 3,
  n.iter = 10000,
  n.burnin = 2000,
  n.thin = 2
)

# 查看结果
print(ohio_bugs)


# 诊断
plot(ohio_bugs)
