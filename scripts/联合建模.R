## 联合建模
# 加载数据
data(joint, package = "brinla")
longdat <- joint$longitudinal  # 纵向数据
survdat <- joint$survival      # 生存数据
n1 <- nrow(longdat)            # 纵向数据样本数
n2 <- nrow(survdat)            # 生存数据样本数

# 准备响应变量
y.long <- c(longdat$y, rep(NA, n2))  # 纵向响应
y.surv <- inla.surv(             # 生存响应
  time = c(rep(NA, n1), survdat$SURVTIME), 
  event = c(rep(NA, n1), survdat$CENSOR)
)
Yjoint <- list(y.long, y.surv)   # 联合响应变量列表

# 准备固定效应协变量
linear.covariate <- data.frame(
  mu = as.factor(c(rep(1, n1), rep(2, n2))),  # 模型类型标识
  
  # 纵向部分协变量
  l.TIME = c(longdat$TIME, rep(0, n2)),
  l.TIMEDRUG = c(longdat$TIMEDRUG, rep(0, n2)),
  l.SEX = c(longdat$SEX, rep(0, n2)),
  l.PREVOI = c(longdat$PREVOI, rep(0, n2)),
  l.STRATUM = c(longdat$STRATUM, rep(0, n2)),
  
  # 生存部分协变量
  s.DRUG = c(rep(0, n1), survdat$DRUG),
  s.SEX = c(rep(0, n1), survdat$SEX),
  s.PREVOI = c(rep(0, n1), survdat$PREVOI),
  s.STRATUM = c(rep(0, n1), survdat$STRATUM)
)

# 准备随机效应协变量
ntime <- length(unique(longdat$TIME))  # 时间点数量
random.covariate <- list(
  U11 = c(rep(1:n2, each = ntime), rep(NA, n2)),      # 随机截距
  U21 = c(rep(n2+(1:n2), each = ntime), rep(NA, n2)), # 随机斜率
  U12 = c(rep(NA, n1), 1:n2),                         # 生存随机截距
  U22 = c(rep(NA, n1), n2+(1:n2)),                    # 生存随机斜率
  U3 = c(rep(NA, n1), 1:n2)                           # 额外生存随机效应
)

# 合并数据
joint.data <- c(linear.covariate, random.covariate)
joint.data$Y <- Yjoint

# 模型公式
formula <- Y ~ mu + l.TIME + l.TIMEDRUG + l.SEX + l.PREVOI + l.STRATUM + 
           s.DRUG + s.SEX + s.PREVOI + s.STRATUM - 1 + 
           f(U11, model="iid2d", param = c(23,100,100,0), 
             initial = c(-2.7,0.9,-0.22), n=2*n2) + 
           f(U21, l.TIME, copy="U11") + 
           f(U12, copy="U11", fixed = FALSE, param=c(0,0.01), initial = -0.2) + 
           f(U22, copy="U11", fixed = FALSE, param = c(0,0.01), initial = -1.6)

# 拟合联合模型
joint.inla <- inla(formula, 
                  family = c("gaussian", "exponentialsurv"), 
                  data = joint.data, 
                  control.compute = list(dic=TRUE))

# 输出结果
round(joint.inla$summary.fixed, 4)    # 固定效应估计结果
round(joint.inla$summary.hyper, 4)    # 超参数估计结果
