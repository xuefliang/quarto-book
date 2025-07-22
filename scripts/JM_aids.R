data('aids.id',package = 'JM')
data('aids',package = 'JM')

library(JM)
lmeFit <- lme(CD4 ~ obstime + obstime:drug, data = aids,
      random = ~ obstime | patient) 


# Cox survival model fit
survFit <- coxph(Surv(Time, death) ~ drug, data = aids.id, x = TRUE)

# Joint model,timeVar 需指向 lmeFit 模型中的时间变量。
jointFit <- jointModel(lmeFit, survFit, timeVar = "obstime",
                      method = "piecewise-PH-aGH")



#############
library(INLA)

# 准备数据：确保 aids 数据包含纵向数据，aids.id 包含生存数据
# 纵向模型（CD4 ~ obstime + obstime:drug，随机效应为 obstime | patient）
# 生存模型（Surv(Time, death) ~ drug）

# 准备数据
data_joint <- aids
data_joint$surv_time <- NA
data_joint$event <- NA
data_joint[match(aids.id$patient, data_joint$patient), "surv_time"] <- aids.id$Time
data_joint[match(aids.id$patient, data_joint$patient), "event"] <- aids.id$death

# 创建 inla.surv 对象
data_joint$surv_response <- with(data_joint, inla.surv(time = surv_time, event = event))

# 定义纵向和生存公式
long_formula <- CD4 ~ obstime + obstime:drug + f(patient, obstime, model = "rw1")
surv_formula <- surv_response ~ drug

# 联合模型
joint_model <- inla(
  formula = list(long = long_formula, surv = surv_formula),  # 命名列表
  data = list(long = data_joint, surv = data_joint),        # 命名数据
  family = c("gaussian", "coxph"),
  control.family = list(
    list(),  # 纵向模型（默认高斯）
    list(hazard = "piecewise", n.intervals = 10)  # 生存模型（分段常数基线风险）
  ),
  control.compute = list(dic = TRUE, waic = TRUE),
  control.inla = list(int.strategy = "eb")  # 提高数值稳定性
)

# 输出结果
summary(joint_model)