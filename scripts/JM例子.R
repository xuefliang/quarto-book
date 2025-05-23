library(JM)
# Animation example 
# Mixed-effects model fit
lmeFit.p1 <- lme(log(pro) ~ time + time:treat, data = prothro,
    random = ~ time | id)  

# Cox survival model fit
survFit.p1 <- coxph(Surv(Time, death) ~ treat, data = prothros, x = TRUE)  

# Joint model
jointFit.p1 <- jointModel(lmeFit.p1, survFit.p1, timeVar = "time",
    method = "piecewise-PH-aGH")

summary(jointFit.p1)

# We are interested in producing predictions of survival probabilities for Patient 155
dataP155 <- prothro[prothro$id == 155, ]
len_id <- nrow(dataP155)

# We can plot the data
sfit3 <- survfitJM(jointFit.p1, newdata = dataP155[1:3, ]) 
sfit4 <- survfitJM(jointFit.p1, newdata = dataP155[1:4, ]) 

par(mfrow=c(1,2))
plotfit3 <- plot(sfit3, estimator="mean", include.y = TRUE, conf.int=0.95, fill.area=TRUE, col.area="lightblue", main="Patient 155")
plotfit4 <- plot(sfit4, estimator="mean", include.y = TRUE, conf.int=0.95, fill.area=TRUE, col.area="lightblue", main="Patient 155")

data(prothro,package = 'JM')

data(prothros,package = 'JM')
str(prothro)


data('prothros',package = 'JM')
data('prothro',package = 'JM')

joint_fit <- fit_joint_inla(
  long_data = prothro,
  surv_data = prothros,
  long_formula = log(pro) ~ time + time:treat,
  surv_formula = Surv(Time, death) ~ treat,
  id_var = "id",
  time_var = "time",
  event_var = "death"
)
print_joint_summary(joint_fit)
