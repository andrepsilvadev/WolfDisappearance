

model1 <- glm(cbind(sum_fate,n_territories-sum_fate) ~  sum_AttackDogs_n + sum_wolfLKill_n + n_territories, family = binomial, data = df.municipality_scaled)
summary(model1)
confint(model1)
with(summary(model1), 1 - deviance/null.deviance)

https://stats.stackexchange.com/questions/89734/glm-for-proportion-data-in-r

