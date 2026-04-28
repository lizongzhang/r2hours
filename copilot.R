library(tidyverse)

data(mpg)

mpg %>% 
  ggplot(aes(x = displ, y = hwy, 
             color = drv, size = displ)) + #aes() maps the variables to the axes
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Engine Displacement vs. Highway MPG",
    x = "Engine Displacement (L)",
    y = "Highway MPG"
  ) 

#利用mpg, 进行回归分析
# 1. 线性回归
lm_model <- lm(hwy ~ displ + drv, data = mpg)
summary(lm_model)

# 2. 逻辑回归
log_model <- glm(drv ~ displ + hwy, data = mpg, family = binomial)
summary(log_model)

# 3. 多项式回归
poly_model <- lm(hwy ~ poly(displ, 2) + drv, data = mpg)
summary(poly_model)

# 4. 岭回归
library(MASS)
ridge_model <- lm.ridge(hwy ~ displ + drv, data = mpg, lambda = seq(0, 10, by = 0.1))
plot(ridge_model)

# 5. Lasso回归
library(glmnet)
x <- model.matrix(hwy ~ displ + drv, data = mpg)[, -1]
y <- mpg$hwy
lasso_model <- glmnet(x, y, alpha = 1)


mpg %>%
  ggplot(aes(x = displ, y = hwy, 
             color = drv, size = displ)) + #aes() maps the variables to the axes
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Engine Displacement vs. Highway MPG",
    x = "Engine Displacement (L)",
    y = "Highway MPG"
  )

#利用mpg绘制分组箱线图

mpg %>%
  ggplot(aes(x = drv, y = hwy, fill = drv)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Highway MPG by Drive Type",
    x = "Drive Type",
    y = "Highway MPG"
  ) +
  theme_minimal()

#利用mpg绘制分组小提琴图

mpg %>%
  ggplot(aes(x = drv, y = hwy, fill = drv)) +
  geom_violin() +
  labs(
    title = "Violin Plot of Highway MPG by Drive Type",
    x = "Drive Type",
    y = "Highway MPG"
  ) +
  theme_minimal()
#利用mpg绘制分组散点图
mpg %>%
  ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  labs(
    title = "Scatter Plot of Engine Displacement vs. Highway MPG by Drive Type",
    x = "Engine Displacement (L)",
    y = "Highway MPG"
  ) +
  theme_minimal()

library(tidyverse)
data(mpg)

mpg %>% 
  ggplot(aes(x = displ, y = hwy, 
             color = drv, size = displ)) + #aes() maps the variables to the axes
  geom_point() + # 散点图
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Engine Displacement vs. Highway MPG",
    x = "Engine Displacement (L)",
    y = "Highway MPG"
  )
  

# 绘制分组箱线图

mpg %>%
  ggplot(aes(x = drv, y = hwy, fill = drv)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Highway MPG by Drive Type",
    x = "Drive Type",
    y = "Highway MPG"
  ) +
  theme_minimal()

# 绘制分组小提琴图
mpg %>%
  ggplot(aes(x = drv, y = hwy, fill = drv)) +
  geom_violin() +
  labs(
    title = "Violin Plot of Highway MPG by Drive Type",
    x = "Drive Type",
    y = "Highway MPG"
  ) +
  theme_minimal()

#用mpg,做回归分析

# 1. 线性回归
lm_model <- lm(hwy ~ displ + drv, data = mpg)
summary(lm_model)

# 2. 逻辑回归
log_model <- glm(drv ~ displ + hwy, data = mpg, family = binomial) #family = binomial表示逻辑回归

summary(log_model)

# lm() 函数的使用
# 线性回归模型

lm(cty ~ displ + drv + hwy, data = mpg)

# 3. 多项式回归
poly_model <- lm(hwy ~ poly(displ, 2) + drv, data = mpg)

# 4. 岭回归
library(MASS)
ridge_model <- lm.ridge(hwy ~ displ + drv, data = mpg, lambda = seq(0, 10, by = 0.1))

plot(ridge_model)

# 5. Lasso回归
library(glmnet)
x <- model.matrix(hwy ~ displ + drv, data = mpg)[, -1]









