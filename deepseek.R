# 加载必要包
library(MASS)    # 有序Logistic回归
library(brant)   # 比例优势假设检验
library(tidyverse)
library(effects) # 可视化效应
library(psych)

# 读取数据
data <- read.csv("康复患者模拟数据.csv", stringsAsFactors = FALSE)

# 变量类型转换
data <- data %>%
  mutate(
    性别 = factor(性别, levels = c("男", "女")),
    治疗方案 = factor(治疗方案),
    功能恢复 = factor(data$功能恢复, 
                        levels = c("差", "一般", "良好"), 
                        ordered = TRUE)
    )

# 查看数据结构
str(data)

describe(data)

# 构建有序Logistic回归模型
model <- polr(
  功能恢复 ~ 年龄 + 性别 + 住院天数 + 治疗方案,
  data = data,
  Hess = TRUE,  # 保留Hessian矩阵以计算标准误
  method = "logistic"
)


# 查看模型摘要
summary_model <- summary(model)
print(summary_model)

# 计算OR值和置信区间
OR <- exp(coef(model))
OR_CI <- exp(confint(model))

# 整理结果表格
results <- data.frame(
  Variable = names(coef(model)),
  OR = round(OR, 2),
  CI_lower = round(OR_CI[,1], 2),
  CI_upper = round(OR_CI[,2], 2),
  p_value = round(summary_model$coefficients[,4], 3)
)
rownames(results) <- NULL

print(results)

# Brant检验验证假设
brant_test <- brant(model)
print(brant_test)

# 可视化检验结果
plot(brant_test)



# deep --------------------------------------------------------------------

# 加载必要包
library(MASS)    # 有序Logistic回归
library(brant)   # 比例优势假设检验
library(effects) # 可视化效应
library(dplyr)   # 数据预处理

# 读取数据
data <- read.csv("康复患者模拟数据.csv", stringsAsFactors = FALSE)

# 变量类型转换
data <- data %>%
  mutate(
    性别 = factor(性别, levels = c("男", "女")),
    治疗方案 = factor(治疗方案, levels = c("A", "B", "C")),,
    功能恢复 = ordered(功能恢复, levels = c("差", "一般", "良好"))
  )

# ----------------------
# 1. 加载必要包
# ----------------------
library(psych)    # 描述统计
library(MASS)     # 有序Logistic回归 (polr函数)
library(lmtest)   # 系数检验 (coeftest)
library(brant)    # Brant检验（比例优势假设）
library(pscl)     # 计算伪R²

# ----------------------
# 2. 数据读取与预处理
# ----------------------
# 读取CSV文件（注意文件路径需与实际情况一致）
data <- read.csv("康复患者模拟数据.csv")

# 将变量转换为合适的类型
data <- data %>%
  mutate(
    性别 = factor(性别, levels = c("男", "女")),
    治疗方案 = factor(治疗方案, levels = c("C", "B", "A")),,
    功能恢复 = ordered(功能恢复, levels = c("差", "一般", "良好"))
  )

# ----------------------
# 3. 描述统计分析
# ----------------------
# 对年龄和住院天数进行描述统计
desc_stats <- describe(data[, c("年龄", "住院天数")], trim = 0.1)
print(desc_stats)

# ----------------------
# 4. 有序Logistic回归建模
# ----------------------
# 拟合模型（因变量：功能恢复；自变量：年龄、住院天数、性别、治疗方案）
model <- polr(
  功能恢复 ~ 年龄 + 住院天数 + 性别 + 治疗方案,
  data = data,
  Hess = TRUE  # 保留Hessian矩阵以计算p值
)

# 查看模型摘要
summary_model <- summary(model)
print(summary_model)

# ----------------------
# 5. 回归结果检验
# ----------------------
# 计算系数p值
coeftest_result <- coeftest(model)
print(coeftest_result)

# Brant检验（验证比例优势假设）
brant_test <- brant(model)
print(brant_test)




# 正确获取解释变量（排除 cutpoints，如 "1|2", "2|3"）
predictor_names <- names(coef(model))[!grepl("\\|", names(coef(model)))]

# 分别提取系数、OR、CI、t值（都基于 predictor_names）
Coef <- coef(model)[predictor_names]
OR <- exp(Coef)

# confint.default 会返回所有参数（含 cutpoints），我们只提取解释变量部分
CI_raw <- confint.default(model)
CI_exp <- exp(CI_raw[predictor_names, ])

# t 值 & p 值提取
ctable <- coef(summary(model))[predictor_names, , drop = FALSE]
pvals <- round(2 * (1 - pnorm(abs(ctable[, "t value"]))), 4)

# ✅ 构建数据框
result <- data.frame(
  Variable = predictor_names,
  Coef = round(Coef, 3),
  OR = round(OR, 3),
  CI.lower = round(CI_exp[, 1], 3),
  CI.upper = round(CI_exp[, 2], 3),
  p.value = pvals
)

print(result)

