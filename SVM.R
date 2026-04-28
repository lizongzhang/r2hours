
# iris SVM ----------------------------------------------------------------

# 1. 加载必要包
library(e1071)    # SVM实现
library(caret)    # 数据分割与模型评估
library(ggplot2)  # 可视化

# 2. 数据准备
data(iris)        # 加载数据集
set.seed(123)     # 设定随机种子

# 查看数据结构
head(iris)
summary(iris)

# 划分训练集（70%）和测试集（30%）
train_index <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]

# 3. 训练SVM模型
svm_linear <- svm(Species ~ ., 
                  data = train_data, 
                  kernel = "linear", 
                  cost = 1)
summary(svm_linear)

# 4. 模型评估
pred_linear <- predict(svm_linear, test_data)

# 计算准确率
cat("线性核准确率:\n")
print(confusionMatrix(pred_linear, test_data$Species))



# 生成模拟数据 ------------------------------------------------------------------

# 设置随机种子
set.seed(42)

# 创建 500 个糖尿病患者的模拟数据
n <- 500

# 模拟数据：使用 replicate 简化生成过程
diabetes <- data.frame(
  age = sample(18:80, n, replace = TRUE),
  bmi = runif(n, 18.5, 40),
  glucose = runif(n, 70, 200),
  insulin = runif(n, 5, 30),
  waist = runif(n, 70, 130),
  exercise = sample(c(0, 1), n, replace = TRUE),
  diet = sample(c(0, 1), n, replace = TRUE),
  alcohol = sample(c(0, 1), n, replace = TRUE),
  smoking = sample(c(0, 1), n, replace = TRUE),
  type = sample(c("Type1", "Type2", "LADA"), n, replace = TRUE)
)

# 查看数据的前几行
head(diabetes)

# 复制iris的svm代码，查找替换iris, Species --------------------------------------------------------------------



# 划分训练集（70%）和测试集（30%）
train_index <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]

# 3. 训练SVM模型
svm_linear <- svm(Species ~ ., 
                  data = train_data, 
                  kernel = "linear", 
                  cost = 1)
summary(svm_linear)



# 4. 模型评估
pred_linear <- predict(svm_linear, test_data)

# 计算准确率
cat("线性核准确率:\n")
print(confusionMatrix(pred_linear, test_data$Species))









# diabetes svm ------------------------------------------------------------






# 糖尿病的SVM -----------------------------------------------------------------

# 确保 DiabetesType 是因子变量

diabetes$type <- factor(diabetes$type)


# 划分训练集（70%）和测试集（30%）
train_index <- createDataPartition(diabetes$type, p = 0.7, list = FALSE)
train_data <- diabetes[train_index, ]
test_data <- diabetes[-train_index, ]

# 3. 训练SVM模型
svm_linear <- svm(type ~ ., 
                  data = train_data, 
                  kernel = "linear", 
                  cost = 1)
summary(svm_linear)



# 4. 模型评估
pred_linear <- predict(svm_linear, test_data)

# 计算准确率
cat("线性核准确率:\n")
print(confusionMatrix(pred_linear, test_data$type))



