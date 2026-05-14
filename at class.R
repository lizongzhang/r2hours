
?plot()

data(mtcars)

summary()

lm(mpg ~ wt, data = mtcars)

hist(mtcars$mpg,
     freq = FALSE, 
     breaks = seq(10, 40, 10),
     col = "cyan",
     border = "white")

corrplot(cor(mtcars))


mtcars[mtcars$cyl %in% c(4, 6), ]

c <- 10 
x <- c(c, 20, 30) # 虽然能运行，但极易混淆


data(mtcars)

?mtcars

# 手动创建一个数据框
df <- data.frame(
  name    = c("张三", "李四", "王五", "赵六"),
  age     = c(22, 25, 21, 23),
  score   = c(88.5, 92.0, 75.5, 85.0),
  passed  = c(TRUE, TRUE, FALSE, TRUE)
)

str(df)

head(mtcars,2)

names(df)

row.names(df)

row.names(mtcars)

summary(mtcars[, c("mpg", "hp", "wt")])

mtcars$cyl

# <- alt/option + -
mtcars$kpl <-  mtcars$mpg * 0.425

mtcars$kpl <- NULL

names(df)[names(df) == "score"] <- "grade"

mtcars[1, 2]

mtcars[1, ]

mtcars[, 1:2]

mtcars[1:5, "mpg"]

mtcars[mtcars$cyl %in% c(4, 6), ] |> head(4)



  
  

