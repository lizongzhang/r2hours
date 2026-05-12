

# L3 ----------------------------------------------------------------------

## L3.1 ----------------------------------------------------------------------


# 四则运算
8*9

## L3.2 ----------------------------------------------------------------------




# L4 ----------------------------------------------------------------------

data("mtcars")

summary(mtcars)

# <- alt + -; option + -
mpg_mean <- mean(mtcars$mpg)

View(mtcars)

# data frame
plot(mtcars)

plot(mtcars$wt, mtcars$mpg,
     col = "blue",
     cex = 1.5,
     main = "weight & MPG",
     xlab = "weight",
     ylab = "Miles per Gallon")

plot(as.factor(mtcars$cyl), mtcars$mpg,
     col = c("red","blue", "green"))

plot(as.factor(mtcars$cyl))



