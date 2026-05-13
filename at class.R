
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
