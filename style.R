library(tidyverse)
data(mpg)

# 逗号后面空格，逗号前不空格(与英文句子中逗号的使用一致)

# good
mpg[, 1]

hist(mpg$hwy, breaks = 20, col = "skyblue", 
     main = "Histogram of Highway Mileage", 
     xlab = "Miles per Gallon")

# bad
mpg[ ,1]

hist(mpg$hwy ,breaks = 20 ,col = "skyblue", 
     main = "Histogram of Highway Mileage", 
     xlab = "Miles per Gallon")

# =、==、+、-、<-, ~, %>%，｜等用空格包围

# good
model1 <- lm(cty ~ displ + cyl, data = mpg)
summary(model1)

mpg %>% lm(cty ~ displ + cyl, data = .) %>% summary()

# bad
model1<-lm(cty ~ displ+cyl,data=mpg)
summary(model1)
mpg%>%lm(cty~displ+cyl,data=.)%>%summary()

# 具有高优先级的运算符包括：::、:::、$、@、[、[[、^，不用空格包围

# good
mpg$cty
mpg[, 1]
psych::describe(mpg)

# bad
mpg $ cty
mpg [ , 1]
psych :: describe(mpg)

# 赋值符用<-,  不用=。=只用于函数参数的赋值

# good
x <- 5

# bad
x = 5

# 一行代码不要太长
# 每一个参数项单独一行
# 最后一行，单独的一个右括号）

# good
mpg %>%
  ggplot(aes(x = displ, y = cty, color = drv)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Weight and MPG",
       x = "Displacement (in cubic inches)",
       y = "City Miles per Gallon"
  ) +
  theme_minimal()

# bad
mpg %>% ggplot(aes(x = displ, y = cty, color = drv)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(title = "Scatterplot of Weight and MPG", x = "Weight (in 1000 lbs)", y = "Miles per Gallon" ) + theme_minimal()

# 函数名单独一行
# 换行保持缩进
# good
mpg %>% 
  filter(class == "suv" | class == "compact") %>%
  ggplot(aes(x = displ, y = cty, col = class)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Displacement and City MPG",
       x = "Displacement in Litres",
       y = "City Miles per Gallon"
  ) +
  theme_minimal()

# bad
mpg %>% 
  filter(class == "suv" | class == "compact") %>%
  ggplot(aes(x = displ, y = cty, col = class)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Displacement and City MPG",
       x = "Displacement in Litres",
       y = "City Miles per Gallon") +
  theme_minimal()

# styler::style_text()函数可以自动调整代码格式

install.packages("styler")
library(styler)

"lm(cty~displ+cyl,data=mpg)"%>% 
  style_text()

lm(cty ~ displ + cyl, data = mpg)

"mpg%>%ggplot(aes(x=displ,y=cty,col=class))+geom_point()+geom_smooth(method='lm',se=FALSE)" %>% 
  style_text()

mpg %>% ggplot(aes(x = displ, y = cty, col = class)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

"mpg %>% 
filter(class=='suv'|class=='compact') %>%
ggplot(aes(x=displ, y=cty, col=class))+
geom_point()+
geom_smooth(method='lm', se=FALSE)+
labs(title='Scatterplot of Displacement and City MPG',
x='Displacement in Litres',
y='City Miles per Gallon')+
theme_minimal()" %>% 
  style_text()


mpg %>%
  filter(class == "suv" | class == "compact") %>%
  ggplot(aes(x = displ, y = cty, col = class)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Scatterplot of Displacement and City MPG",
    x = "Displacement in Litres",
    y = "City Miles per Gallon"
  ) +
  theme_minimal()






