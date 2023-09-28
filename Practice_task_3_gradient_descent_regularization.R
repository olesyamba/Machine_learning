#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("glmnet")
library(ggplot2)
library(tidyr)
library(dplyr)
library(glmnet)

################################################################################
###### 1. Gradient Descent: basic idea #########################################
################################################################################

# fix random number generator state for results replication
set.seed(2)

# generate ordered vector: X
X = sort(runif(min=0, max=6, n=500))

# generate actual y
y_true = 50 + 10*X

# generate y observable for researcher: y_obs = actual y + epsilon, 
# let's say epsilon~N(0, 10^2)
epsilon = rnorm(n=500, mean=0, sd=10)
y_obs = y_true + epsilon

# initiate parameters
beta_0 = 0
beta_1 = 0
# define learning rate
learning_rate = 0.00001

plot(X, y_obs, pch=16, ylim=c(0, 120))
abline(beta_0, beta_1, col='red', lwd=3)

# for example: 500 iterations
for (i in seq(500)){
  # calculate the derivative
  grad_0 = sum(-2*(y_obs - beta_0 - beta_1 * X))
  grad_1 = sum(-2*X*(y_obs - beta_0 - beta_1 * X))
  # update estimates
  beta_0 = beta_0 - learning_rate * grad_0
  beta_1 = beta_1 - learning_rate * grad_1
  # print results each 100 iterations
  if (i %% 100 == 0){
    cat(round(beta_0, 3), round(beta_1, 3), '\n')
    plot(X, y_obs, pch=16, ylim=c(0, 120))
    abline(beta_0, beta_1, col='red', lwd=3)
    Sys.sleep(1)
  }
}

#функция ниже рассчитывает методом наименьших квадратов и должна получать тоже 50 и 10
round(lm(y_obs ~ X)$coefficients, 3)
# когда мы поправим количество итераций и лернинг рэйт, у нас коэфы 
# должны получиться как в трушном уравнении 50 и 10

# Когда нам перестать обучать модель? когда функция потерь начнет уменьшаться 
# очень медленно, порог, который мы задаем, где остановиться и есть трешхолдер


#TASK 3#########################################################################
'''3. Использовать код из блока 1. Gradient Descent: basic idea. Убедиться, 
что beta0=beta1=0. Подобрать значение learning rate, при котором после 100 
итераций оценка константы (beta0) будет отличаться от МНК-оценки, равной 51.403,
не более, чем на 1. Кратко объяснить, как мы подобрали данное значение.'''

# initiate parameters
beta_0 = 0
beta_1 = 0
# define learning rate
learning_rate = 0.00001

#функция ниже рассчитывает методом наименьших квадратов и должна получать тоже 50 и 10
lin_mod = round(lm(y_obs ~ X)$coefficients, 3)
lin_mod_beta0 = lin_mod[1]
lin_mod_beta1 = lin_mod[2]

#y = 50 + 10x
# 500 iterations? 1000? 10000? 100000?

plot(X, y_obs, pch=16, ylim=c(0, 120))
abline(beta_0, beta_1, col='red', lwd=3)
  
for (learning_rate in seq(0.00001, 1, 0.000001)) {
# for example: 100 iterations
  for (i in seq(100)){
    # calculate the derivative Считаем градиенты
    grad_0 = sum(-2*(y_obs - beta_0 - beta_1 * X)) # функция градиента для конкретной функции, рассчитывается заранее и отдельно
    grad_1 = sum(-2*X*(y_obs - beta_0 - beta_1 * X))
    # update estimates чем больше Лернинг рэйт, тем большими шагами мы будем двигаться в обратную градиенту стороны, слишком большой градиент приведет к тому, что мы будем бегать мимо оптимума просто
    beta_0 = beta_0 - learning_rate * grad_0
    beta_1 = beta_1 - learning_rate * grad_1
    # print results each 100 iterations
    if (i %% 100 == 0){
      cat(round(beta_0, 3), round(beta_1, 3), '\n')
      plot(X, y_obs, pch=16, ylim=c(0, 120))
      abline(beta_0, beta_1, col='red', lwd=3)
      Sys.sleep(1)
    }
  }
  if (abs(lin_mod_beta0 - beta_0) < 1) {
    break
  }
  # initiate parameters
  beta_0 = 0
  beta_1 = 0
}
print(learning_rate)
print(beta_0)
print(beta_1)
'Для подбора learning rate мы сначала оценили в какую сторону будут меняться 
значения beta0 и beta1 при увеличении и уменьшении изначально заданного learning
rate = 0.00001. Мы поняли, что при его небольшом увеличении значения beta0 
постепенно растут, а значения beta1 постепенно уменьшаются. Тогда мы сделали 
внешний цикл, который изменял параметр learning rate на 0.000001 при каждом 
прохождении цикла. В цикле мы обозначили условие, при котором он должен будет
остановиться: когда разница между МНК-оценкой и beta0 станет меньше единицы. 
Таким образом, значение learning rate, при котором данное условие выполнилось,
составило 0.000148, а значения beta0 50.41805. (Интересный факт: мы душнилы и
при ручном переборе мы дошли до еще более близкого значения beta0 при learning
rate =0.000153)'

#TASK 4#########################################################################
#Использовать код из блока 1. Gradient Descent: basic idea. Убедиться, 
# что изначально beta0=beta1=0. Подобрать learning rate, при котором после 
# 1000 итераций оценка константы (beta0) будет меньше единицы по модулю. 
# Кратко объяснить, как мы подобрали данное значение.

# initiate parameters
beta_0 = 0
beta_1 = 0

plot(X, y_obs, pch=16, ylim=c(0, 120))
abline(beta_0, beta_1, col='red', lwd=3)

for (learning_rate in seq(0.00001, 0, -0.000001)) {
  # for example: 1000 iterations
  for (i in seq(1000)){
    # calculate the derivative Считаем градиенты
    grad_0 = sum(-2*(y_obs - beta_0 - beta_1 * X)) # функция градиента для конкретной функции, рассчитывается заранее и отдельно
    grad_1 = sum(-2*X*(y_obs - beta_0 - beta_1 * X))
    # update estimates чем больше Лернинг рэйт, тем большими шагами мы будем двигаться в обратную градиенту стороны, слишком большой градиент приведет к тому, что мы будем бегать мимо оптимума просто
    beta_0 = beta_0 - learning_rate * grad_0
    beta_1 = beta_1 - learning_rate * grad_1
    # print results each 100 iterations
    if (i %% 1000 == 0){
      cat(round(beta_0, 3), round(beta_1, 3), '\n')
      plot(X, y_obs, pch=16, ylim=c(0, 120))
      abline(beta_0, beta_1, col='red', lwd=3)
      Sys.sleep(1)
    }
  }
  if (abs(beta_0) < 1) {
    break
  }
  # initiate parameters
  beta_0 = 0
  beta_1 = 0
}
print(learning_rate)
print(beta_0)
print(beta_1)

# Check, that after 1000 iterations abs(beta0) will be less than 1 
beta_0 = 0
beta_1 = 0

for (i in seq(1000)){
  # calculate the derivative Считаем градиенты
  grad_0 = sum(-2*(y_obs - beta_0 - beta_1 * X)) # функция градиента для конкретной функции, рассчитывается заранее и отдельно
  grad_1 = sum(-2*X*(y_obs - beta_0 - beta_1 * X))
  # update estimates чем больше Лернинг рэйт, тем большими шагами мы будем двигаться в обратную градиенту стороны, слишком большой градиент приведет к тому, что мы будем бегать мимо оптимума просто
  beta_0 = beta_0 - learning_rate * grad_0
  beta_1 = beta_1 - learning_rate * grad_1
  # print results each 100 iterations
  if (i %% 100 == 0){
    cat(round(beta_0, 30), round(beta_1, 30), '\n')
    plot(X, y_obs, pch=16, ylim=c(0, 120))
    abline(beta_0, beta_1, col='red', lwd=3)
    Sys.sleep(1)
  }
}
'Для подбора такого learning rate, чтобы после 1000 итераций beta0 по модулю 
было меньше 1, мы изменили цикл, написанный для 3 задания, и вместо увеличения 
уменьшали learning rate после каждой итерации на 0. 000001. Иными словами, наш
цикл подбирал такое значение параметра, начиная с которого, оценка beta0, как 
и прежде, увеличивалась при течении итераций, но даже после 1000 повторов она
оставалась меньше 1. Соответствующее значение параметра learning rate =
1.694066e-21. Фактически, рациональнее было бы изменить направление относительно
градиента и двигаться не против него, а сонаправленно с ним, но в формулировке
задания сказано, что мы можем менять только значение самого параметра, поэтому
мы не стали пробовать эту методику и менять функцию в теле цикла.'
################################################################################
###### 2. Gradient Descent: threshold improvement ##############################
################################################################################

# initiate parameters
beta_0 = 0
beta_1 = 0
# define learning rate
learning_rate = 10^(-4)
# define threshold - tolerance
tol = 10^(-6) # подбирается содержательно (какая точность нас устраивает, сколько знаков после запятой)
# add initial value to start loop
improve = 1
mse = 10^9
iteration = 0

# цикл с уловием, который каждый раз проверяет "мы улучшились на 1 копейку, 
# одна копейка больше 1 тысячной копейки? продолжаем"
# когда импрув будет слишком маленький (меньше задаваемого тула), цикл остановится 
while (abs(improve) > tol){
  iteration = iteration + 1
  # calculate the derivative
  grad_0 = sum(-2*(y_obs - beta_0 - beta_1 * X))
  grad_1 = sum(-2*X*(y_obs - beta_0 - beta_1 * X))
  # update coefficients
  beta_0 = beta_0 - learning_rate * grad_0
  beta_1 = beta_1 - learning_rate * grad_1
  # calculate current mse
  mse_current = mean((y_obs - beta_0 - beta_1 * X)^2)
  improve = - (mse_current - mse)
  mse = mse_current
}

print(paste(iteration, round(mse, 3), round(beta_0, 3), round(beta_1, 3)))
round(lm(y_obs~X)$coefficients, 3)
# Why there is a difference?

################################################################################
###### 3. Gradient Descent: regularization #####################################
################################################################################
# define learning rate
learning_rate = 10^(-4)
# define threshold - tolerance
tol = 10^(-6)
# regularization coefficient
lambda = 100

# scaling is important!

# initiate parameters
beta_0 = 0
beta_1 = 0

# add initial value to start loop
improve = 1
mse = 10^9
iteration = 0

# В цикле все тоже самое, но добавилось слагаемое лямбда на сигнум = как раз регуляризация
# мы можем не заметить изменений, если лямбда будет равна нулю или будет очень маленькой
while (abs(improve) > tol){
  iteration = iteration + 1
  # calculate the derivative (with regularization)
  grad_0 = sum(-2*(y_obs - beta_0 - beta_1 * X)) # attention: no penalty for constant
  grad_1 = sum(-2*X*(y_obs - beta_0 - beta_1 * X)) + lambda * sign(beta_1)
  # update coefficients
  beta_0 = beta_0 - learning_rate * grad_0
  beta_1 = beta_1 - learning_rate * grad_1
  # calculate current mse
  mse_current = mean((y_obs - beta_0 - beta_1 * X)^2)
  improve = - (mse_current - mse)
  mse = mse_current
}

print(paste(iteration, round(mse, 3), round(beta_0, 3), round(beta_1, 3)))
round(lm(y_obs ~ X)$coefficients, 3)

# Why there is a difference?
# разное количество итераций и одинаковые коэфы = где больше итераций, та модель лучше

#TASK 5#########################################################################
'5. Использовать код из блока 3. Gradient Descent: regularization. Доработать 
код так, чтобы он оценивал линейную регрессию, имеющую истинное уравнение 
y = 50 + 10x + 2x2 + ε, минимизируя сумму квадратов остатков с использованием 
ridge регуляризации. Подобрать начальные параметры так, чтобы из начального 
значения beta0=beta1=beta2=0 через 1000 итераций достигались оценки 
коэффициентов, отличающихся от истинных значений по модулю не больше, чем на 
единицу. В качестве ответа вставить доработанные строки кода в наш pdf файл. 
Проверить, что эти строки работают.'

# fix random number generator state for results replication
set.seed(2)

# generate ordered vector: X
X = sort(runif(min=0, max=6, n=500))

# generate actual y
y_true = 50 + 10*X + 2*X^2

# generate y observable for researcher: y_obs = actual y + epsilon, 
# let's say epsilon~N(0, 10^2)
epsilon = rnorm(n=500, mean=0, sd=10)
y_obs = y_true + epsilon


df = data.frame(cbind(c(y_true), c(y_obs), c(X)))
# define learning rate
learning_rate = 0.00000708

#make empty dataframe, in which results will be saved
lambdas = data.frame()

# initiate parameters
beta_0 = 0
beta_1 = 0
beta_2 = 0
best_beta_0 = 10^9
best_beta_1 = 10^9
best_beta_2 = 10^9
iteraction = 0

y = beta_0 + beta_1*X + beta_2*X^2
df = cbind(df, y)

for (lambda in seq(56, 57, 0.05)) {
  beta_0 = 0
  beta_1 = 0
  beta_2 = 0
  for (i in seq(1000)){
    iteraction = iteraction+1
    # calculate the derivative (with regularization)
    grad_0 = sum(-2*(y_obs - beta_0 - beta_1 * X - beta_2 * X^2)) # attention: no penalty for constant
    grad_1 = sum(-2*X*(y_obs - beta_0 - beta_1 * X - beta_2 * X^2)) + 2 * lambda * (beta_0 + beta_1 + beta_2)
    grad_2 = sum(-2*X^2*(y_obs - beta_0 - beta_1 * X - beta_2 * X^2)) + 2 * lambda * (beta_0 + beta_1 + beta_2)
    # update coefficients
    beta_0 = beta_0 - learning_rate * grad_0
    beta_1 = beta_1 - learning_rate * grad_1
    beta_2 = beta_2 - learning_rate * grad_2
    if (i %% 1000 == 0){
      cat(round(beta_0, 30), round(beta_1, 30), round(beta_2, 30), '\n')
      y = beta_0 + beta_1*X + beta_2*X^2
      df$y = y
      plot = ggplot(df) +
        geom_line(aes(X, y), col='red') +
        geom_point(aes(X, y_obs))
      print(plot)
      Sys.sleep(1)
    }
  }
  if (abs(50 - beta_0) <= abs(50 - best_beta_0) & abs(10 - beta_1) < 1 & abs(2 - beta_2) < 1){
    best_beta_0 = beta_0
    best_beta_1 = beta_1
    best_beta_2 = beta_2
    best_lr = learning_rate
    best_lambda = lambda
  }
  if (abs(50 - beta_0) < 1 & abs(10 - beta_1) < 1 & abs(2 - beta_2) < 1){
    print(beta_0)
    print(beta_1)
    print(beta_2)
    print(learning_rate)  
    print(lambda)
    break
  }
  # calculate current mse
  mse = mean((y_obs - beta_0 - beta_1 * X)^2)
  # update lambdas dataframe
  lambdas = rbind(lambdas, c(lambda , beta_0, beta_1, beta_2, mse))
}

#update the dataframe with row names
names(lambdas)[1] <- "lambda"
names(lambdas)[2] <- "beta_0"
names(lambdas)[3] <- "beta_1"
names(lambdas)[4] <- "beta_2"
names(lambdas)[5] <- "mse"
'В ходе реализации предложенного анализа мы аналитически описали функцию потерь,
минимизирующую сумму квадратов остатков с использованием ридж регуляризации. 
Далее мы аналитически вычислили градиенты и скорректировали их расчет в коде. 
После этого нами был произведен ручной подбор примерного интервала значений 
learning_rate и Lambda, при которых в результате тысячи итераций оценки бета 
отличались от истинных на минимальную величину. Далее мы доработали код, с 
помощью которого был осуществлен перебор значений lambda из определенного 
вручную интервала, который также визуализировал каждую итерацию и по итогу 
выбирал лучшую лямбду при минимальном отклонении бет. Лучшие результаты оценки 
параметров сохранялись в новые величины с приставкой best, они представлены на
рисунке ниже. Параметр learning_rate был выставлен на таком уровне, что при
уменьшении шага алгоритм уже не успевал приблизиться к необходимым значениям за 
1000 итераций, а увеличение шага приводило к пропуску необходимого значения 
lambda и бет и параметры уходили в бесконечно большие отрицательные величины.
На графике, представленном после кода, заметно, что подобранные нами значения 
параметров близки к идеальным. Однако в результате анализа мы выявили, что 
определить значение параметров, при которых все беты (в нашем случае только 
бета1 и бета 2) отклоняются от истинных значений по модулю меньше, чем на 1, 
- невозможно. Если бы допустимое количество итераций было увеличено или вообще
не ограничено, мы бы уменьшили шаг за счёт добавления внешнего цикла для 
перебора шага и достигли бы поставленной цели.'
################################################################################
###### 3. Regularization: collinearity #########################################
################################################################################
# мы добавили новую переменную, она коррелирует с иксом, поэтому будет
# мультиколлинеарность, оценки будут смещенными, несостоятельными и 
# очень завышенными, поэтому мы используем лассо, чтобы он занулил зэд 
# и у нас остались такие же беты, как в тру 

# Добавляем подбор лямбд, в предыдущем пункте мы просто от балды лямбду 100 взяли

# add insignificant variable
Z = X * (-0.8) + rnorm(n=500, mean=0, sd=2)
cor(X, Z)

lambdas = data.frame()

# define learning rate
learning_rate = 10^(-6)
# define threshold - tolerance
tol = 10^(-4)
# regularization coefficient
lambda = 100

for (lambda in 10^c(-10, -5, 0, 1, 2, 3, 4)){
  beta_0 = 0
  beta_1 = 0
  beta_2 = 0
  improve = 1
  mse = 10^9
 
  while (abs(improve) > tol){
    # calculate the derivative (with regularization)
    grad_0 = sum(-2*(y_obs - beta_0 - beta_1 * X - beta_2 * Z)) #attention: no penalty for constant
    grad_1 = sum(-2*X*(y_obs - beta_0 - beta_1 * X - beta_2 * Z)) + lambda * sign(beta_1)
    grad_2 = sum(-2*Z*(y_obs - beta_0 - beta_1 * X - beta_2 * Z)) + lambda * sign(beta_2)
    # update coefficients
    beta_0 = beta_0 - learning_rate * grad_0
    beta_1 = beta_1 - learning_rate * grad_1
    beta_2 = beta_2 - learning_rate * grad_2
    # calculate current mse
    mse_current = mean((y_obs - beta_0 - beta_1 * X - beta_2 * Z)^2)
    improve = - (mse_current - mse)
    mse = mse_current
  }
  lambdas = rbind(lambdas, c(lambda, mse, beta_0, beta_1, beta_2))
}

colnames(lambdas) = c('lambda', 'mse', 'intercept', 'X', 'Z')
round(lm(y_obs ~ X + Z)$coefficients, 3)
round(lm(y_obs ~ X)$coefficients, 3)
View(lambdas)
# What is the best lambda?

################################################################################
###### 4. Regularization: collinearity #########################################
################################################################################
#   ВСЕ ТОЖЕ САМОЕ МОЖНО СДЕЛАТЬ ФУНКЦИЕЙ

x = data.matrix(data.frame(X, Z))
cv_lasso = cv.glmnet(x, y_obs, alpha=1)

lambda = cv_lasso$lambda.min
plot(cv_lasso)

best_model = glmnet(x, y_obs, alpha=1, lambda=lambda)
coef(best_model)
# Why is it so?

#TASK 8#########################################################################
'8. Изучить аргументы функции cv.glmnet и записать код для решения следующей 
задачи: построить линейную модель с квадратичной функцией потерь и 
регуляризацией методом эластичной сети, найти оптимальный параметр регуляризации
(лямбда) при помощи 5-fold cv, ориентируясь на минимум mse.'
# fix random number generator state for results replication
set.seed(2)

# generate ordered vector: X
X = sort(runif(min=0, max=6, n=500))

# generate actual y
y_true = 50 + 10*X + 2*X^2

# generate y observable for researcher: y_obs = actual y + epsilon, 
# let's say epsilon~N(0, 10^2)
epsilon = rnorm(n=500, mean=0, sd=10)
y_obs = y_true + epsilon

x = data.matrix(data.frame(X, X^2))
y = data.matrix(y_obs)

cvfit <- cv.glmnet(x, y, type.measure = "mse", nfolds = 5, alpha = 0.5)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")

'За линейную модель с квадратичной функцией потерь отвечает аргумент family, в
нашем случае его необходимо задать, как равный “gaussian”, но для данной функции
это дефолтное значение, поэтому его специфицировать необязательно. Для
добавления регуляризации методом эластичной сети необходимо специфицировать 
параметр alpha,описанный нами в прошлых заданиях. Так как в задании не указано 
точное необходимое значение параметра, мы выставили его равным 0.5, чтобы не 
допустить перекоса в сторону ridge или lasso. Для реализации кросс-валидации с
делением первичной выборки на 5 частей (5-fold cv) мы определили атрибут nfolds
равным 5. Для того чтобы минимизация реализовывалась на основе 
среднеквадратичной ошибки, мы определили атрибут type.measure = “mse”.'
