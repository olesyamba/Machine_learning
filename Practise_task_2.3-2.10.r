# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("dplyr")
library(ggplot2)
library(tidyr)
library(dplyr)
############################## TASK 3,4,5,6 ####################################
'3. Как отобрать переменные для линейной модели на основании результатов 
Lasso регрессии? Кратко описать алгоритм.'

'Цель: отбор важных регрессоров.
1. Для начала необходимо задать выборку данных, на основе которых будет 
построена модель. Определяется зависимая переменная Y и объясняющие
переменные X (обычно при применении этого метода, объясняющих переменных 
достаточно много и может наблюдаться мультиколлинеарность)
2. Далее строится линейная регрессия (𝑌 = 𝑋 * ß + Ɛ), из которой с помощью
метода LASSO получаем оценки коэффициентов ß (добавляем ограничение в задачу,
теперь мы минимизируем не только RSS как в OLS, но и новое слагаемое 
лямбда*(сумма модулей бэт - штраф за количество
переменных, предотвращающий переобучение)).
3. LASSO зануляет незначимые или малозначимые коэффициенты ß, так как
минимизирует сумму модулей оценок коэффициентов (регуляризация
L1), умноженную на параметр регуляризации.
4. Таким образом, с помощью LASSO - регрессии происходит отбор
переменных для линейной модели (выбрасываются те, в которых занулились ß).
В модели остаются только значимые объясняющие переменные и их совокупное 
число уменьшается.'

'4. Как отобрать переменные для линейной модели на основании результатов
Ridge регрессии? Кратко описать алгоритм.'

'Цель: уменьшение влияния неважных регрессоров без потери доли объясненной 
дисперсии или оценка влияния большого количества факторов с небольшим эффектом.
Следует отметить, что в результате корректировки оценок параметров модели при 
использовании гребневой регрессии они никогда не принимают нулевых значений, 
поэтому гребневая регрессия не является методом отбора переменных. С её помощью
производится корректировка оценок параметров регрессионной модели с целью 
повышения её устойчивости, снижающейся из-за корреляции признаков набора 
данных.1
Алгоритм использования Ridge-регрессии аналогичен, за исключением вида
регуляризации.
1. Для начала необходимо задать выборку данных, на основе которых будет 
построена модель. Определяется зависимая переменная Y и объясняющие 
переменные X (обычно при применении этого метода, объясняющих переменных 
достаточно много и может наблюдаться мультиколлинеарность)
2. Далее строится линейная регрессия (𝑌 = 𝑋 * ß + Ɛ), из которой с помощью
метода RIDGE получаем оценки коэффициентов ß (добавляем ограничение в задачу,
теперь мы минимизируем не только RSS как в OLS, но и новое слагаемое 
лямбда*(сумма квадратов бэт - штраф за количество переменных, предотвращающий 
переобучение)).
3. RIDGE уменьшает, но не зануляет, коэффициенты ß (стремятся к нулю) для 
наименее значимых переменных, так как минимизирует сумму квадратов оценок 
коэффициентов (регуляризация L2), умноженную на параметр регуляризации.
4. Таким образом, с помощью RIDGE - регрессии не происходит отбор переменных
для линейной модели, но уменьшается влияние незначимых факторов, повышается 
устойчивость модели.
  1 URL: https://loginom.ru/blog/feature-selection, 19.09.22
 
Гребневая регрессия лучше подходит в ситуации, когда мы хотим сделать 
приоритетными большое количество переменных, каждая из которых имеет 
небольшой эффект. Если в модели требуется учитывать несколько переменных, 
каждая из которых имеет средний или большой эффект, лучшим выбором 
будет Lasso.2'

'5. Найти значение обозначений "L1"и "L2"в функциях потерь, описать его с
использованием формул своими словами. Доказать (или опровергните) 
существование L0.'

'По своей сути L1 и L2 - это две разновидности функции потерь, возникающих
при неправильном принятии решений на основе наблюдаемых данных.
L1 - Функция потерь с абсолютными отклонениями (наименьшая ошибка в абсолютном 
значении). Функция потерь с абсолютными отклонениями (регуляризация L1) 
оценивает абсолютную разницу между истинным значением оцениваемого параметра 
и его оценкой (сумма модулей) (оценка относительно медианы). Этот вариант 
предпочтителен в случае, когда исследовательский вопрос требует, чтобы
относительно оценки 50% наблюдений лежало в левой части распределения и 50% 
в правой.
L2 - Функция потерь с наименьшими квадратами. А функция наименьших квадратов 
(регуляризация L1) оценивает квадратичную разницу между истинным значением
оцениваемого параметра и его оценкой (сумма квадратов) (оценка относительно 
среднего). Этот вариант лучше, если исследуются нормально распределенные
величины и ошибка небольшая.
Что касается L0, она будет равна 1 умножить на количество параметров
Lp=sum(|e|)^p
Самая главная проблема не в том, что она не зависит от ошибки'

'6. Какая модель даёт наилучший прогноз в точке X=4? X = 0?
Можно ли на основании ответов на эти вопросы выбрать в целом модель наилучшего 
качества и почему?'

'Для X = 4 и для X = 0 наилучший прогноз дает полиномиальная модель 3
степени, поскольку значение косинуса при X = 4 равно примерно -0.65, а 
при X = 0 равно 1. В данной модели прогнозы являются ближайшими к действительным
значениям (-0.58 и 1.18 соответственно).
Основываясь только на том, что в точках X=4 и X=0, третья модель дала наилучшие
результаты, некорректно принимать полиномиальную модель третьей степени за 
наилучшую, так как мы не имеем представления об отклонении оценок данной модели 
от истинного значения в остальных точках, а величина X является непрерывной 
(в других дискретных точках отклонение оценки модели 3 может быть больше 
отклонения оценок других моделей). Более корректным было бы сравнение моделей 
с использованием среднеквадратического отклонения или в идеале минимизации 
суммы квадратов остатков. Точность прогноза в конкретной точке не может говорить
о высокой точности модели в целом.'

################################################################################
###### 1. Basic idea ###########################################################
################################################################################

# fix random number generator state for results replication
set.seed(2)

# generate ordered vector: X
X = sort(runif(min=0, max=2*pi, n=50))

# generate actual y: y = cos(x)
y_true = cos(X)

# generate y observable for researcher: y_obs = actual y + epsilon, 
# let's say epsilon~N(0, 0.6^2)
epsilon = rnorm(n=50, mean=0, sd=0.6)
y_obs = y_true + epsilon

# create dataset
df = data.frame(cbind(X, y_obs, y_true))
head(df)

#train model with different polynomial degrees
model_0 = lm(y_obs ~ 1, data=df) # y = a
model_1 = lm(y_obs ~ poly(X, degree=1, raw=TRUE), data=df)# y = a + b*x
model_3 = lm(y_obs ~ poly(X, degree=3, raw=TRUE), data=df)# y = a + b*x + c*x^2 + d*x^3
model_15 = lm(y_obs ~ poly(X, degree=15, raw=TRUE), data=df)# y = a + b*x + ... + n * x^14 + o*x^15

# bring all data together (X, actual y, observable y, and models predictions) together
df1 = data.frame(X, y_true, y_obs,
                  model_0$fitted.values, 
                  model_1$fitted.values, 
                  model_3$fitted.values, 
                  model_15$fitted.values)
           
# change column names
colnames(df1) = c('X', 'true', 'observable', '0 degree', '1 degree', '3 degree', '15 degree')
head(df1)

# transform data from wide to long format for plot
# example: http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/#tidyr
df2 = gather(df1, "degree", "fit", c('0 degree', '1 degree', '3 degree', '15 degree'), factor_key=T)
head(df2)

# look at predictions with different polynomial degrees
# points - actual values of y
# overfitting? underfitting?
ggplot(aes(y=fit, x=X, color=degree), data=df2) + 
  geom_line(size=1.2) + geom_point(aes(y=observable, x=X), color='black') # + theme_void()

# calculate model residuals
df2['residual'] = df2['true'] - df2['fit']

# why it's invalid approach for optimal degree choice?
df2 %>% 
  group_by(degree) %>% 
    summarise(mean=mean(residual), var=var(residual))

# let's get predictions for X = 4
predict(model_0, data.frame(X=4))
predict(model_1, data.frame(X=4))
predict(model_3, data.frame(X=4))
predict(model_15, data.frame(X=4))
# true value = ?
# cos(4) = -0.6536436

######################## Task 7 ################################################

#Оценим модель вида y = a + bx + cx2 и посчитаем сумму квадратов ошибок этой модели
model_2 = lm(y_obs ~ poly(X, degree=2, raw=TRUE), data=df)# y = a + b*x + c*x^2
df3 = data.frame(X, y_true, y_obs,
                 model_2$fitted.values)
colnames(df3) = c('X', 'true', 'observable', 'fit')
head(df3)
df3['residual^2'] = (df3['true'] - df3['fit'])^2  
sum(df3['residual^2'])



################################################################################
###### 2. Bias & variance estimation############################################
################################################################################

'8. Ящичковая диаграмма показывает распределение прогнозных значений моделей,
оценённых на разных входных данных в точке X=4. При какой степени многочлена 
модель имеет наибольший разброс прогнозных значений? Как мы это определили?'

' Основываясь на ящичковой диаграмме, построенной для полиномиальных моделей 
различных степеней, можно сказать что наибольший разброс прогнозных значений в
модели с наибольшей степенью 7. Это можно определить по длине “усов” на 
графике:'

library(ggplot2)
library(tidyr)
library(dplyr)

# data generation process
get_data = function(n=n_obs){
  X = sort(runif(min=X_min, max=X_max, n=n))
  y_true = cos(X)
  epsilon = rnorm(n=n, mean=0, sd=sigma)
  y_obs = y_true + epsilon
  df = data.frame(cbind(X, y_obs, y_true))
  return(df)
}

# function to create plot 
predict_plot_data = function(df, max_deg=max_degree){
  pred_plot = df
  for (degree in seq(from=0, to=max_deg, by=1)){
    if (degree==0){
      model = lm(y_obs ~ 1, data=df)
      pred_plot[toString(degree)] <- model$fitted.values
    }
    else{
      model = lm(y_obs ~ poly(X, degree=degree, raw=TRUE), data=df)
      pred_plot[toString(degree)] <- model$fitted.values
    }
  }
  pred_plot = gather(pred_plot, "degree", "fit", "0":toString(max_deg), factor_key=F)
  pred_plot$degree = as.numeric(pred_plot$degree)
  return(pred_plot)
}


# create function to get predictions in the point
predict_data = function(df, X_pred, max_deg=max_degree){
  predictions = data.frame()
  for (degree in seq(from=0, to=max_deg, by=1)){
    if (degree==0){
      model = lm(y_obs ~ 1, data=df)
      predictions[1, toString(degree)] <- predict(model, data.frame(X=X_pred))
    }
    else{
      model = lm(y_obs ~ poly(X, degree=degree, raw=TRUE), data=df)
      predictions[1, toString(degree)] <- predict(model, data.frame(X=X_pred))
    }
  }
  return(predictions)
}

# input parameters: try to change!
n_obs = 50
X_min = 0
X_max = 2*pi
sigma = 0.8
max_degree = 7

######################## Script for plot replication############################
# generate data
df = get_data()
head(df)

# create plot
df_plot = predict_plot_data(df)
df_plot = df_plot %>% dplyr::filter(degree %in% c(0))
df_plot$degree = as.factor(df_plot$degree)

ggplot(aes(y=fit, x=X, color=degree), data=df_plot) + 
  geom_line(size=1.2) + geom_point(aes(y=y_true, x=X), color='black')


######################## Script for bias&variance estimation####################
X_pred = 4
predictions_all = data.frame()
for (i in seq(1, 100, 1)){
    df_tmp = get_data()
    pred_tmp = predict_data(df_tmp, X_pred)
    predictions_all = rbind(predictions_all, pred_tmp)
}
head(predictions_all)

predictions_long = gather(predictions_all, 'degree', 'estimate')
predictions_long$degree = as.factor(as.numeric(predictions_long$degree))

ggplot(aes(group=degree, x=degree, y=estimate), data=predictions_long) + 
  geom_boxplot() + 
  geom_hline(yintercept=cos(X_pred), color='red', size=1)


# mean squared error
my_mse = function(true, predict){
  mean((predict - true) ^ 2)}
# bias
my_bias = function(predict, true){
  (mean(predict) - true)^2}
# variance
my_var = function(predict){
  mean((predict - mean(predict)) ^ 2)}

mse = apply(X=predictions_all, MARGIN=2, FUN=my_mse, true=cos(X_pred))
bias = apply(X=predictions_all, MARGIN=2, FUN=my_bias, true=cos(X_pred))
variance = apply(X=predictions_all, MARGIN=2, FUN=my_var)

bias_variance = data.frame(cbind(bias, variance))
bias_variance$degree = seq(0, max_degree, 1)

bias_variance = gather(bias_variance, 'metric', 'value', 'bias':'variance')
ggplot(aes(x=degree, color=metric), data=bias_variance) + 
  geom_point(aes(y=value), size=2) + 
    geom_line(aes(y=value), size=1) + 
      scale_x_continuous(breaks=c(seq(0, max_degree, 1)))

######################## Task 9 #############################################

'9. Найти такую точку на исследуемом промежутке X, в которой у модели типа 
y = a будет отсутствовать систематическое смещение прогнозов. Кратко пояснить,
как мы нашли эту точку.'
# Функция для расчета систематического смещения прогноза 
# (мат ожидание от отклонения прогнозного значения от истинного)
my_bias = function(predict, true){
  (mean(true - predict))}

# Создадим пустой фрейм и определим первое значение необходимой переменной j
predictions_all = data.frame()
j = 1

# Создадим пустой массив, который будет заполняться значениями систематических 
# смещений прогноза для каждой из точек последовательности 
bias = array()
x = 0

# Для устранения недостатка создадим цикл в цикле, который позволяет рассчитать 
# систематические смещения прогноза для x, заданного в интервале,
# и при обучении на 100 выборках

for (x in seq(0, 2*pi, 0.001)){
  for (i in seq(1, 100, 1)){
    df_tmp = get_data()
    pred_tmp = predict_data(df_tmp, x, max_deg=0)
    predictions_all = rbind(predictions_all, pred_tmp)
  }
  bias[j] = apply(X=predictions_all, MARGIN=2, FUN=my_bias, true=cos(x))
  j = j+1
  predictions_all = data.frame()
  if (abs(bias[j-1]) <= 0.001) break
}

# Минимальное систематическое смещение прогноза равно:
print(min(abs(bias)))

#При каком X выбранном с точностью до тысячных достигается минимальное
# систематическое смещение прогноза
index = which(abs(bias) == min(abs(bias)))
a = 0+0.001*index
print(a)

'Для нахождения точки, в которой будет отсутствовать систематическое смещение 
прогнозов, мы воспользовались перебором значений X с точностью до тысячных. Мы 
включили в алгоритм обучения модели внешний цикл, который осуществляет перебор 
значений X. Также мы задали массив для хранения значений смещений. Далее, в ходе
цикла мы задали параметр степени полинома как равный нулю и по окончании каждых
100 итераций рассчитывали значение смещения для каждого X.
По итогам прохождения внешнего цикла или же при его прерывании, когда значение
смещения было близким к 0, мы получаем само значение смещения, а также значение
X, при котором было получено смещение. В результате, мы получили значение X 
равное 1.572.
Стоит заметить, что 1.572 является округлением числа ㄫ/2, значение Y
теоретического в котором равно 0. Поскольку значение смещения равное 0 не 
удается получить даже при точности до одной миллионной, можно сделать 
предположение, что 0 достигается именно в точке ㄫ/2, а оно является 
иррациональным и количество знаков после запятой у него бесконечно.'

######################## Task 10 #############################################

'10. Предыдущий пункт иллюстрирует, что показатели смещения и разброса, 
рассчитанные в скрипте, зависят от точки, в которой они определяются. 
Преобразовать скрипт, чтобы устранить этот недостаток.'

'Для того, чтобы показания смещения и разброса не зависели от точки, в которой
они определяются, их можно рассчитать для всех точек, которые заданы в выборке. 
Для этого мы изменили алгоритм обучения модели, добавив внешний цикл для того, 
чтобы модель обучалась на всех X из выборки.
Для показателей смещения мы изменили входные данные функции, в нашем коде
теоретическое значение Y рассчитывается внутри функции, а на вход подаются
все значения X в выборке. В итоге мы получаем общий показатель смещения для
каждой из моделей.
Что касается показателей разброса, по ним также теперь посчитано значение, 
основываясь на прогнозах для каждого значения X в выборке.'

# P.S. Если необходимо посчитать смещения и разброс для некоторого определенного
# набора точек, можно воспользоваться следующим кодом:
X_pred = df$X

# bias
my_bias = function(predict, x){
  (mean(cos(x) - predict))
}

predictions_all = data.frame()

for (x in X_pred){
  for (i in seq(1, 100, 1)){
    df_tmp = get_data()
    pred_tmp = predict_data(df_tmp, x)
    predictions_all = rbind(predictions_all, pred_tmp)
  }
}

bias = apply(X=predictions_all, MARGIN=2, FUN=my_bias, x = X_pred)
bias

# variance
my_var = function(predict){
  mean((predict - mean(predict)) ^ 2)}

variance = apply(X=predictions_all, MARGIN=2, FUN=my_var)
variance

