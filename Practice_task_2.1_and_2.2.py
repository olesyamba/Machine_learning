"""
Machine Learning Practice task 2
Done by Olesya Krasnukhina 
"""

# Task №1
'''1. Объяснить своими словами, зачем в работе используется 
квантильная регрессия вместо классической регрессии?

Выбор авторами квантильной регрессии вместо классической 
обусловлен целью исследования. Как заявляют авторы: 
“ Наша основная цель - охарактеризовать все распределение 
структуры заработной платы, поэтому мы используем семнадцать 
различных условных квантилей”.

Классический метод наименьших квадратов позволил бы им 
построить прогноз и описать структуру взаимосвязи зависимой 
и объясняющих переменных только по среднему значению распределения.
А квантильная регрессия, в свою очередь, позволяет вместо 
среднего использовать любой квантиль условного распределения,
что позволяет более полно (на уровне любого квантиля, а 
значит в теории возможно и полностью) охарактеризовать 
распределение структуры ЗП, а значит достичь цели 
исследования. Кроме того, она позволяет исследовать
влияние объясняющих переменных на форму распределения, 
а также на условное влияние регрессоров на зависимые 
переменные на разных "слоях" распределения.
Кроме того, метод робастен: важнее сколько ошибок 
положительных и сколько отрицательных, чем какой они величины.
А также он позволяет построить несколько кривых регрессии 
(на разных квантилях) вместо одной.'''

'''
Используя обозначения статьи, построить график квантильной функции потерь с параметром θ=0.9 в осях error vs loss.
'''
import numpy as np
import matplotlib.pyplot as plt

def compute_quantile_loss(y, f, q):
  # q: Quantile to be evaluated, e.g., 0.5 for median.
  # y: True value.
  # f: Fitted (predicted) value.
  e = y - f
  return np.maximum(q * e, (1 - q) * e)

# Suppose we've made a prediction for a single point with a true value of zero,
# and our predictions range from -1 to +1 that is, our errors also range from -1 to +1.
n_samples = 1000
y_true = np.zeros(n_samples)
y_pred = np.linspace(-1, 1, n_samples)
res = np.array(y_true - y_pred)

quantile_losses = compute_quantile_loss(y_true, y_pred, 0.9)
# change default style figure and font size
plt.rcParams['figure.figsize'] = 12, 8
plt.rcParams['font.size'] = 12

plt.plot(y_pred, quantile_losses)

plt.legend([str(int(90)) + 'th percentile'])
plt.xlabel('Error')
plt.ylabel('Quantile loss')
plt.title('Quantile loss by error and quantile', loc='left')
plt.show()

# Task №2
'''
Записать функции потерь Хубера с параметрами 0.5 и 4 как функции Loss(e). Построить графики выписанных функций в осях error vs loss.
'''

import scipy

deltas = [0.5, 4]
huber = [
  scipy.special.huber(delta, res)
  for delta in deltas
]

# change default style figure and font size
plt.rcParams['figure.figsize'] = 12, 8
plt.rcParams['font.size'] = 12

for huber2 in huber:
  plt.plot(y_pred, huber2)

plt.legend([str(d) for d in deltas])
plt.xlabel('Error')
plt.ylabel('Huber loss')
plt.title('Huber loss by error and quantile', loc='left')
plt.show()