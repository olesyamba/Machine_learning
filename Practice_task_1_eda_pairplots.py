"""
Machine Learning in Economics and Finance
Done by Krasnukhina Olesya 
"""

# Import libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

pd.set_option('display.max_columns', None)

# Import data
df = pd.read_csv('Data HW1/data.csv')

# Task 1
# Overall information about dataset and summary statistics of the variables
print(df)
print(df.describe().T)
print(df.info())
print(df.isnull().sum())
print(df['liked'].value_counts())

# Number of unique values
print("Number of unique values:\n", df.nunique())

# Task 4 Build 3 different most representative ways how to
# depict pair distribution | Graph 1
sns.jointplot(df,
              x=df['valence'],
              y=df['tempo'],
              hue="liked",
              kind='scatter')
plt.show()

# Task 4 | Graph 2
sns.displot(df,
            x='mode',
            kde=True,
            hue="liked",
            multiple='dodge',
            col='mode',
            height=4,
            aspect=0.5)
plt.show()

# Task 4 | Graph 3
sns.catplot(df,
            x="key",
            y="instrumentalness",
            hue="liked",
            kind="strip")
plt.show()

# Корреляция
upp_mat = np.triu(df.corr())
plt.figure(figsize=(12, 9))
sns.heatmap(df.corr(), vmin=-1, vmax=+1, annot=True, cmap='coolwarm', linewidths=.5, mask=upp_mat)
plt.tight_layout()
plt.show()

# Точечный и настраивание размера
plt.figure(figsize=(15, 5))
sns.scatterplot(df.skew().sort_values(ascending=False))
plt.show()

# Огромный пэирплот для препроцессинга
sns.pairplot(df, hue="liked")
plt.show()

# Двумерное распределение, меняешь кайнд на хист или скейтер и получаешь крутые штуки, но кде круче
sns.displot(df, x=df['energy'], y=df['duration_ms'], bins=16, hue="liked", kind='kde')
plt.show()
# ['scatter', 'hist', 'hex', 'kde', 'reg', 'resid'] варианты кайнда

# Джойнплот, красивые графики с двумерным и одномерным распределением сразу, кайнд кде надо
sns.jointplot(df, x=df['energy'], y=df['loudness'], hue="mode", kind='scatter')
plt.show()

sns.jointplot(df, x=df['acousticness'], y=df['energy'], hue="key", kind='kde')
plt.show()

# Надо додумать, но это более крутой пэирплот, где сверху гисты, снизу кде, на осях одномерное распределение
sns.set(style="darkgrid")
sns.pairplot(g, hue="liked")

# Создаем экземпляр класса:
g = sns.PairGrid(df,  hue="liked")

# задаем тип графиков над главной диагональю:
g.map_upper(sns.histplot, bins=30)

# задаем тип графиков под главной диагональю:
g.map_lower(sns.kdeplot, bw_adjust=0.7)

# задаем тип графиков на главной диагонали:
g.map_diag(sns.histplot, kde=True)
sns.pairplot(g, hue="liked")
plt.show()
"""
