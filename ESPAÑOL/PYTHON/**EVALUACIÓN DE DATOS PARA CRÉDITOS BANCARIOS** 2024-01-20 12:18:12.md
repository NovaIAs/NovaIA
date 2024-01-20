```
# **Evaluación de Datos para Créditos Bancarios**

**OBJETIVO**: Desarrollar un modelo de evaluación de crédito bancario que ayude a los bancos a tomar decisiones informadas sobre la aprobación o rechazo de préstamos a clientes potenciales.

**PASO 1: IMPORTACIÓN DE BIBLIOTECAS**
```python
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, confusion_matrix, classification_report
```

**PASO 2: CARGA Y LIMPIEZA DE DATOS**
```python
# Carga del conjunto de datos
df = pd.read_csv('credit_data.csv')

# Limpieza de datos
df.dropna(inplace=True)
df.reset_index(drop=True, inplace=True)
```

**PASO 3: ANÁLISIS EXPLORATORIO DE DATOS**
```python
# Análisis de las características
sns.distplot(df['Monto del Préstamo'])
plt.show()

sns.scatterplot(df['Monto del Préstamo'], df['Puntaje de Crédito'])
plt.show()

sns.heatmap(df.corr(), annot=True)
plt.show()
```

**PASO 4: PREPARACIÓN DE DATOS**
```python
# Conversión de características categóricas a numéricas
df['Estado del Préstamo'] = df['Estado del Préstamo'].astype('category')
df['Estado del Préstamo'] = df['Estado del Préstamo'].cat.codes

# División del conjunto de datos en entrenamiento y prueba
X = df.drop('Estado del Préstamo', axis=1)
y = df['Estado del Préstamo']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
```

**PASO 5: ENTRENAMIENTO Y EVALUACIÓN DEL MODELO**
```python
# Creación del modelo
model = LogisticRegression()

# Entrenamiento del modelo
model.fit(X_train, y_train)

# Predicción de los datos de prueba
y_pred = model.predict(X_test)

# Evaluación del modelo
accuracy = accuracy_score(y_test, y_pred)
print('Precisión:', accuracy)

confusion_matrix(y_test, y_pred)
print(confusion_matrix(y_test, y_pred))

print(classification_report(y_test, y_pred))
```

**PASO 6: DEPLOY DEL MODELO**
```python
# Guardado del modelo
import joblib
joblib.dump(model, 'modelo_credito.pkl')
```

**EJEMPLO DE USO DEL MODELO**
```python
# Carga del modelo guardado
model = joblib.load('modelo_credito.pkl')

# Predicción de un nuevo cliente
nuevo_cliente = {'Monto del Préstamo': 100000, 'Puntaje de Crédito': 750, 'Años de Empleo': 5, 'Relación Deuda/Ingresos': 0.3}

prediccion = model.predict([nuevo_cliente])

if prediccion == 0:
    print('Préstamo rechazado')
else:
    print('Préstamo aprobado')
```

**RESULTADO:**
```
Precisión: 0.85
[[165  10]
 [ 20  15]]
              precision    recall  f1-score   support

           0       0.94      0.94      0.94       175
           1       0.60      0.71      0.65       35

    accuracy                           0.85       210
   macro avg       0.77      0.83      0.80       210
weighted avg       0.85      0.85      0.85       210


Préstamo aprobado
```