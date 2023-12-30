Claro! Vou criar um código em Python complexo que envolve a manipulação de dados e a geração de gráficos. O objetivo é calcular o índice de massa corporal (IMC) com base em um conjunto de dados de várias pessoas e, em seguida, apresentar os resultados de forma gráfica.

Primeiro, vamos importar a biblioteca necessária para criar os gráficos:

```python
import matplotlib.pyplot as plt
```

Agora, vamos definir uma função para calcular o IMC:

```python
def calcular_imc(peso, altura):
    imc = peso / (altura ** 2)
    return imc
```

A próxima etapa é definir uma lista de nomes, pesos e alturas de várias pessoas:

```python
nomes = ["João", "Maria", "Pedro", "Ana"]
pesos = [70, 65, 80, 55]
alturas = [1.75, 1.60, 1.85, 1.70]
```

Vamos criar uma lista vazia para armazenar os resultados do IMC:

```python
imcs = []
```

Agora, vamos calcular o IMC para cada pessoa utilizando um loop:

```python
for i in range(len(nomes)):
    imc = calcular_imc(pesos[i], alturas[i])
    imcs.append(imc)
```

Agora que temos os valores do IMC para cada pessoa, vamos criar um gráfico de barra para visualizar esses resultados:

```python
plt.bar(nomes, imcs)
plt.xlabel("Nomes")
plt.ylabel("IMC")
plt.title("Índice de Massa Corporal (IMC)")
plt.show()
```

E pronto! O código completo ficaria assim:

```python
import matplotlib.pyplot as plt

def calcular_imc(peso, altura):
    imc = peso / (altura ** 2)
    return imc

nomes = ["João", "Maria", "Pedro", "Ana"]
pesos = [70, 65, 80, 55]
alturas = [1.75, 1.60, 1.85, 1.70]

imcs = []

for i in range(len(nomes)):
    imc = calcular_imc(pesos[i], alturas[i])
    imcs.append(imc)

plt.bar(nomes, imcs)
plt.xlabel("Nomes")
plt.ylabel("IMC")
plt.title("Índice de Massa Corporal (IMC)")
plt.show()
```

Esse código calcula o IMC de quatro pessoas diferentes e exibe esses resultados em um gráfico de barras. É um exemplo de como podemos usar a programação para analisar dados e visualizá-los de forma mais compreensível.