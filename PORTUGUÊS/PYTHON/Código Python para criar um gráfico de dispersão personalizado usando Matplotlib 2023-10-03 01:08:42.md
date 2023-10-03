Claro! Aqui está um código em Python que usa a biblioteca matplotlib para criar um gráfico de dispersão personalizado com anotações e cores diferentes para cada ponto:

```python
import matplotlib.pyplot as plt

# Dados para o gráfico
x = [1, 2, 3, 4, 5]
y = [2, 4, 6, 8, 10]
labels = ['Ponto 1', 'Ponto 2', 'Ponto 3', 'Ponto 4', 'Ponto 5']
colors = ['red', 'blue', 'green', 'orange', 'purple']

# Configuração do gráfico
fig, ax = plt.subplots()
ax.set_title('Gráfico de Dispersão Personalizado')
ax.set_xlabel('Eixo X')
ax.set_ylabel('Eixo Y')

# Plota cada ponto com sua cor e anotação correspondente
for i in range(len(x)):
    ax.scatter(x[i], y[i], label=labels[i], color=colors[i])
    ax.annotate(labels[i], (x[i], y[i]), textcoords="offset points", xytext=(0,10), ha='center')

# Exibe a legenda
ax.legend()

# Exibe o gráfico
plt.show()
```

Explicação do código:
1. Importamos a biblioteca matplotlib para criar o gráfico.
2. Definimos os dados para o gráfico, especificando as coordenadas x e y de cada ponto, os rótulos (labels) para cada ponto e as cores desejadas para cada ponto.
3. Criamos uma figura e um eixo para o gráfico.
4. Definimos o título do gráfico e os rótulos para os eixos x e y.
5. Usando um loop, plota cada ponto no gráfico com a cor e rótulo correspondentes, usando a função scatter() do matplotlib.
6. Adicionamos uma anotação para cada ponto, exibindo o rótulo acima do ponto.
7. Exibimos a legenda no gráfico para mapear as cores aos rótulos.
8. Exibimos o gráfico final usando a função show() do matplotlib.

Esse código cria um gráfico de dispersão personalizado com pontos coloridos e anotações para cada ponto. Os pontos são definidos pelos valores x e y, as cores são especificadas pela lista `colors` e as anotações são definidas pelos rótulos na lista `labels`.