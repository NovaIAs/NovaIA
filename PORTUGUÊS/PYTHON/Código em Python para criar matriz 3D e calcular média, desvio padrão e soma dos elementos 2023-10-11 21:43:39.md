Claro! Aqui está um código complexo em Python que utiliza a biblioteca random para criar uma matriz 3D de valores aleatórios entre 0 e 1. Em seguida, ele realiza uma série de operações matemáticas para calcular a média, o desvio padrão e a soma dos elementos dessa matriz.

```python
import random
import math

def criar_matriz_3d(dimensao):
    matriz = []
    for i in range(dimensao):
        matriz2d = []
        for j in range(dimensao):
            matriz1d = []
            for k in range(dimensao):
                matriz1d.append(random.random())  # Gera um valor aleatório entre 0 e 1
            matriz2d.append(matriz1d)
        matriz.append(matriz2d)
    return matriz

def calcular_media(matriz):
    soma = 0
    elementos = 0
    for matriz2d in matriz:
        for matriz1d in matriz2d:
            for elemento in matriz1d:
                soma += elemento
                elementos += 1
    media = soma / elementos
    return media

def calcular_desvio_padrao(matriz, media):
    soma_quadrados = 0
    elementos = 0
    for matriz2d in matriz:
        for matriz1d in matriz2d:
            for elemento in matriz1d:
                soma_quadrados += (elemento - media) ** 2
                elementos += 1
    variancia = soma_quadrados / elementos
    desvio_padrao = math.sqrt(variancia)
    return desvio_padrao

def calcular_soma(matriz):
    soma = 0
    for matriz2d in matriz:
        for matriz1d in matriz2d:
            for elemento in matriz1d:
                soma += elemento
    return soma

dimensao = 5  # Definir a dimensão da matriz 3D
matriz = criar_matriz_3d(dimensao)
media = calcular_media(matriz)
desvio_padrao = calcular_desvio_padrao(matriz, media)
soma = calcular_soma(matriz)

print("Matriz 3D:")
for matriz2d in matriz:
    for matriz1d in matriz2d:
        print(matriz1d)
    print()

print("Média:", media)
print("Desvio Padrão:", desvio_padrao)
print("Soma:", soma)
```

Este código começa importando as bibliotecas `random` e `math`. Em seguida, define uma função `criar_matriz_3d` que recebe a dimensão da matriz como parâmetro e retorna uma matriz 3D preenchida com valores aleatórios entre 0 e 1.

Depois, temos a função `calcular_media`, que percorre todos os elementos da matriz e calcula a média dos valores.

A função `calcular_desvio_padrao` recebe a matriz e a média como parâmetros e utiliza esses valores para calcular o desvio padrão dos elementos da matriz.

Em seguida, a função `calcular_soma` percorre todos os elementos da matriz e realiza a soma deles.

Após as definições das funções, o código define a dimensão da matriz como 5 e chama as funções para criar a matriz, calcular a média, o desvio padrão e a soma dos elementos. Por fim, exibe a matriz, a média, o desvio padrão e a soma na saída.