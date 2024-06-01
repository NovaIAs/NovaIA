**Code complexe en PORTUGOL :**

```portuol
# Importe a biblioteca math
importar "math"

# Defina uma função para calcular o seno de um ângulo
funcao seno(angulo)
  retornar sin(angulo)

# Defina uma função para calcular o cosseno de um ângulo
funcao cosseno(angulo)
  retornar cos(angulo)

# Defina uma função para calcular a tangente de um ângulo
funcao tangente(angulo)
  retornar tan(angulo)

# Defina uma função para calcular a área de um triângulo
funcao areaTriangulo(base, altura)
  retornar (base * altura) / 2

# Defina uma função para calcular o volume de uma esfera
funcao volumeEsfera(raio)
  retornar (4 / 3) * pi * (raio ** 3)

# Defina uma função para calcular a distância entre dois pontos
funcao distanciaPontos(x1, y1, x2, y2)
  retornar sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2)

# Defina uma função para calcular a média de uma lista de números
funcao media(numeros)
  soma = 0
  para i = 1 para tamanho(numeros)
    soma = soma + numeros[i]
  retornar soma / tamanho(numeros)

# Defina uma função para verificar se um número é primo
funcao ehPrimo(numero)
  se numero <= 1
    retornar falso
  para i = 2 para inteiro(sqrt(numero))
    se numero % i == 0
      retornar falso
  retornar verdadeiro

# Defina uma função para gerar uma lista de números primos até um determinado número
funcao gerarPrimos(limite)
  primos = []
  para i = 1 para limite
    se ehPrimo(i)
      adicionar(primos, i)
  retornar primos

# Defina uma função para inverter uma string
funcao inverterString(str)
  invertido = ""
  para i = tamanho(str) para 1 passo -1
    invertido = invertido + str[i]
  retornar invertido

# Defina uma função para ordenar uma lista de strings
funcao ordenarStrings(lista)
  lista_ordenada = copia(lista)
  ordenar(lista_ordenada)
  retornar lista_ordenada

# Defina uma função para pesquisar um elemento em uma lista
funcao pesquisar(lista, elemento)
  para i = 1 para tamanho(lista)
    se lista[i] == elemento
      retornar i
  retornar -1

# Defina uma função para remover um elemento de uma lista
funcao remover(lista, elemento)
  indice = pesquisar(lista, elemento)
  se indice >= 0
    remover(lista, indice)

# Defina uma função para contar o número de ocorrências de um elemento em uma lista
funcao contarOcorrencias(lista, elemento)
  ocorrencias = 0
  para i = 1 para tamanho(lista)
    se lista[i] == elemento
      ocorrencias = ocorrencias + 1
  retornar ocorrencias
```

**Explicação do código :**

Ce code couvre un large éventail de sujets mathématiques et informatiques, notamment :

* **Calculs trigonométriques** : calcul du sinus, cosinus et tangente d'un angle.
* **Géométrie plane** : calcul de l'aire d'un triangle.
* **Géométrie dans l'espace** : calcul du volume d'une sphère.
* **Mathématiques de base** : calcul de la distance entre deux points, de la moyenne d'une liste de nombres et vérification de la primalité d'un nombre.
* **Mathématiques discrètes** : génération d'une liste de nombres premiers, inversion d'une chaîne, tri d'une liste de chaînes, recherche et suppression d'un élément dans une liste, comptage des occurrences d'un élément dans une liste.