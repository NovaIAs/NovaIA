```portuguol
# Programa: Calculadora de derivadas de funções polinomiais

# Declaração de variáveis
funcao = ""
derivada = ""
grau = 0
coeficientes = []

# Leitura da função polinomial
print("Digite a função polinomial:")
funcao = input()

# Separação da função em coeficientes e grau
partes = funcao.split("x^")
grau = int(partes[1])
coeficientes = [int(c) for c in partes[0].split()]

# Cálculo da derivada
for i in range(grau):
    if coeficientes[i] != 0:
        derivada += str(coeficientes[i] * (grau - i)) + "x^" + str(grau - i - 1)
        if i < grau - 1:
            derivada += " + "

# Impressão da derivada
print("A derivada da função é:")
print(derivada)

```

Explicação do código:

* O programa começa declarando as variáveis necessárias, incluindo a função polinomial, sua derivada, o grau da função e os coeficientes dos termos da função.

* Em seguida, o programa lê a função polinomial do usuário.

* A função é então separada em coeficientes e grau, separando a expressão pelo caractere "x^". O grau da função é extraído da última parte da expressão, e os coeficientes são extraídos da primeira parte, separando-os pelo caractere " ".

* O programa então calcula a derivada da função polinomial. Isso é feito multiplicando cada coeficiente pelo expoente do termo correspondente e decrementando o expoente em uma unidade. Por exemplo, o termo `3x^2` tem um coeficiente de 3 e um expoente de 2. Sua derivada é `6x^1`, que é igual a `6x`.

* A derivada é então impressa na tela.