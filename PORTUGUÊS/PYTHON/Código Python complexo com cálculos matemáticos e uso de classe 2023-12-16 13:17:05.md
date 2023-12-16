Claro! Aqui está um exemplo de um código Python complexo que realiza uma série de operações matemáticas:

```python
# Função para calcular o fatorial de um número
def fatorial(n):
    if n == 0:
        return 1
    else:
        return n * fatorial(n-1)

# Função para calcular o número de Fibonacci
def fibonacci(n):
    if n <= 1:
        return n
    else:
        return fibonacci(n-1) + fibonacci(n-2)

# Classe para calcular a média dos valores de uma lista
class Calculadora:
    def __init__(self, numeros):
        self.numeros = numeros

    def calcular_media(self):
        soma = sum(self.numeros)
        media = soma / len(self.numeros)
        return media

# Função para calcular a média ponderada de três notas
def media_ponderada(nota1, nota2, nota3):
    peso1, peso2, peso3 = 2, 3, 5
    soma_pesos = peso1 + peso2 + peso3
    media = (nota1*peso1 + nota2*peso2 + nota3*peso3) / soma_pesos
    return media

# Código principal
if __name__ == "__main__":
    numero = 7
    print("O fatorial de", numero, "é", fatorial(numero))

    numero_fibonacci = 10
    print("O", numero_fibonacci, "número de Fibonacci é", fibonacci(numero_fibonacci))

    lista_numeros = [2, 4, 6, 8, 10]
    calculadora = Calculadora(lista_numeros)
    print("A média dos valores da lista", lista_numeros, "é", calculadora.calcular_media())

    nota1, nota2, nota3 = 7.5, 8.0, 6.5
    print("A média ponderada das notas", nota1, ",", nota2, "e", nota3, "é", media_ponderada(nota1, nota2, nota3))
```

Neste código, temos algumas funções e uma classe que realizam cálculos matemáticos. 

A função `fatorial` calcula o fatorial de um número usando recursão. 

A função `fibonacci` retorna o enésimo número da sequência de Fibonacci usando recursão também.

A classe `Calculadora` recebe uma lista de números e possui um método `calcular_media` que calcula a média dos valores da lista.

A função `media_ponderada` calcula a média ponderada de três notas, atribuindo pesos diferentes a cada nota.

No código principal, alguns exemplos de uso das funções e da classe são mostrados, imprimindo os resultados na tela.

Espero que este código atenda às suas expectativas!