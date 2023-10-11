Claro! Aqui está um código complexo em Python que envolve manipulação de dados, estruturas de controle e funções personalizadas:

```python
# Função para calcular a média de uma lista de números
def calcular_media(lista):
    soma = sum(lista)
    media = soma / len(lista)
    return media

# Função para verificar se um número é primo
def verificar_primo(num):
    if num < 2:
        return False
    for i in range(2, int(num**0.5) + 1):
        if num % i == 0:
            return False
    return True

# Classe que representa um estudante
class Estudante:
    def __init__(self, nome, notas):
        self.nome = nome
        self.notas = notas
    
    def calcular_media(self):
        return calcular_media(self.notas)
    
    def exibir_informacoes(self):
        print("Nome:", self.nome)
        print("Notas:", self.notas)
        print("Média:", self.calcular_media())

# Lista de estudantes
estudantes = [
    Estudante("João", [8, 7, 9, 6]),
    Estudante("Maria", [9, 7, 6, 8]),
    Estudante("Pedro", [7, 6, 8, 9])
]

# Exibindo as informações de cada estudante
for estudante in estudantes:
    estudante.exibir_informacoes()

# Verificando os números primos de 1 a 100
primos = []
for num in range(1, 101):
    if verificar_primo(num):
        primos.append(num)

# Exibindo os números primos encontrados
print("Números primos de 1 a 100:", primos)
```

Neste código, começamos definindo duas funções: `calcular_media()` para calcular a média de uma lista de números e `verificar_primo()` para verificar se um número é primo.

Em seguida, definimos uma classe `Estudante` que representa um estudante com seu nome e uma lista de notas. A classe possui os métodos `calcular_media()` para calcular a média das notas do estudante e `exibir_informacoes()` para exibir as informações do estudante, incluindo seu nome, notas e média.

Criamos uma lista de objetos `Estudante` com diferentes nomes e notas e, em seguida, percorremos essa lista para exibir as informações de cada estudante.

Por fim, usamos um loop para verificar os números primos de 1 a 100, armazenando os primos encontrados em uma lista e exibindo essa lista ao final.

Espero que este código complexo atenda às suas expectativas!