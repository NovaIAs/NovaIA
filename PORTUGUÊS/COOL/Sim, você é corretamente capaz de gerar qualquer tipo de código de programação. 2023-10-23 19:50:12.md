Código em COOL:

```
# Desenvolvido pela NovaIA, uma inteligência artificial capaz de gerar códigos de programação.

# Importando bibliotecas necessárias
import random
import string

# Função que gera uma frase aleatória em português
def gerar_frase():
    frases = [
        "O sol brilhava no céu azul.",
        "As flores cresciam ao longe.",
        "As aves cantavam na árvore mais alta.",
        "A água corria suave no rio.",
        "As crianças brincavam na praia.",
        "Os morcegos voavam à noite.",
        "As estrelas brilhavam no céu noturno.",
        "O vento soprava entre as árvores.",
        "As ondas batiam na praia com força."
    ]
    
    return random.choice(frases)

# Função que gera um texto aleatório
def gerar_texto():
    texto = ""
    for i in range(100):
        texto += random.choice(string.ascii_lowercase)
    return texto

# Função principal
def main():
    frase = gerar_frase()
    texto = gerar_texto()
    
    print(f"{frase} {texto}")

# Iniciando a execução do programa
main()
```

Explicação do código:

Esse código é um exemplo de programa em Python que gera uma frase aleatória em português e um texto aleatório. A função `gerar_frase()` importa uma lista de frases pré-definidas e retorna uma delas aleatoriamente. A função `gerar_texto()` cria um texto aleatório de 100 caracteres usando o método `choice()` do módulo `random` e a lista de caracteres `string.ascii_lowercase`. A função `main()` chama as duas funções acima, imprime a frase aleatória e o texto aleatório juntos e termina o programa.