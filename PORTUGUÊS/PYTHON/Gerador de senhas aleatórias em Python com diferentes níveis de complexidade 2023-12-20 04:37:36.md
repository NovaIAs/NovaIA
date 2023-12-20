Claro! Aqui está um exemplo de código em Python que cria um gerador de senhas aleatórias com diferentes níveis de complexidade:

```python
import random
import string

def gerar_senha(comprimento, complexidade):
    caracteres = ""
    if complexidade == "baixa":
        caracteres = string.ascii_letters + string.digits
    elif complexidade == "média":
        caracteres = string.ascii_letters + string.digits + string.punctuation
    elif complexidade == "alta":
        caracteres = string.ascii_letters + string.digits + string.punctuation + string.whitespace
    else:
        raise ValueError("Nível de complexidade inválido")

    senha = ''.join(random.choice(caracteres) for _ in range(comprimento))
    return senha

def main():
    print("Bem-vindo ao gerador de senhas aleatórias!")
    comprimento = int(input("Digite o comprimento desejado para a senha: "))
    complexidade = input("Digite o nível de complexidade (baixa, média ou alta): ")

    senha = gerar_senha(comprimento, complexidade)
    print("A senha gerada é:", senha)

if __name__ == "__main__":
    main()
```

Neste código, temos uma função `gerar_senha` que recebe dois parâmetros: `comprimento` e `complexidade`. O parâmetro `comprimento` define o tamanho da senha desejada, e o parâmetro `complexidade` define o nível de complexidade da senha (baixa, média ou alta).

Dentro da função `gerar_senha`, verificamos o nível de complexidade fornecido e atribuímos os caracteres adequados à variável `caracteres`. Em seguida, utilizamos a função `random.choice` para escolher caracteres aleatórios a partir da string `caracteres` e concatenamos o resultado em uma string chamada `senha`. Por fim, retornamos a senha gerada.

A função `main` é responsável por interagir com o usuário, solicitando o comprimento e o nível de complexidade desejados. Em seguida, chama a função `gerar_senha` passando os parâmetros fornecidos e exibe a senha gerada.

Para executar o programa, basta chamar a função `main`. O usuário será guiado pelas instruções e receberá uma senha aleatória com o comprimento e complexidade desejados.