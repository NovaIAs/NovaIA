Claro! Aqui está um exemplo de código complexo em Python:

```
# Programa de análise de texto

# Função para contar o número de palavras em um texto
def contar_palavras(texto):
    palavras = texto.split()
    return len(palavras)

# Função para contar o número de caracteres em um texto
def contar_caracteres(texto):
    return len(texto)

# Função para contar o número de letras maiúsculas em um texto
def contar_letras_maiusculas(texto):
    maiusculas = 0
    for letra in texto:
        if letra.isupper():
            maiusculas += 1
    return maiusculas

# Função para contar o número de vogais em um texto
def contar_vogais(texto):
    vogais = 0
    for letra in texto:
        if letra.lower() in 'aeiou':
            vogais += 1
    return vogais

# Função principal
def main():
    # Solicitar ao usuário que digite um texto
    texto = input("Digite um texto: ")

    # Chamar as funções para realizar as análises
    num_palavras = contar_palavras(texto)
    num_caracteres = contar_caracteres(texto)
    num_maiusculas = contar_letras_maiusculas(texto)
    num_vogais = contar_vogais(texto)

    # Exibir os resultados
    print("Análise do Texto:")
    print("- Número de palavras:", num_palavras)
    print("- Número de caracteres:", num_caracteres)
    print("- Número de letras maiúsculas:", num_maiusculas)
    print("- Número de vogais:", num_vogais)

# Chamar a função principal
if __name__ == "__main__":
    main()
```

Neste código, temos um programa de análise de texto. Ele possui várias funções que executam diferentes análises em um texto fornecido pelo usuário.

A função `contar_palavras` recebe o texto e utiliza o método `split()` para separar as palavras em uma lista. Em seguida, retorna o comprimento dessa lista, que representa o número de palavras no texto.

A função `contar_caracteres` recebe o texto e retorna o comprimento do próprio texto, que representa o número de caracteres.

A função `contar_letras_maiusculas` percorre cada letra do texto e verifica se ela é maiúscula utilizando o método `isupper()`. Caso seja maiúscula, incrementa o contador `maiusculas`. No final, retorna o valor de `maiusculas`, que representa o número de letras maiúsculas no texto.

A função `contar_vogais` também percorre cada letra do texto e verifica se ela é uma vogal. Para isso, utiliza o método `lower()` para considerar tanto as vogais maiúsculas quanto minúsculas. Se a letra for uma vogal, incrementa o contador `vogais`. No final, retorna o valor de `vogais`, que representa o número de vogais no texto.

Na função principal `main()`, solicitamos que o usuário digite um texto. Em seguida, chamamos cada uma das funções de análise e armazenamos os resultados em variáveis correspondentes. Por fim, exibimos os resultados na tela.

Por fim, temos um bloco `if __name__ == "__main__":` que verifica se o código está sendo executado diretamente (e não sendo importado como um módulo). Se for o caso, chama a função principal `main()`. Isso permite que o programa seja executado quando o script é executado diretamente, mas não quando é importado como um módulo.