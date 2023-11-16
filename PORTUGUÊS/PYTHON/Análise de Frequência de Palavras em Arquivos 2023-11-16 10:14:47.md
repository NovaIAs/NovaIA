Claro! Aqui está um código complexo em Python que lida com manipulação de arquivos e utiliza alguns conceitos avançados de programação:

```python
import os

def contar_palavras_arquivos(diretorio):
    palavras = {}
    for root, dirs, files in os.walk(diretorio):
        for file in files:
            if file.endswith(".txt"):
                caminho_arquivo = os.path.join(root, file)
                with open(caminho_arquivo, "r") as arquivo:
                    for linha in arquivo:
                        for palavra in linha.split():
                            if palavra in palavras:
                                palavras[palavra] += 1
                            else:
                                palavras[palavra] = 1
    return palavras

def ordenar_palavras(palavras):
    return sorted(palavras.items(), key=lambda x: x[1], reverse=True)

def imprimir_palavras_frequentes(diretorio):
    palavras = contar_palavras_arquivos(diretorio)
    palavras_ordenadas = ordenar_palavras(palavras)
    for palavra, frequencia in palavras_ordenadas:
        print(f"{palavra}: {frequencia}")

diretorio = input("Digite o diretório a ser analisado: ")
imprimir_palavras_frequentes(diretorio)
```

Este código tem como objetivo contar a frequência de palavras em arquivos de texto presentes em um diretório e seus subdiretórios. 

A função `contar_palavras_arquivos` recebe um diretório como parâmetro e percorre todos os arquivos `.txt` presentes nesse diretório e em seus subdiretórios. Ao encontrar um arquivo, ele é aberto e cada linha é lida. Em seguida, cada linha é dividida em palavras utilizando o método `split()`. Para cada palavra encontrada, é verificado se ela já existe no dicionário `palavras`, que armazena a frequência de cada palavra. Caso a palavra já exista, o valor correspondente é incrementado em 1. Caso contrário, a palavra é adicionada ao dicionário com frequência 1.

A função `ordenar_palavras` recebe o dicionário de palavras e retorna uma lista de tuplas ordenadas por frequência, em ordem decrescente. A ordenação é feita utilizando a função `sorted()` com o parâmetro `key` sendo uma função lambda que extrai o valor correspondente à frequência de cada palavra.

A função `imprimir_palavras_frequentes` recebe um diretório como parâmetro e imprime na tela as palavras mais frequentes nos arquivos presentes nesse diretório e em seus subdiretórios. Primeiro, ela chama a função `contar_palavras_arquivos` para obter o dicionário de palavras e suas frequências. Em seguida, chama a função `ordenar_palavras` para obter a lista ordenada de palavras. Por fim, itera sobre essa lista e imprime cada palavra juntamente com sua frequência.

No final do código, é solicitado ao usuário que digite o diretório a ser analisado, e então a função `imprimir_palavras_frequentes` é chamada com esse diretório como parâmetro, mostrando as palavras mais frequentes na tela.