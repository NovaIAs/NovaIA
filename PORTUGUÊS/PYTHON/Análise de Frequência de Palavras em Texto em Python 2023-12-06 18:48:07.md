Claro! Aqui está um exemplo de um código complexo em Python, que envolve manipulação de arquivos, processamento de texto e estruturas de dados avançadas. O código é explicado ao longo do exemplo para ajudar na compreensão.

```python
import re
from collections import defaultdict

def contar_palavras(texto):
    # Lendo o arquivo de texto
    with open(texto, 'r', encoding='utf-8') as arquivo:
        texto_completo = arquivo.read()

    # Removendo caracteres especiais e transformando em minúsculas
    texto_completo = re.sub(r'[^\w\s]', '', texto_completo.lower())

    # Dividindo o texto em palavras
    palavras = texto_completo.split()

    # Contando a ocorrência de cada palavra usando defaultdict
    contador = defaultdict(int)
    for palavra in palavras:
        contador[palavra] += 1

    # Ordenando as palavras por frequência, do maior para o menor
    palavras_ordenadas = sorted(contador.items(), key=lambda x: x[1], reverse=True)

    # Imprimindo as 10 palavras mais frequentes
    for palavra, frequencia in palavras_ordenadas[:10]:
        print(f'{palavra}: {frequencia} ocorrências')

    # Verificando a palavra mais longa
    palavra_mais_longa = max(palavras, key=len)
    print(f'A palavra mais longa é: {palavra_mais_longa}')

    # Retornando a lista de palavras e suas frequências
    return palavras_ordenadas

# Executando a função com um arquivo de exemplo
resultados = contar_palavras('texto_exemplo.txt')
```

Neste exemplo, o código lê um arquivo de texto em UTF-8, remove caracteres especiais, transforma todas as palavras em minúsculas e as divide em uma lista. Em seguida, utiliza um dicionário do tipo `defaultdict` para contar a ocorrência de cada palavra do texto.

Depois, as palavras são ordenadas por frequência de forma decrescente. As 10 palavras mais frequentes são impressas junto com o número de ocorrências. Além disso, o código encontra a palavra mais longa no texto e a imprime.

Por fim, a função retorna a lista de palavras ordenadas com suas respectivas frequências.

Vale ressaltar que o código assume que existe um arquivo chamado "texto_exemplo.txt" presente no mesmo diretório em que o código está sendo executado.