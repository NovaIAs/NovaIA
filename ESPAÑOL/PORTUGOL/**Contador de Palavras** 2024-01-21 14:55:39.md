```portuguol
/*
Este programa lê um arquivo de texto e conta o número de palavras nele.
*/

ler arquivo "arquivo.txt" como texto
texto_quebrado = quebrar_texto_em_palavras(texto)
numero_de_palavras = tamanho(texto_quebrado)

escrever "O arquivo contém ", numero_de_palavras, " palavras.\n"

definir função quebrar_texto_em_palavras(texto)
    palavras = []
    for palavra in texto.split(" "):
        if palavra != "":
            palavras.append(palavra)
    return palavras

/*
Esta função recebe um texto e retorna uma lista de palavras.
A função usa o método split() para dividir o texto em palavras, usando o espaço como separador.
Em seguida, a função itera sobre as palavras e remove as palavras vazias.
Por fim, a função retorna a lista de palavras.
*/

```