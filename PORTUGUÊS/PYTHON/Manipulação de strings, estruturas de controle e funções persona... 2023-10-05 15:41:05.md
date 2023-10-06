Claro! Aqui está um exemplo de código complexo em Python que envolve manipulação de strings, estruturas de controle e funções personalizadas. Este código tem como objetivo receber uma frase como entrada e realizar uma série de operações para gerar uma nova frase modificada.

```python
def contar_vogais(frase):
    vogais = ['a', 'e', 'i', 'o', 'u']
    contador = 0
    for letra in frase:
        if letra.lower() in vogais:
            contador += 1
    return contador

def inverter_palavras(frase):
    palavras = frase.split()
    palavras_invertidas = [palavra[::-1] for palavra in palavras]
    return ' '.join(palavras_invertidas)

def substituir_letras(frase):
    substituicoes = {'a': '@', 'e': '3', 'i': '!', 'o': '0'}
    nova_frase = ''
    for letra in frase:
        if letra.lower() in substituicoes:
            nova_frase += substituicoes[letra.lower()]
        else:
            nova_frase += letra
    return nova_frase

def main():
    frase = input("Digite uma frase: ")
    vogais = contar_vogais(frase)
    frase_invertida = inverter_palavras(frase)
    frase_substituida = substituir_letras(frase)
    
    print("Número de vogais na frase:", vogais)
    print("Frase com palavras invertidas:", frase_invertida)
    print("Frase com letras substituídas:", frase_substituida)

if __name__ == "__main__":
    main()
```

Este código começa definindo três funções personalizadas:

1. `contar_vogais`: recebe uma frase como entrada e conta o número de vogais presentes nela. Para isso, itera sobre cada letra da frase e verifica se ela é uma vogal (considerando letras maiúsculas e minúsculas). Retorna o contador de vogais encontrado.
2. `inverter_palavras`: recebe uma frase como entrada e inverte a ordem das palavras. Primeiro, a frase é dividida em palavras individuais usando o método `split()`. Em seguida, cada palavra é invertida usando a sintaxe de fatiamento de strings (`[::-1]`). Finalmente, as palavras invertidas são unidas novamente em uma única frase usando o método `join()`.
3. `substituir_letras`: recebe uma frase como entrada e substitui algumas letras por símbolos especiais. As substituições são definidas em um dicionário, onde as chaves representam as letras a serem substituídas e os valores representam os símbolos a serem usados como substituição. A função itera sobre cada letra da frase e verifica se ela está presente no dicionário de substituições. Se estiver, a letra é substituída pelo valor correspondente. Caso contrário, a letra é mantida como está.

A função `main()` é a função principal que será executada quando o programa for iniciado. Ela solicita ao usuário que digite uma frase e chama as três funções mencionadas acima para realizar as operações desejadas. Em seguida, imprime na tela o número de vogais na frase original, a frase com as palavras invertidas e a frase com as letras substituídas.

Esse código é apenas um exemplo de como um código complexo em Python pode ser elaborado. Você pode adaptá-lo e adicionar mais funcionalidades de acordo com suas necessidades.
