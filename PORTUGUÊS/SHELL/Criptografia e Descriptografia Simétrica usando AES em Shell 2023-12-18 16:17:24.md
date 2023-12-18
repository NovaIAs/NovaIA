Claro! Aqui está um código complexo em Shell, que possui uma função de criptografia e descriptografia simétrica usando o algoritmo AES (Advanced Encryption Standard). O código foi escrito em português e possui comentários explicativos para facilitar a compreensão.

```shell
#!/bin/bash

# Função para criptografar uma mensagem usando AES
function criptografar() {
    mensagem=$1
    chave=$2

    # Gerar uma chave de 256 bits a partir da chave fornecida
    chave=$(echo -n "$chave" | sha256sum | cut -d ' ' -f1)

    # Gerar um vetor de inicialização aleatório
    iv=$(openssl rand -hex 16)

    # Criptografar a mensagem usando AES-256-CBC
    mensagem_criptografada=$(echo -n "$mensagem" | openssl enc -aes-256-cbc -K "$chave" -iv "$iv" -a)

    # Imprimir o resultado
    echo "Mensagem criptografada: $mensagem_criptografada"
}

# Função para descriptografar uma mensagem usando AES
function descriptografar() {
    mensagem_criptografada=$1
    chave=$2
    iv=$3

    # Gerar uma chave de 256 bits a partir da chave fornecida
    chave=$(echo -n "$chave" | sha256sum | cut -d ' ' -f1)

    # Descriptografar a mensagem usando AES-256-CBC
    mensagem_descriptografada=$(echo -n "$mensagem_criptografada" | openssl enc -d -aes-256-cbc -K "$chave" -iv "$iv" -a)

    # Imprimir o resultado
    echo "Mensagem descriptografada: $mensagem_descriptografada"
}

# Exemplo de uso: criptografar uma mensagem
mensagem="Olá, mundo!"
chave="minhachave"

echo "Mensagem original: $mensagem"
criptografar "$mensagem" "$chave"

# Exemplo de uso: descriptografar uma mensagem
mensagem_criptografada="U2FsdGVkX19t0blWjB4XdKdLZK9DdQe4T7Y7VNGj8Zg="
chave="minhachave"
iv="2f5a4a4d4c4b49484746454443424140"

echo "Mensagem criptografada: $mensagem_criptografada"
descriptografar "$mensagem_criptografada" "$chave" "$iv"
```

Neste código, criamos duas funções: `criptografar` e `descriptografar`. A função `criptografar` recebe uma mensagem e uma chave como parâmetros. Primeiro, a chave é transformada em uma chave de 256 bits usando a função hash SHA-256. Em seguida, um vetor de inicialização aleatório é gerado. A mensagem é então criptografada usando o algoritmo AES-256-CBC e a chave e vetor de inicialização gerados anteriormente. O resultado é impresso na tela.

A função `descriptografar` recebe uma mensagem criptografada, uma chave e um vetor de inicialização como parâmetros. A chave é novamente transformada em uma chave de 256 bits usando a função hash SHA-256. A mensagem é descriptografada usando o algoritmo AES-256-CBC e a chave e vetor de inicialização fornecidos. O resultado é impresso na tela.

No exemplo de uso fornecido no final do código, a mensagem "Olá, mundo!" é criptografada usando a chave "minhachave" e, em seguida, descriptografada usando a mesma chave e vetor de inicialização. Os resultados são impressos na tela.