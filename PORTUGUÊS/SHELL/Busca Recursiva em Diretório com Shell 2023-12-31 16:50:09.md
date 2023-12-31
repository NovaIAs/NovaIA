Claro! Aqui está um código em Shell que realiza uma busca recursiva em um diretório específico, listando todos os arquivos e diretórios encontrados:

```bash
#!/bin/bash

# Função recursiva para percorrer diretórios
function percorrer_diretorios() {
    local diretorio=$1
    
    # Percorre todos os arquivos e diretórios no diretório atual
    for arquivo in "$diretorio"/*; do
        # Verifica se o arquivo é um diretório
        if [ -d "$arquivo" ]; then
            echo "Diretório: $arquivo"
            
            # Chama a função recursivamente para o diretório encontrado
            percorrer_diretorios "$arquivo"
        else
            echo "Arquivo: $arquivo"
        fi
    done
}

# Diretório inicial
diretorio_inicial="/caminho/do/diretorio"

# Chama a função para percorrer os diretórios
percorrer_diretorios "$diretorio_inicial"
```

Explicação do código:

1. A primeira linha `#!/bin/bash` define que o interpretador a ser usado é o bash.

2. A função `percorrer_diretorios` é definida para percorrer recursivamente os diretórios.

3. Dentro da função, a variável `diretorio` recebe o diretório atual a ser percorrido.

4. Utilizando um loop `for`, o código percorre todos os arquivos e diretórios dentro do diretório atual.

5. É verificado se o arquivo é um diretório utilizando o comando `[ -d "$arquivo" ]`.

6. Se for um diretório, o nome do diretório é impresso na tela e a função `percorrer_diretorios` é chamada recursivamente para explorar o diretório encontrado.

7. Se não for um diretório, o nome do arquivo é impresso na tela.

8. No final do código, é definido o diretório inicial a ser explorado, atribuindo o caminho para a variável `diretorio_inicial`.

9. Por fim, a função `percorrer_diretorios` é chamada com o diretório inicial como argumento. Isso inicia a busca recursiva.