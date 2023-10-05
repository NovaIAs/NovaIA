Claro! Aqui está um código em Shell que cria um diretório recursivamente com base em uma estrutura de pastas definida em um arquivo de texto:

```
#!/bin/bash

# Função para criar diretórios recursivamente
criar_diretorios() {
    local caminho=$1
    local nivel=$2
    local estrutura=$3

    # Verifica se o nível é maior que 0
    if [ $nivel -gt 0 ]; then
        # Percorre a estrutura de pastas
        while IFS= read -r pasta; do
            # Cria o diretório
            mkdir "$caminho/$pasta"

            # Chama a função recursivamente para criar os subdiretórios
            criar_diretorios "$caminho/$pasta" $((nivel - 1)) "$estrutura"
        done <<< "$estrutura"
    fi
}

# Caminho base onde os diretórios serão criados
caminho_base="/caminho/base"

# Nível máximo de profundidade dos diretórios
nivel_maximo=3

# Estrutura das pastas (cada linha representa uma pasta)
estrutura_pasta="pasta1
pasta2
pasta3
pasta4
pasta5"

# Chama a função para criar os diretórios
criar_diretorios "$caminho_base" $nivel_maximo "$estrutura_pasta"
```

Explicação:

1. A primeira linha `#!/bin/bash` indica que o script será executado usando o interpretador bash.
2. Em seguida, declaramos a função `criar_diretorios()` que recebe três parâmetros: o caminho onde os diretórios serão criados, o nível atual de profundidade e a estrutura de pastas.
3. Dentro da função, verificamos se o nível é maior que zero. Se for, percorremos a estrutura de pastas usando um loop `while` e, para cada pasta, criamos um diretório com o comando `mkdir`.
4. Em seguida, chamamos a função `criar_diretorios()` recursivamente para criar os subdiretórios dentro do diretório recém-criado.
5. Fora da função, definimos o caminho base onde os diretórios serão criados, o nível máximo de profundidade dos diretórios e a estrutura das pastas em uma variável de texto.
6. Por fim, chamamos a função `criar_diretorios()` passando os parâmetros necessários para iniciar a criação dos diretórios.

Este código em Shell cria uma estrutura de pastas de forma recursiva com base na estrutura definida no arquivo de texto. O nível máximo de profundidade dos diretórios pode ser ajustado conforme necessário.