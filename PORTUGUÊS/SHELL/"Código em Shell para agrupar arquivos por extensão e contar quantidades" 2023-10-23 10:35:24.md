Claro! Aqui está um exemplo de um código em Shell bastante complexo e diferenciado:

```shell
#!/bin/bash

# Este script em Shell irá analisar um diretório específico e listar todos os arquivos nele, 
# agrupando-os por extensão em pastas separadas e contando o número de arquivos em cada uma delas.

# Definindo o diretório a ser analisado
diretorio="/caminho/do/diretorio"

# Criando uma função para criar pastas para cada extensão encontrada
criar_pasta_extensao() {
    extensao=$1
    pasta_extensao="${diretorio}/${extensao}_files"
    if [[ ! -d $pasta_extensao ]]; then
        mkdir "$pasta_extensao"
    fi
}

# Percorrendo todos os arquivos no diretório
for arquivo in "$diretorio"/*; do
    if [[ -f $arquivo ]]; then
        # Obtendo a extensão do arquivo
        extensao="${arquivo##*.}"
        
        # Criando uma pasta para a extensão do arquivo, se ainda não existir
        criar_pasta_extensao "$extensao"
        
        # Movendo o arquivo para a pasta da extensão correspondente
        mv "$arquivo" "${diretorio}/${extensao}_files/"
    fi
done

# Listando as pastas criadas e contando o número de arquivos em cada uma delas
for pasta in "$diretorio"/*_files; do
    if [[ -d $pasta ]]; then
        qtd_arquivos=$(ls -l "$pasta" | grep -v "total" | wc -l)
        extensao="${pasta##*/}"
        echo "A pasta ${extensao} contém ${qtd_arquivos} arquivo(s)."
    fi
done
```

Explicação do código:
1. Primeiro, definimos o diretório a ser analisado na variável `diretorio`.
2. Em seguida, criamos a função `criar_pasta_extensao` para criar uma pasta para cada extensão encontrada.
3. Dentro do loop `for`, percorremos todos os arquivos no diretório especificado.
4. Verificamos se o arquivo é um arquivo regular usando o comando `[[ -f $arquivo ]]`.
5. Em seguida, obtemos a extensão do arquivo usando a expressão `${arquivo##*.}`.
6. Chamamos a função `criar_pasta_extensao` passando a extensão como parâmetro.
7. Movemos o arquivo para a pasta da extensão correspondente usando o comando `mv`.
8. No segundo loop `for`, percorremos todas as pastas criadas.
9. Verificamos se a pasta é um diretório usando o comando `[[ -d $pasta ]]`.
10. Usamos o comando `ls -l` para listar os arquivos na pasta.
11. Usamos o comando `grep -v "total"` para excluir a primeira linha que mostra o total de arquivos.
12. Usamos o comando `wc -l` para contar o número de linhas, ou seja, o número de arquivos na pasta.
13. Imprimimos na tela o nome da pasta e a quantidade de arquivos usando o comando `echo`.

Espero que este código complexo e explicado em detalhes seja útil para você!