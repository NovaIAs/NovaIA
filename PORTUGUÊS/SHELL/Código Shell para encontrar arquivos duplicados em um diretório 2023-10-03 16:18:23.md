Claro! Aqui está um exemplo de um código em Shell que realiza uma tarefa complexa: encontrar arquivos duplicados em um diretório e exibir uma lista deles.

```shell
#!/bin/bash

# Define o diretório a ser verificado
diretorio="/caminho/do/diretorio"

# Cria um array para armazenar os arquivos duplicados
declare -A arquivos_duplicados

# Percorre todos os arquivos no diretório e subdiretórios
while IFS= read -rd '' arquivo; do
    # Calcula o hash MD5 do arquivo
    hash=$(md5sum "$arquivo" | awk '{ print $1 }')

    # Verifica se o hash já existe no array de arquivos duplicados
    if [[ -n "${arquivos_duplicados[$hash]}" ]]; then
        # Adiciona o arquivo duplicado ao array
        arquivos_duplicados[$hash]+=",$arquivo"
    else
        # Adiciona o hash do arquivo ao array de arquivos duplicados
        arquivos_duplicados[$hash]=$arquivo
    fi
done < <(find "$diretorio" -type f -print0)

# Exibe a lista de arquivos duplicados
for hash in "${!arquivos_duplicados[@]}"; do
    # Verifica se existem mais de um arquivo com o mesmo hash
    if [[ $(grep -o "," <<<"${arquivos_duplicados[$hash]}") ]]; then
        echo "Arquivos duplicados com o hash $hash:"
        echo "${arquivos_duplicados[$hash]}"
        echo
    fi
done
```

Explicação do código:
1. Na primeira linha, `#!/bin/bash`, indicamos que o script deve ser interpretado pelo bash.
2. Em seguida, definimos a variável `diretorio` como o caminho do diretório que queremos verificar.
3. Criamos um array associativo chamado `arquivos_duplicados` para armazenar os arquivos duplicados encontrados.
4. Em um loop, percorremos todos os arquivos no diretório e subdiretórios usando o comando `find`.
5. Para cada arquivo, calculamos o hash MD5 usando o comando `md5sum` e extraímos apenas o hash utilizando o comando `awk`.
6. Verificamos se o hash já existe no array `arquivos_duplicados`. Se existir, adicionamos o arquivo duplicado separado por vírgula. Se não, adicionamos o hash do arquivo como chave no array.
7. Após percorrer todos os arquivos, fazemos outro loop para exibir a lista de arquivos duplicados.
8. Verificamos se existe mais de um arquivo com o mesmo hash, verificando se há uma vírgula no valor do array `arquivos_duplicados`. Se houver, exibimos a mensagem "Arquivos duplicados com o hash" seguida do hash e, em seguida, exibimos a lista de arquivos separados por vírgula.
9. O resultado final será uma lista dos arquivos duplicados encontrados no diretório especificado.

Espero que esse código atenda às suas necessidades!