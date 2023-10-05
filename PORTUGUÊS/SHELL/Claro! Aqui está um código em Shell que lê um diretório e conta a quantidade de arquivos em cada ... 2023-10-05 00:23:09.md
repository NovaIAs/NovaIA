Claro! Aqui está um código em Shell que lê um diretório e conta a quantidade de arquivos em cada subdiretório. O código utiliza recursividade para percorrer todos os subdiretórios e exibe o resultado final.

```shell
#!/bin/bash

# Função recursiva para contar arquivos em um diretório
contar_arquivos() {
  local diretorio=$1
  local contador=0

  # Percorre todos os arquivos e subdiretórios no diretório atual
  for item in "$diretorio"/*; do
    if [ -d "$item" ]; then
      # Se for um diretório, chama a função recursivamente
      contador=$((contador + $(contar_arquivos "$item")))
    elif [ -f "$item" ]; then
      # Se for um arquivo, incrementa o contador
      contador=$((contador + 1))
    fi
  done

  # Exibe o resultado para o diretório atual
  echo "Diretório '$diretorio': $contador arquivos"

  # Retorna o contador para a chamada recursiva
  echo $contador
}

# Diretório inicial a ser verificado
diretorio_inicial="/caminho/do/diretorio"

# Chama a função inicial para contar os arquivos
contar_arquivos "$diretorio_inicial"
```

Explicação do código:
- A função `contar_arquivos` é uma função recursiva que recebe um diretório como parâmetro.
- A função percorre todos os itens (arquivos e subdiretórios) no diretório atual usando um loop `for`.
- Se o item for um diretório (`-d`), a função chama a si mesma recursivamente com o subdiretório como parâmetro, incrementando o contador com o resultado retornado pela chamada recursiva.
- Se o item for um arquivo (`-f`), o contador é incrementado em 1.
- Após percorrer todos os itens no diretório atual, a função exibe o resultado do contador para o diretório atual.
- A função retorna o contador para a chamada recursiva.
- No final do código, você pode definir o diretório inicial a ser verificado na variável `diretorio_inicial`.
- O código chama a função `contar_arquivos` passando o diretório inicial como parâmetro para iniciar a contagem de arquivos.