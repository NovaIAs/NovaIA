Claro! Aqui está um exemplo de código complexo em Shell:

```
#!/bin/bash

# Este é um script em Shell que faz o download de imagens de um site e as redimensiona para diferentes tamanhos.

# Definindo as URLs das imagens que serão baixadas
urls=("https://site.com/imagem1.jpg" "https://site.com/imagem2.jpg" "https://site.com/imagem3.jpg")

# Função para redimensionar as imagens
redimensionar_imagem() {
    imagem=$1
    tamanho=$2
    imagem_redimensionada="imagem_redimensionada_$tamanho.jpg"

    # Utilizando o comando convert do ImageMagick para redimensionar a imagem
    convert $imagem -resize $tamanho $imagem_redimensionada

    echo "Imagem $imagem redimensionada para o tamanho $tamanho"
}

# Loop para baixar e redimensionar as imagens
for url in "${urls[@]}"; do
    # Obtendo o nome da imagem a partir da URL
    imagem=$(basename $url)

    # Baixando a imagem utilizando o comando curl
    curl -o $imagem $url

    echo "Imagem $imagem baixada"

    # Redimensionando a imagem para diferentes tamanhos
    redimensionar_imagem $imagem "200x200"
    redimensionar_imagem $imagem "400x400"
    redimensionar_imagem $imagem "800x800"
done
```

Neste exemplo, o script em Shell faz o download de três imagens de um site e as redimensiona para três tamanhos diferentes (200x200, 400x400 e 800x800). O código utiliza o comando `curl` para baixar as imagens e o comando `convert` do ImageMagick para redimensioná-las.

O script utiliza um array `urls` para armazenar as URLs das imagens que serão baixadas. Em seguida, ele itera sobre esse array e realiza as seguintes ações para cada URL:

1. Obtém o nome da imagem a partir da URL utilizando o comando `basename`.
2. Faz o download da imagem utilizando o comando `curl`.
3. Chama a função `redimensionar_imagem` três vezes, passando como parâmetro o nome da imagem e cada um dos tamanhos desejados. A função `redimensionar_imagem` utiliza o comando `convert` para redimensionar a imagem para o tamanho especificado.
4. Exibe uma mensagem informando que a imagem foi redimensionada.

Dessa forma, o script faz o download das imagens e as redimensiona para os tamanhos desejados, exibindo mensagens informativas ao longo do processo.