Claro! Aqui está um código complexo em Shell que cria uma matriz de números aleatórios e, em seguida, realiza a soma dos elementos em cada linha e coluna:

```shell
#!/bin/bash

# Definir o tamanho da matriz
linhas=5
colunas=5

# Função para gerar um número aleatório entre 1 e 10
gerar_numero_aleatorio() {
    echo $(( (RANDOM % 10) + 1 ))
}

# Inicializar a matriz com zeros
for ((i=0; i<linhas; i++)); do
    for ((j=0; j<colunas; j++)); do
        matriz[$i,$j]=0
    done
done

# Preencher a matriz com números aleatórios
for ((i=0; i<linhas; i++)); do
    for ((j=0; j<colunas; j++)); do
        matriz[$i,$j]=$(gerar_numero_aleatorio)
    done
done

# Imprimir a matriz
echo "Matriz gerada:"
for ((i=0; i<linhas; i++)); do
    for ((j=0; j<colunas; j++)); do
        echo -n "${matriz[$i,$j]} "
    done
    echo
done

# Calcular a soma das linhas
echo "Soma das linhas:"
for ((i=0; i<linhas; i++)); do
    soma_linha=0
    for ((j=0; j<colunas; j++)); do
        soma_linha=$((soma_linha + ${matriz[$i,$j]}))
    done
    echo "Linha $((i+1)): $soma_linha"
done

# Calcular a soma das colunas
echo "Soma das colunas:"
for ((j=0; j<colunas; j++)); do
    soma_coluna=0
    for ((i=0; i<linhas; i++)); do
        soma_coluna=$((soma_coluna + ${matriz[$i,$j]}))
    done
    echo "Coluna $((j+1)): $soma_coluna"
done
```

Explicação do código:

1. Definimos o tamanho da matriz com as variáveis `linhas` e `colunas`.
2. A função `gerar_numero_aleatorio` utiliza o comando `RANDOM` para gerar um número aleatório entre 1 e 10.
3. Inicializamos a matriz com zeros usando dois loops `for`.
4. Preenchemos a matriz com números aleatórios usando novamente dois loops `for` e chamando a função `gerar_numero_aleatorio`.
5. Imprimimos a matriz gerada usando loops `for` aninhados.
6. Calculamos a soma das linhas usando dois loops `for`. Inicialmente, definimos a variável `soma_linha` como zero e, em seguida, somamos os elementos de cada linha.
7. Imprimimos a soma das linhas.
8. Calculamos a soma das colunas usando dois loops `for`. Inicialmente, definimos a variável `soma_coluna` como zero e, em seguida, somamos os elementos de cada coluna.
9. Imprimimos a soma das colunas.

Esse código cria uma matriz aleatória e realiza cálculos com as somas das linhas e colunas. É um código relativamente complexo e pode ser adaptado para diferentes tamanhos de matriz.