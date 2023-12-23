Claro! Aqui está um código complexo em Julia que calcula a raiz quadrada de uma matriz utilizando o método de decomposição espectral:

```julia
# Função para calcular a raiz quadrada de uma matriz
function calcularRaizQuadrada(matriz)
    # Realiza a decomposição espectral da matriz
    autovalores, autovetores = eigen(matriz)
    
    # Calcula a raiz quadrada dos autovalores
    autovaloresRaiz = sqrt.(autovalores)
    
    # Monta a matriz de raiz quadrada
    matrizRaizQuadrada = autovetores * Diagonal(autovaloresRaiz) * adjoint(autovetores)
    
    return matrizRaizQuadrada
end

# Cria uma matriz de exemplo
matrizExemplo = [1 2 3; 4 5 6; 7 8 9]

# Chama a função para calcular a raiz quadrada da matriz de exemplo
matrizRaizQuadrada = calcularRaizQuadrada(matrizExemplo)

# Imprime a matriz de raiz quadrada
println("A matriz de raiz quadrada é:")
println(matrizRaizQuadrada)
```

Neste código, temos uma função chamada `calcularRaizQuadrada`, que recebe uma matriz como entrada e retorna a matriz resultante da raiz quadrada da matriz original. Para realizar esse cálculo, utilizamos a decomposição espectral da matriz, obtendo os autovalores e autovetores.

Em seguida, calculamos a raiz quadrada de cada autovalor e montamos a matriz de raiz quadrada utilizando os autovetores correspondentes. Por fim, retornamos a matriz resultante.

No exemplo, criamos uma matriz de exemplo `matrizExemplo` e chamamos a função `calcularRaizQuadrada` passando essa matriz como argumento. A matriz resultante é armazenada na variável `matrizRaizQuadrada` e, por fim, é impressa na tela.