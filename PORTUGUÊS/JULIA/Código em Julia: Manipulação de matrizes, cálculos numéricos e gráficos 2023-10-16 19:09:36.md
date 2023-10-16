Claro! Aqui está um código complexo em Julia que envolve manipulação de matrizes, cálculos numéricos e gráficos:

```julia
# Definindo a função para calcular o somatório de uma série
function calcular_somatorio(n::Int)
    soma = 0.0
    for i in 1:n
        soma += i^2 / factorial(i)
    end
    return soma
end

# Definindo a função para gerar uma matriz triangular inferior
function gerar_matriz_triangular(n::Int)
    matriz = Matrix{Float64}(undef, n, n)
    for i in 1:n
        for j in 1:i
            matriz[i, j] = i + j
        end
    end
    return matriz
end

# Definindo a função para plotar um gráfico
function plotar_grafico(x, y)
    using Plots
    plot(x, y, marker = :circle, linestyle = :solid, linewidth = 2, xlabel = "x", ylabel = "y", title = "Gráfico")
end

# Definindo o número de termos da série
n = 10

# Calculando o somatório da série
resultado = calcular_somatorio(n)
println("O resultado do somatório é: ", resultado)

# Gerando uma matriz triangular inferior de tamanho n
matriz_triangular = gerar_matriz_triangular(n)
println("A matriz triangular inferior gerada é:")
println(matriz_triangular)

# Gerando um vetor de valores para x
x = collect(1:n)

# Calculando os valores de y a partir de uma função
y = sin.(x) + 2 * cos.(x)

# Plotando o gráfico
plotar_grafico(x, y)
```

Neste código em Julia, temos três funções principais:

1. A função `calcular_somatorio` recebe um número inteiro `n` como entrada e calcula o somatório de uma série utilizando um loop `for`. A cada iteração, o termo `i^2 / factorial(i)` é adicionado à variável `soma`. O resultado do somatório é retornado.

2. A função `gerar_matriz_triangular` recebe um número inteiro `n` como entrada e gera uma matriz triangular inferior de tamanho `n`. A matriz é inicializada com valores indefinidos (`undef`) e, em seguida, os elementos abaixo e na diagonal principal são preenchidos com valores correspondentes à soma dos índices.

3. A função `plotar_grafico` recebe dois vetores `x` e `y` como entrada e utiliza a biblioteca `Plots` para gerar um gráfico com os pontos `(x[i], y[i])`. O gráfico é personalizado com marcadores, estilo de linha, rótulos nos eixos e título.

Além das funções, o código também inclui a definição do número de termos da série (`n`), o cálculo do somatório utilizando a função `calcular_somatorio`, a impressão do resultado, a geração de uma matriz triangular inferior utilizando a função `gerar_matriz_triangular`, a impressão da matriz gerada e, por fim, a criação de um vetor de valores para `x` e a geração de um vetor de valores para `y` utilizando funções matemáticas (`sin` e `cos`). O gráfico é então plotado utilizando a função `plotar_grafico`.