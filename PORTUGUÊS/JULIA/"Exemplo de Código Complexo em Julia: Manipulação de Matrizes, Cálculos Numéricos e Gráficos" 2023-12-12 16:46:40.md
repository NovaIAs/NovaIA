Claro! Aqui está um exemplo de código complexo em Julia que envolve manipulação de matrizes, cálculos numéricos e gráficos:

```julia
# Função para calcular o produto de duas matrizes
function produto_matricial(A::Matrix{Float64}, B::Matrix{Float64})
    m, n = size(A)
    p, q = size(B)
    
    if n != p
        error("As dimensões das matrizes não são compatíveis!")
    end
    
    C = zeros(Float64, m, q)
    
    for i in 1:m
        for j in 1:q
            for k in 1:n
                C[i, j] += A[i, k] * B[k, j]
            end
        end
    end
    
    return C
end

# Criação de matrizes aleatórias
A = rand(1:10, 3, 4)
B = rand(1:10, 4, 2)

# Cálculo do produto matricial
C = produto_matricial(A, B)

# Impressão das matrizes
println("Matriz A:")
println(A)
println("Matriz B:")
println(B)
println("Produto matricial C = A * B:")
println(C)

# Criação de um vetor de valores x
x = range(-2π, stop=2π, length=100)

# Cálculo da função seno e cosseno de x
y1 = sin.(x)
y2 = cos.(x)

# Criação do gráfico
using Plots
gr()
plot(x, y1, label="sen(x)", linewidth=2)
plot!(x, y2, label="cos(x)", linewidth=2)
title!("Gráfico das funções seno e cosseno")
xlabel!("x")
ylabel!("y")
legend!(:bottomright)
```

Neste código em Julia, temos uma função chamada `produto_matricial` que calcula o produto de duas matrizes. Ela recebe duas matrizes `A` e `B` como argumentos e retorna a matriz resultante `C`. O código verifica se as dimensões das matrizes são compatíveis e, em seguida, realiza o cálculo do produto matricial usando três loops aninhados.

Em seguida, o código cria duas matrizes aleatórias `A` e `B` e calcula o produto matricial `C = A * B`. Depois, imprime as matrizes e o resultado do produto matricial.

Por fim, o código cria um vetor de valores `x` usando a função `range` e calcula as funções seno e cosseno de `x`. Ele utiliza a biblioteca `Plots` para criar um gráfico das funções seno e cosseno, personalizando o título, rótulos dos eixos e legenda.

Você pode executar este código em um ambiente Julia para ver os resultados e o gráfico gerado.