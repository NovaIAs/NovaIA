Claro! Aqui está um exemplo de código complexo em Julia que utiliza a biblioteca Plots para gerar um gráfico interativo de uma função matemática:

```julia
using Plots

function gerar_grafico()
    # Definir os parâmetros da função
    amplitude = 1.0
    frequencia = 1.0
    fase = 0.0
    
    # Definir o intervalo de valores para x
    x = range(0, stop=2π, length=100)
    
    # Calcular os valores da função
    y = amplitude * sin.(frequencia .* x .+ fase)
    
    # Configurar o gráfico
    gr()
    plot(x, y, color=:blue, linewidth=2, label="Função")
    title!("Gráfico de uma função senoidal")
    xlabel!("x")
    ylabel!("y")
    
    # Adicionar interatividade ao gráfico
    plot!([x[1]], [y[1]], markershape=:circle, markersize=6, markercolor=:red, label="Ponto Inicial")
    scatter!([x[end]], [y[end]], markershape=:square, markersize=6, markercolor=:green, label="Ponto Final")
    
    # Mostrar o gráfico
    display(plot)
end

# Chamar a função para gerar o gráfico
gerar_grafico()
```

Neste código, primeiro importamos a biblioteca Plots usando o comando `using Plots`. Em seguida, definimos uma função chamada `gerar_grafico` que será responsável por gerar o gráfico. 

Dentro da função, definimos os parâmetros da função senoidal, como a amplitude, frequência e fase. Em seguida, criamos um vetor `x` utilizando a função `range` que contém os valores de x no intervalo de 0 a 2π com 100 pontos.

Depois, calculamos os valores da função senoidal utilizando a função `sin.` que permite calcular o seno de cada elemento do vetor `x`. Multiplicamos a frequência por `x` e adicionamos a fase para obter os valores corretos.

Em seguida, utilizamos a função `gr()` para configurar o backend de plotagem. Utilizamos a função `plot` para criar o gráfico, passando os vetores `x` e `y` como argumentos. Configuramos a cor para azul, a largura da linha para 2 e adicionamos uma etiqueta para a legenda.

Também adicionamos um título ao gráfico utilizando a função `title!`. Utilizamos as funções `xlabel!` e `ylabel!` para adicionar rótulos aos eixos x e y, respectivamente.

Para adicionar interatividade ao gráfico, utilizamos as funções `plot!` e `scatter!` para adicionar pontos de destaque no gráfico. Por fim, utilizamos a função `display(plot)` para mostrar o gráfico na saída.

Espero que este código complexo atenda às suas expectativas!