Claro! Aqui está um exemplo de código em Julia que utiliza a biblioteca Plots para criar um gráfico 3D interativo de uma função matemática:

```julia
using Plots

function plot_3d_function()
    # Define a função matemática
    f(x, y) = sin(sqrt(x^2 + y^2)) / sqrt(x^2 + y^2)

    # Define o intervalo para os pontos x e y
    x_range = -5:0.1:5
    y_range = -5:0.1:5

    # Cria uma matriz vazia para armazenar os valores da função
    z_values = zeros(length(x_range), length(y_range))

    # Calcula a função para cada par de pontos (x, y)
    for i in 1:length(x_range)
        for j in 1:length(y_range)
            z_values[i, j] = f(x_range[i], y_range[j])
        end
    end

    # Cria o gráfico 3D interativo
    plot(x_range, y_range, z_values, st = :surface, title = "Gráfico 3D de uma função", xlabel = "x", ylabel = "y", zlabel = "f(x, y)")
end

plot_3d_function()
```

Neste código, a função `plot_3d_function` utiliza a biblioteca Plots para criar um gráfico 3D interativo de uma função matemática. A função matemática utilizada é `f(x, y) = sin(sqrt(x^2 + y^2)) / sqrt(x^2 + y^2)`, que é uma função simples que descreve uma superfície em forma de ondas. 

Em seguida, definimos os intervalos para os pontos `x` e `y`, que vão de -5 a 5 com um passo de 0.1. Em seguida, criamos uma matriz vazia `z_values` para armazenar os valores da função. 

Dentro do loop `for`, calculamos o valor da função para cada par de pontos `(x, y)` e armazenamos na matriz `z_values`. 

Por fim, utilizamos a função `plot` da biblioteca Plots para criar o gráfico 3D interativo. Passamos os intervalos `x_range` e `y_range` como argumentos para definir a escala do gráfico e os valores da função `z_values` como argumento para desenhar a superfície. Também adicionamos um título ao gráfico e rótulos nos eixos x, y e z.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.