Claro! Aqui está um código em Julia que realiza uma análise estatística de um conjunto de dados:

```julia
# Importando o pacote Statistics
using Statistics

# Função para calcular a média
function calcularMedia(dados)
    return mean(dados)
end

# Função para calcular a mediana
function calcularMediana(dados)
    return median(dados)
end

# Função para calcular a moda
function calcularModa(dados)
    moda = countmap(dados)
    moda_values = values(moda)
    moda_keys = collect(keys(moda))
    return moda_keys[argmax(moda_values)]
end

# Função para calcular a variância
function calcularVariancia(dados)
    return var(dados)
end

# Função para calcular o desvio padrão
function calcularDesvioPadrao(dados)
    return std(dados)
end

# Função para calcular o coeficiente de correlação de Pearson
function calcularCoeficienteCorrelacao(dados1, dados2)
    return cor(dados1, dados2)
end

# Função principal
function main()
    # Dados de exemplo
    dados = [10, 15, 20, 25, 30, 35, 40, 45, 50]

    # Cálculo da média
    media = calcularMedia(dados)
    println("A média dos dados é: ", media)

    # Cálculo da mediana
    mediana = calcularMediana(dados)
    println("A mediana dos dados é: ", mediana)

    # Cálculo da moda
    moda = calcularModa(dados)
    println("A moda dos dados é: ", moda)

    # Cálculo da variância
    variancia = calcularVariancia(dados)
    println("A variância dos dados é: ", variancia)

    # Cálculo do desvio padrão
    desvioPadrao = calcularDesvioPadrao(dados)
    println("O desvio padrão dos dados é: ", desvioPadrao)

    # Dados de exemplo para o coeficiente de correlação
    dados1 = [1, 2, 3, 4, 5]
    dados2 = [10, 20, 30, 40, 50]

    # Cálculo do coeficiente de correlação
    coeficienteCorrelacao = calcularCoeficienteCorrelacao(dados1, dados2)
    println("O coeficiente de correlação entre os dados é: ", coeficienteCorrelacao)
end

# Chamada da função principal
main()
```

Neste código, começamos importando o pacote Statistics, que fornece funções estatísticas úteis. Em seguida, definimos uma série de funções para calcular estatísticas básicas, como média, mediana, moda, variância, desvio padrão e coeficiente de correlação de Pearson.

A função principal `main()` contém um conjunto de dados de exemplo e chama cada uma das funções para calcular e imprimir as estatísticas correspondentes.

Você pode ajustar os dados de entrada e adicionar mais funcionalidades conforme necessário. Este código de exemplo pode ser um ponto de partida para análises estatísticas mais complexas em Julia.