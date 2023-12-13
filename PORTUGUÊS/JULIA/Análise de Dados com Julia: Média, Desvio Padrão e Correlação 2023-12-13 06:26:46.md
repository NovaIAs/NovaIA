Claro! Abaixo está um código complexo em Julia que lida com manipulação de matrizes e cálculos estatísticos. Este código realiza uma análise de dados em um conjunto de amostras, calculando a média, o desvio padrão e a correlação entre duas variáveis.

```julia
# Definindo uma função para calcular a média dos valores em um vetor
function calcular_media(vetor)
    soma = sum(vetor)
    media = soma / length(vetor)
    return media
end

# Definindo uma função para calcular o desvio padrão dos valores em um vetor
function calcular_desvio_padrao(vetor)
    media = calcular_media(vetor)
    soma = sum((x - media)^2 for x in vetor)
    variancia = soma / (length(vetor) - 1)
    desvio_padrao = sqrt(variancia)
    return desvio_padrao
end

# Definindo uma função para calcular a correlação entre dois vetores
function calcular_correlacao(vetor1, vetor2)
    media1 = calcular_media(vetor1)
    media2 = calcular_media(vetor2)
    desvio_padrao1 = calcular_desvio_padrao(vetor1)
    desvio_padrao2 = calcular_desvio_padrao(vetor2)
    
    soma = sum((x - media1) * (y - media2) for (x, y) in zip(vetor1, vetor2))
    correlacao = soma / ((length(vetor1) - 1) * desvio_padrao1 * desvio_padrao2)
    return correlacao
end

# Definindo uma matriz de amostras
amostras = [
    [1, 2, 3, 4, 5],
    [2, 4, 6, 8, 10],
    [3, 6, 9, 12, 15]
]

# Calculando a média, o desvio padrão e a correlação para cada par de variáveis
resultados = []
for i in 1:size(amostras, 1)
    for j in (i + 1):size(amostras, 1)
        media = calcular_media(amostras[i])
        desvio_padrao = calcular_desvio_padrao(amostras[i])
        correlacao = calcular_correlacao(amostras[i], amostras[j])
        
        push!(resultados, (i, j, media, desvio_padrao, correlacao))
    end
end

# Imprimindo os resultados
for resultado in resultados
    println("Variáveis $(resultado[1]) e $(resultado[2]):")
    println("Média: $(resultado[3])")
    println("Desvio Padrão: $(resultado[4])")
    println("Correlação: $(resultado[5])")
    println()
end
```

Neste código, começamos definindo três funções: `calcular_media`, `calcular_desvio_padrao` e `calcular_correlacao`. A função `calcular_media` calcula a média dos valores em um vetor, a função `calcular_desvio_padrao` calcula o desvio padrão dos valores em um vetor e a função `calcular_correlacao` calcula a correlação entre dois vetores.

Em seguida, definimos uma matriz de amostras chamada `amostras`, que contém três vetores de amostras. A matriz `amostras` representa três variáveis diferentes.

Em seguida, usamos dois loops `for` para calcular a média, o desvio padrão e a correlação para cada par de variáveis. Armazenamos os resultados em uma lista chamada `resultados`.

Por fim, imprimimos os resultados na tela, exibindo a média, o desvio padrão e a correlação para cada par de variáveis.