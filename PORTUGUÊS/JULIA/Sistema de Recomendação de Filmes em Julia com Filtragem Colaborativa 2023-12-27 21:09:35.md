Claro! Aqui está um código complexo em Julia que cria um sistema de recomendação de filmes baseado em filtragem colaborativa:

```julia
# Importar bibliotecas necessárias
using DelimitedFiles
using LinearAlgebra

# Carregar dados de avaliação de filmes
avaliacoes = readdlm("avaliacoes.csv", ',', skipstart=1)

# Definir função de similaridade de cosseno
function similaridade_cosseno(v1, v2)
    return dot(v1, v2) / (norm(v1) * norm(v2))
end

# Definir função de recomendação de filmes
function recomendar_filmes(avaliacoes, usuario, n)
    # Criar dicionário para armazenar similaridade entre usuários
    similaridades = Dict{Int, Float64}()
    
    # Calcular similaridade entre o usuário atual e todos os outros usuários
    for i in 1:size(avaliacoes, 1)
        if i != usuario
            similaridade = similaridade_cosseno(avaliacoes[usuario, :], avaliacoes[i, :])
            similaridades[i] = similaridade
        end
    end
    
    # Ordenar usuários por similaridade em ordem decrescente
    usuarios_ordenados = sort(collect(similaridades), by=x->x[2], rev=true)
    
    # Inicializar dicionário para armazenar pontuações de filmes recomendados
    pontuacoes_recomendadas = Dict{Int, Float64}()
    
    # Calcular pontuações de filmes recomendados com base nas avaliações dos usuários mais similares
    for i in 1:n
        usuario_similar = usuarios_ordenados[i][1]
        similaridade = usuarios_ordenados[i][2]
        
        for j in 1:size(avaliacoes, 2)
            if avaliacoes[usuario, j] == 0 && avaliacoes[usuario_similar, j] > 0
                pontuacao_atual = get(pontuacoes_recomendadas, j, 0.0)
                pontuacoes_recomendadas[j] = pontuacao_atual + avaliacoes[usuario_similar, j] * similaridade
            end
        end
    end
    
    # Ordenar filmes recomendados por pontuação em ordem decrescente
    filmes_recomendados = sort(collect(pontuacoes_recomendadas), by=x->x[2], rev=true)
    
    # Imprimir os n melhores filmes recomendados
    println("Os $n melhores filmes recomendados para o usuário $usuario são:")
    for i in 1:n
        filme = filmes_recomendados[i][1]
        pontuacao = filmes_recomendados[i][2]
        println("Filme $filme - Pontuação: $pontuacao")
    end
end

# Exemplo de uso
recomendar_filmes(avaliacoes, 1, 5)
```

Neste código em Julia, primeiro importamos as bibliotecas necessárias para lidar com arquivos CSV e realizar operações de álgebra linear. Em seguida, carregamos os dados de avaliação de filmes a partir de um arquivo CSV.

Em seguida, definimos a função `similaridade_cosseno` que calcula a similaridade de cosseno entre dois vetores de avaliações de filmes. Essa função utiliza o produto escalar e a norma dos vetores para calcular o valor da similaridade.

A função `recomendar_filmes` recebe como entrada a matriz de avaliações de filmes, o número do usuário para o qual queremos fazer a recomendação e o número de filmes a serem recomendados. Essa função implementa o algoritmo de filtragem colaborativa, que calcula a similaridade entre o usuário atual e todos os outros usuários, ordena os usuários por similaridade e, em seguida, calcula as pontuações dos filmes recomendados com base nas avaliações dos usuários mais similares.

Por fim, temos um exemplo de uso da função `recomendar_filmes`, onde passamos a matriz de avaliações de filmes, o número do usuário (no caso, 1) e o número de filmes a serem recomendados (no caso, 5). Os melhores filmes recomendados são exibidos no console.

Esse código complexo em Julia demonstra um sistema de recomendação de filmes utilizando filtragem colaborativa, onde a similaridade entre os usuários é calculada e utilizada para recomendar filmes com base nas avaliações dos usuários mais similares.