Claro! Aqui está um código complexo em R que utiliza algoritmos genéticos para resolver o problema do caixeiro viajante. O caixeiro viajante é um problema clássico de otimização combinatória, que consiste em encontrar o menor caminho possível que um viajante deve percorrer para visitar todas as cidades em um conjunto dado, retornando à cidade de origem.

```
# Definindo as cidades e suas coordenadas
cidades <- data.frame(
  cidade = c("A", "B", "C", "D", "E"),
  x = c(2, 5, 8, 6, 3),
  y = c(1, 6, 3, 2, 5)
)

# Função para calcular a distância entre duas cidades
distancia <- function(cidade1, cidade2) {
  sqrt((cidade2$x - cidade1$x)^2 + (cidade2$y - cidade1$y)^2)
}

# Função objetiva: calcular o comprimento total do caminho
comprimento_caminho <- function(caminho) {
  total <- 0
  for (i in 2:length(caminho)) {
    total <- total + distancia(cidades[caminho[i-1], ], cidades[caminho[i], ])
  }
  total <- total + distancia(cidades[caminho[length(caminho)], ], cidades[caminho[1], ])
  return(total)
}

# Função para gerar uma população inicial aleatória
gerar_populacao_inicial <- function(num_individuos) {
  populacao <- list()
  for (i in 1:num_individuos) {
    caminho <- sample(1:nrow(cidades))
    populacao[[i]] <- caminho
  }
  return(populacao)
}

# Função para realizar o crossover entre dois indivíduos
crossover <- function(individuo1, individuo2) {
  ponto_corte <- sample(2:(length(individuo1)-1), 1)
  novo_individuo <- c(individuo1[1:ponto_corte], individuo2[(ponto_corte+1):length(individuo2)])
  return(novo_individuo)
}

# Função para realizar a mutação em um indivíduo
mutacao <- function(individuo, taxa_mutacao) {
  if (runif(1) < taxa_mutacao) {
    pontos_mutacao <- sample(2:(length(individuo)-1), 2)
    individuo[pontos_mutacao] <- individuo[c(pontos_mutacao[2], pontos_mutacao[1])]
  }
  return(individuo)
}

# Função para selecionar dois indivíduos utilizando o método da roleta viciada
selecao_roleta <- function(pontuacoes) {
  total <- sum(pontuacoes)
  proporcoes <- pontuacoes / total
  cum_sum <- cumsum(proporcoes)
  r <- runif(1)
  for (i in 1:length(cum_sum)) {
    if (r <= cum_sum[i]) {
      return(i)
    }
  }
}

# Função principal para resolver o problema do caixeiro viajante utilizando algoritmos genéticos
resolver_caixeiro_viajante <- function(num_individuos, num_geracoes, taxa_mutacao) {
  populacao <- gerar_populacao_inicial(num_individuos)
  melhor_caminho <- NULL
  melhor_comprimento <- Inf
  
  for (geracao in 1:num_geracoes) {
    pontuacoes <- rep(0, num_individuos)
    
    # Avaliar o desempenho de cada indivíduo
    for (i in 1:num_individuos) {
      pontuacoes[i] <- 1 / comprimento_caminho(populacao[[i]])
      
      # Verificar se o indivíduo atual é o melhor até o momento
      if (pontuacoes[i] > 1 / melhor_comprimento) {
        melhor_caminho <- populacao[[i]]
        melhor_comprimento <- comprimento_caminho(melhor_caminho)
      }
    }
    
    nova_populacao <- list()
    
    # Criar a nova população utilizando seleção, crossover e mutação
    for (i in 1:num_individuos) {
      pai1 <- selecao_roleta(pontuacoes)
      pai2 <- selecao_roleta(pontuacoes)
      
      filho <- crossover(populacao[[pai1]], populacao[[pai2]])
      filho <- mutacao(filho, taxa_mutacao)
      
      nova_populacao[[i]] <- filho
    }
    
    populacao <- nova_populacao
  }
  
  return(melhor_caminho)
}

# Chamando a função principal para resolver o problema do caixeiro viajante
melhor_caminho <- resolver_caixeiro_viajante(num_individuos = 100, num_geracoes = 1000, taxa_mutacao = 0.01)

# Imprimindo o melhor caminho encontrado
print(melhor_caminho)
```

Este código utiliza algoritmos genéticos para gerar uma população inicial aleatória de indivíduos, onde cada indivíduo é representado por uma permutação das cidades. Em seguida, ele avalia o desempenho de cada indivíduo, utilizando a função de avaliação `comprimento_caminho`, que calcula o comprimento total do caminho percorrido pelo viajante. 

Em seguida, ele seleciona dois indivíduos para realizar o crossover, que é a combinação de partes dos dois indivíduos para gerar um novo indivíduo. Além disso, há também uma taxa de mutação para garantir a diversidade genética da população, onde ocorrem trocas aleatórias entre as posições das cidades no caminho.

Este processo é repetido por um número determinado de gerações, até que se obtenha um resultado satisfatório. No final, o código imprime o melhor caminho encontrado.