Claro! Aqui está um código complexo em R que realiza um algoritmo genético para resolver o problema do Caixeiro Viajante:

```
# Função para calcular a distância entre duas cidades
calcularDistancia <- function(cidade1, cidade2) {
  # Implemente aqui a sua fórmula de cálculo de distância
  # Exemplo: sqrt((cidade1$x - cidade2$x)^2 + (cidade1$y - cidade2$y)^2)
}

# Função para calcular o fitness de um indivíduo (rota)
calcularFitness <- function(rota) {
  distanciaTotal <- 0
  for (i in 1:(length(rota) - 1)) {
    cidadeAtual <- rota[i]
    proximaCidade <- rota[i + 1]
    distanciaTotal <- distanciaTotal + calcularDistancia(cidadeAtual, proximaCidade)
  }
  # Retorno do fitness: menor distância percorrida
  return(1 / distanciaTotal)
}

# Função para gerar uma população inicial aleatória
gerarPopulacaoInicial <- function(numIndividuos, cidades) {
  populacao <- list()
  for (i in 1:numIndividuos) {
    rota <- sample(cidades)
    populacao[[i]] <- rota
  }
  return(populacao)
}

# Função para realizar o crossover entre dois indivíduos (rotas)
crossover <- function(rota1, rota2) {
  pontoCorte <- sample(2:(length(rota1) - 1), 1)
  novaRota <- c(rota1[1:pontoCorte], rota2[(pontoCorte + 1):length(rota2)])
  return(novaRota)
}

# Função para realizar a mutação em um indivíduo (rota)
mutacao <- function(rota, taxaMutacao) {
  for (i in 2:(length(rota) - 1)) {
    if (runif(1) < taxaMutacao) {
      cidadeAleatoria <- sample(rota[-c(1, i)])
      rota[i] <- cidadeAleatoria
    }
  }
  return(rota)
}

# Função para selecionar os indivíduos mais aptos
selecao <- function(populacao, tamanhoTorneio) {
  novaPopulacao <- list()
  for (i in 1:length(populacao)) {
    individuosTorneio <- sample(populacao, tamanhoTorneio, replace = FALSE)
    fitnessTorneio <- sapply(individuosTorneio, calcularFitness)
    melhorIndividuo <- individuosTorneio[which.max(fitnessTorneio)]
    novaPopulacao[[i]] <- melhorIndividuo
  }
  return(novaPopulacao)
}

# Função para executar o algoritmo genético
algoritmoGenetico <- function(numGeracoes, tamanhoPopulacao, tamanhoTorneio, taxaMutacao, cidades) {
  populacao <- gerarPopulacaoInicial(tamanhoPopulacao, cidades)
  
  for (geracao in 1:numGeracoes) {
    novaPopulacao <- list()
    
    while (length(novaPopulacao) < tamanhoPopulacao) {
      individuo1 <- sample(populacao, 1)
      individuo2 <- sample(populacao, 1)
      
      filho <- crossover(individuo1, individuo2)
      filho <- mutacao(filho, taxaMutacao)
      
      novaPopulacao[[length(novaPopulacao) + 1]] <- filho
    }
    
    populacao <- selecao(novaPopulacao, tamanhoTorneio)
  }
  
  # Retorno do melhor indivíduo da última geração
  melhoresFitness <- sapply(populacao, calcularFitness)
  melhorIndividuo <- populacao[which.max(melhoresFitness)]
  return(melhorIndividuo)
}

# Definição das cidades (exemplo: coordenadas x e y) 
cidades <- data.frame(x = c(1, 2, 3, 4, 5), y = c(1, 2, 3, 4, 5))

# Parâmetros do algoritmo genético
numGeracoes <- 100
tamanhoPopulacao <- 50
tamanhoTorneio <- 5
taxaMutacao <- 0.1

# Execução do algoritmo genético
melhorRota <- algoritmoGenetico(numGeracoes, tamanhoPopulacao, tamanhoTorneio, taxaMutacao, cidades)

# Imprimir a melhor rota encontrada
print(melhorRota)
```

Este código implementa um algoritmo genético simples para resolver o problema do Caixeiro Viajante. Ele começa definindo algumas funções auxiliares, como calcular a distância entre duas cidades, calcular o fitness de um indivíduo (rota), gerar uma população inicial aleatória, realizar o crossover entre dois indivíduos (rotas), realizar a mutação em um indivíduo (rota) e selecionar os indivíduos mais aptos.

Em seguida, a função `algoritmoGenetico` executa o algoritmo genético em si. Ela recebe como parâmetros o número de gerações, o tamanho da população, o tamanho do torneio, a taxa de mutação e as cidades. Dentro dessa função, a população inicial é gerada, e então o algoritmo é executado por um número especificado de gerações. Em cada geração, uma nova população é gerada através do crossover e mutação dos indivíduos da população anterior, e a seleção é realizada para escolher os indivíduos mais aptos.

No final, o código imprime a melhor rota encontrada pelo algoritmo genético.