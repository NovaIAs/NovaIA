Claro! Aqui está um exemplo de código complexo em Rust, que implementa um algoritmo genético para resolver o problema clássico do caixeiro-viajante (Traveling Salesman Problem - TSP). O código está comentado para explicar cada etapa.

```rust
use rand::Rng;
use std::cmp::Ordering;

// Definindo a estrutura do indivíduo
struct Individuo {
    rota: Vec<usize>,
    distancia: f64,
}

impl Individuo {
    // Função para criar um novo indivíduo aleatório
    fn novo(rng: &mut impl Rng, tamanho: usize) -> Individuo {
        let mut rota: Vec<usize> = (0..tamanho).collect();
        rng.shuffle(&mut rota);
        Individuo {
            rota,
            distancia: 0.0,
        }
    }

    // Função para calcular a distância total da rota
    fn calcular_distancia(&mut self, matriz_distancias: &Vec<Vec<f64>>) {
        let mut distancia = 0.0;
        for i in 0..self.rota.len() - 1 {
            distancia += matriz_distancias[self.rota[i]][self.rota[i + 1]];
        }
        distancia += matriz_distancias[self.rota[self.rota.len() - 1]][self.rota[0]];
        self.distancia = distancia;
    }
}

// Função para criar uma matriz de distâncias aleatória
fn criar_matriz_distancias(rng: &mut impl Rng, tamanho: usize) -> Vec<Vec<f64>> {
    let mut matriz_distancias = vec![vec![0.0; tamanho]; tamanho];
    for i in 0..tamanho {
        for j in i + 1..tamanho {
            matriz_distancias[i][j] = rng.gen_range(1.0..1000.0);
            matriz_distancias[j][i] = matriz_distancias[i][j];
        }
    }
    matriz_distancias
}

// Função para realizar o crossover entre dois indivíduos
fn crossover(pai: &Individuo, mae: &Individuo, rng: &mut impl Rng) -> Individuo {
    let mut filho = Individuo {
        rota: vec![0; pai.rota.len()],
        distancia: 0.0,
    };

    let ponto_corte = rng.gen_range(1..pai.rota.len() - 1);

    for i in 0..ponto_corte {
        filho.rota[i] = pai.rota[i];
    }

    let mut idx = 0;
    for i in ponto_corte..mae.rota.len() {
        while filho.rota.contains(&mae.rota[idx]) {
            idx += 1;
        }
        filho.rota[i] = mae.rota[idx];
    }

    filho
}

// Função para realizar a mutação em um indivíduo
fn mutacao(individuo: &mut Individuo, rng: &mut impl Rng) {
    let i = rng.gen_range(0..individuo.rota.len());
    let j = rng.gen_range(0..individuo.rota.len());
    individuo.rota.swap(i, j);
}

// Função para selecionar os melhores indivíduos da população
fn selecao(populacao: &mut Vec<Individuo>, num_elitismo: usize) -> Vec<Individuo> {
    populacao.sort_by(|a, b| a.distancia.partial_cmp(&b.distancia).unwrap_or(Ordering::Equal));
    populacao[..num_elitismo].to_vec()
}

// Função principal
fn main() {
    let populacao_tamanho = 100;
    let num_geracoes = 1000;
    let num_elitismo = 10;

    let mut rng = rand::thread_rng();

    let tamanho = 20;
    let matriz_distancias = criar_matriz_distancias(&mut rng, tamanho);

    let mut populacao: Vec<Individuo> = (0..populacao_tamanho)
        .map(|_| Individuo::novo(&mut rng, tamanho))
        .collect();

    for _ in 0..num_geracoes {
        let mut nova_populacao: Vec<Individuo> = selecao(&mut populacao, num_elitismo);

        while nova_populacao.len() < populacao_tamanho {
            let pai = rng.choose(&populacao).unwrap();
            let mae = rng.choose(&populacao).unwrap();

            let filho = crossover(pai, mae, &mut rng);

            if rng.gen_range(0.0..1.0) < 0.01 {
                mutacao(&mut filho, &mut rng);
            }

            filho.calcular_distancia(&matriz_distancias);

            nova_populacao.push(filho);
        }

        populacao = nova_populacao;
    }

    populacao.sort_by(|a, b| a.distancia.partial_cmp(&b.distancia).unwrap_or(Ordering::Equal));

    let melhor_individuo = &populacao[0];

    println!("Melhor rota encontrada: {:?}", melhor_individuo.rota);
    println!("Distância total: {:.2}", melhor_individuo.distancia);
}
```

Este código implementa um algoritmo genético para resolver o TSP. Primeiro, definimos a estrutura do indivíduo, que consiste em uma rota (representada por um vetor de índices) e a distância total percorrida nessa rota.

Em seguida, temos as funções auxiliares `novo`, que cria um novo indivíduo aleatório, `calcular_distancia`, que calcula a distância total da rota e `criar_matriz_distancias`, que gera uma matriz de distâncias aleatória.

A função `crossover` realiza o crossover entre dois indivíduos, escolhendo um ponto de corte aleatório e combinando as rotas dos pais para gerar a rota do filho.

A função `mutacao` realiza a mutação em um indivíduo, trocando aleatoriamente duas posições da rota.

A função `selecao` seleciona os melhores indivíduos da população com base em sua distância total percorrida.

Na função principal `main`, definimos os parâmetros do algoritmo, como o tamanho da população, o número de gerações e o número de indivíduos selecionados para elitismo.

Em seguida, criamos a matriz de distâncias e a população inicial aleatória.

Dentro do loop principal, realizamos a seleção, crossover e mutação para gerar a nova população. A cada iteração, a população é ordenada com base na distância total percorrida, e o melhor indivíduo é impresso ao final do algoritmo.

Espero que este código complexo em Rust tenha atendido às suas expectativas!