Claro! Aqui está um exemplo de um código complexo em Rust que implementa um algoritmo genético para resolver o problema do caixeiro-viajante (TSP). O TSP é um problema clássico de otimização, onde o objetivo é encontrar o caminho mais curto que visita todas as cidades em um determinado conjunto de cidades, retornando à cidade inicial.

```rust
use rand::prelude::*;
use std::cmp::Ordering;
use std::f64;

struct Cidade {
    x: f64,
    y: f64,
}

impl Cidade {
    fn nova(x: f64, y: f64) -> Cidade {
        Cidade { x, y }
    }

    fn distancia(&self, outra_cidade: &Cidade) -> f64 {
        let dx = self.x - outra_cidade.x;
        let dy = self.y - outra_cidade.y;
        (dx * dx + dy * dy).sqrt()
    }
}

struct Populacao {
    rotas: Vec<Vec<usize>>,
    cidades: Vec<Cidade>,
}

impl Populacao {
    fn nova(tamanho: usize, cidades: Vec<Cidade>) -> Populacao {
        let mut rng = thread_rng();
        let cidades_len = cidades.len();
        let mut rotas = Vec::with_capacity(tamanho);

        for _ in 0..tamanho {
            let mut rota = (0..cidades_len).collect::<Vec<usize>>();
            rota.shuffle(&mut rng);
            rotas.push(rota);
        }

        Populacao { rotas, cidades }
    }

    fn avaliar_fitness(&mut self) {
        for rota in &mut self.rotas {
            let mut distancia_total = 0.0;

            for i in 0..rota.len() - 1 {
                let cidade_atual = &self.cidades[rota[i]];
                let proxima_cidade = &self.cidades[rota[i + 1]];
                distancia_total += cidade_atual.distancia(proxima_cidade);
            }

            let cidade_inicial = &self.cidades[rota[0]];
            let cidade_final = &self.cidades[rota[rota.len() - 1]];
            distancia_total += cidade_final.distancia(cidade_inicial);
            rota.push(distancia_total as usize);
        }

        self.rotas.sort_by(|a, b| {
            let fitness_a = a[a.len() - 1];
            let fitness_b = b[b.len() - 1];
            fitness_a.partial_cmp(&fitness_b).unwrap_or(Ordering::Equal)
        });
    }

    fn selecionar_pais(&self, quantidade: usize) -> Vec<Vec<usize>> {
        let mut rng = thread_rng();
        let mut pais_selecionados = Vec::with_capacity(quantidade);

        for _ in 0..quantidade {
            let indice_pai = rng.gen_range(0..self.rotas.len());
            pais_selecionados.push(self.rotas[indice_pai].clone());
        }

        pais_selecionados
    }

    fn crossover(&self, pais: Vec<Vec<usize>>) -> Vec<Vec<usize>> {
        let mut rng = thread_rng();
        let mut filhos = Vec::with_capacity(pais.len() * 2);

        for i in 0..pais.len() / 2 {
            let pai1 = &pais[i];
            let pai2 = &pais[i + 1];
            let ponto_corte = rng.gen_range(1..(pai1.len() - 1));
            let mut filho1 = pai1[0..ponto_corte].to_vec();
            let mut filho2 = pai2[0..ponto_corte].to_vec();

            for j in ponto_corte..pai1.len() - 1 {
                if !filho1.contains(&pai2[j]) {
                    filho1.push(pai2[j]);
                }
                if !filho2.contains(&pai1[j]) {
                    filho2.push(pai1[j]);
                }
            }

            filhos.push(filho1);
            filhos.push(filho2);
        }

        filhos
    }

    fn mutacao(&self, filhos: &mut Vec<Vec<usize>>) {
        let mut rng = thread_rng();

        for filho in filhos {
            if rng.gen::<f64>() < 0.05 {
                let indice1 = rng.gen_range(0..filho.len() - 1);
                let indice2 = rng.gen_range(0..filho.len() - 1);
                filho.swap(indice1, indice2);
            }
        }
    }

    fn obter_melhor_rota(&self) -> &Vec<usize> {
        self.rotas.first().unwrap()
    }
}

fn main() {
    let cidades = vec![
        Cidade::nova(0.0, 0.0),
        Cidade::nova(1.0, 5.0),
        Cidade::nova(2.0, 3.0),
        Cidade::nova(5.0, 2.0),
        Cidade::nova(4.0, 6.0),
        Cidade::nova(6.0, 4.0),
    ];

    let tamanho_populacao = 100;
    let geracoes = 1000;

    let mut populacao = Populacao::nova(tamanho_populacao, cidades);

    for _ in 0..geracoes {
        populacao.avaliar_fitness();
        let pais = populacao.selecionar_pais(tamanho_populacao / 2);
        let mut filhos = populacao.crossover(pais);
        populacao.mutacao(&mut filhos);
        populacao.rotas[0..tamanho_populacao / 2].clone_from_slice(&filhos[0..tamanho_populacao / 2]);
    }

    let melhor_rota = populacao.obter_melhor_rota();

    println!("Melhor rota encontrada: {:?}", melhor_rota);
}
```

Este código implementa uma estrutura `Cidade` para representar cada cidade com suas coordenadas `x` e `y`. A estrutura `Populacao` é usada para gerar e evoluir a população de rotas do TSP. Ela contém um vetor de rotas, onde cada rota é representada como um vetor de índices das cidades visitadas.

O algoritmo utiliza o método da seleção de pais, crossover e mutação para evoluir a população. Na função `main()`, inicializamos uma população com um tamanho de 100 e executamos o algoritmo por 1000 gerações. No final, o código imprime a melhor rota encontrada.

Note que o código utiliza a biblioteca externa `rand` para gerar números aleatórios.

Espero que este código complexo em Rust atenda às suas expectativas!