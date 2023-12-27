Certamente! Aqui está um exemplo de código JavaScript mais complexo, que envolve a criação de uma classe para manipulação de um jogo de cartas chamado "Truco". O código está comentado para facilitar a compreensão:

```javascript
// Classe que representa um jogador
class Jogador {
  constructor(nome, pontuacao) {
    this.nome = nome;
    this.pontuacao = pontuacao;
  }

  // Método para adicionar pontos ao jogador
  adicionarPontos(pontos) {
    this.pontuacao += pontos;
  }

  // Método para resetar a pontuação do jogador
  resetarPontuacao() {
    this.pontuacao = 0;
  }
}

// Classe que representa uma carta
class Carta {
  constructor(valor, naipe) {
    this.valor = valor;
    this.naipe = naipe;
  }
}

// Função que embaralha um array de cartas
function embaralhar(cartas) {
  for (let i = cartas.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [cartas[i], cartas[j]] = [cartas[j], cartas[i]];
  }
}

// Classe que representa um baralho
class Baralho {
  constructor() {
    this.cartas = [];

    const naipes = ['Paus', 'Copas', 'Espadas', 'Ouros'];
    const valores = ['4', '5', '6', '7', 'Q', 'J', 'K', 'A', '2', '3'];

    // Cria um array com todas as combinações de cartas possíveis
    for (const naipe of naipes) {
      for (const valor of valores) {
        this.cartas.push(new Carta(valor, naipe));
      }
    }
  }

  // Método para embaralhar o baralho
  embaralhar() {
    embaralhar(this.cartas);
  }

  // Método para distribuir uma quantidade específica de cartas para cada jogador
  distribuirCartas(jogadores, quantidade) {
    for (let i = 0; i < quantidade; i++) {
      for (const jogador of jogadores) {
        jogador.cartas.push(this.cartas.pop());
      }
    }
  }
}

// Criação dos jogadores
const jogador1 = new Jogador('Jogador 1', 0);
const jogador2 = new Jogador('Jogador 2', 0);

// Criação do baralho
const baralho = new Baralho();

// Embaralha as cartas do baralho
baralho.embaralhar();

// Distribui 3 cartas para cada jogador
baralho.distribuirCartas([jogador1, jogador2], 3);

// Exemplo de uso dos métodos e propriedades
console.log('Jogador 1:', jogador1);
console.log('Jogador 2:', jogador2);

jogador1.adicionarPontos(5);
jogador2.adicionarPontos(3);

console.log('Jogador 1:', jogador1);
console.log('Jogador 2:', jogador2);
```

Neste código, é criada uma classe `Jogador` que representa um jogador de truco, com métodos para adicionar pontos e resetar a pontuação. Em seguida, é criada uma classe `Carta` que representa uma carta do jogo, com propriedades para o valor e naipe da carta.

A função `embaralhar` é definida para embaralhar um array de cartas usando o algoritmo de Fisher-Yates.

A classe `Baralho` é criada para representar um baralho composto por várias cartas. No construtor dessa classe, são criadas todas as combinações possíveis de cartas de acordo com os naipes e valores definidos.

Os métodos `embaralhar` e `distribuirCartas` são definidos para embaralhar as cartas e distribuí-las para os jogadores, respectivamente.

Por fim, são criados dois jogadores, um baralho é instanciado e as cartas são embaralhadas. Então, são distribuídas 3 cartas para cada jogador.

O código também faz uso dos métodos e propriedades das classes `Jogador` e `Baralho` para exemplificar o funcionamento.

Espero que este código atenda às suas expectativas! Se precisar de mais alguma coisa, é só me pedir.