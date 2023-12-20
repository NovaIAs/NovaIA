Claro! Aqui está um exemplo de código complexo em TypeScript. Este código é um simulador de um jogo de cartas chamado "Blackjack". Ele permite que o jogador jogue contra a inteligência artificial do computador.

```typescript
enum Naipe {
  Copas = 'Copas',
  Espadas = 'Espadas',
  Ouros = 'Ouros',
  Paus = 'Paus'
}

enum Valor {
  Dois = 2,
  Tres = 3,
  Quatro = 4,
  Cinco = 5,
  Seis = 6,
  Sete = 7,
  Oito = 8,
  Nove = 9,
  Dez = 10,
  Valete = 11,
  Dama = 12,
  Rei = 13,
  As = 14
}

class Carta {
  public naipe: Naipe;
  public valor: Valor;

  constructor(naipe: Naipe, valor: Valor) {
    this.naipe = naipe;
    this.valor = valor;
  }

  get nome(): string {
    return `${this.valor} de ${this.naipe}`;
  }
}

class Baralho {
  private cartas: Carta[];

  constructor() {
    this.cartas = [];
    const naipes = Object.values(Naipe);
    const valores = Object.values(Valor);
    for (const naipe of naipes) {
      for (const valor of valores) {
        this.cartas.push(new Carta(naipe, valor));
      }
    }
  }

  embaralhar(): void {
    for (let i = this.cartas.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [this.cartas[i], this.cartas[j]] = [this.cartas[j], this.cartas[i]];
    }
  }

  pegarCarta(): Carta {
    return this.cartas.pop();
  }
}

class Jogador {
  public nome: string;
  public cartas: Carta[];

  constructor(nome: string) {
    this.nome = nome;
    this.cartas = [];
  }

  receberCarta(carta: Carta): void {
    this.cartas.push(carta);
  }

  calcularPontuacao(): number {
    let pontuacao = 0;
    for (const carta of this.cartas) {
      pontuacao += carta.valor;
    }
    return pontuacao;
  }
}

class Jogo {
  private baralho: Baralho;
  private jogador: Jogador;
  private computador: Jogador;

  constructor(nomeJogador: string) {
    this.baralho = new Baralho();
    this.baralho.embaralhar();
    this.jogador = new Jogador(nomeJogador);
    this.computador = new Jogador('Computador');
  }

  iniciarJogo(): void {
    this.jogador.receberCarta(this.baralho.pegarCarta());
    this.computador.receberCarta(this.baralho.pegarCarta());
    this.jogador.receberCarta(this.baralho.pegarCarta());
    this.computador.receberCarta(this.baralho.pegarCarta());

    console.log(`Jogador: ${this.jogador.nome}`);
    console.log(`Cartas do Jogador: ${this.jogador.cartas.map(c => c.nome).join(', ')}`);
    console.log(`Pontuação do Jogador: ${this.jogador.calcularPontuacao()}`);
    console.log("");

    console.log(`Computador: ${this.computador.nome}`);
    console.log(`Cartas do Computador: ${this.computador.cartas[0].nome}, ?`);
    console.log(`Pontuação do Computador: ?`);
    console.log("");

    while (true) {
      const jogada = prompt('Digite "p" para pedir carta ou "f" para finalizar a jogada:');
      if (jogada === 'p') {
        const carta = this.baralho.pegarCarta();
        this.jogador.receberCarta(carta);
        console.log(`Jogador recebeu a carta: ${carta.nome}`);
        console.log(`Pontuação do Jogador: ${this.jogador.calcularPontuacao()}`);
        console.log("");
        if (this.jogador.calcularPontuacao() > 21) {
          console.log('Você estourou 21! Você perdeu.');
          return;
        }
      } else if (jogada === 'f') {
        break;
      }
    }

    while (this.computador.calcularPontuacao() < 17) {
      const carta = this.baralho.pegarCarta();
      this.computador.receberCarta(carta);
      console.log(`Computador recebeu a carta: ${carta.nome}`);
    }

    console.log("");

    console.log(`Jogador: ${this.jogador.nome}`);
    console.log(`Cartas do Jogador: ${this.jogador.cartas.map(c => c.nome).join(', ')}`);
    console.log(`Pontuação do Jogador: ${this.jogador.calcularPontuacao()}`);
    console.log("");

    console.log(`Computador: ${this.computador.nome}`);
    console.log(`Cartas do Computador: ${this.computador.cartas.map(c => c.nome).join(', ')}`);
    console.log(`Pontuação do Computador: ${this.computador.calcularPontuacao()}`);
    console.log("");

    if (this.jogador.calcularPontuacao() > 21) {
      console.log('Você estourou 21! Você perdeu.');
    } else if (this.computador.calcularPontuacao() > 21) {
      console.log('O computador estourou 21! Você venceu.');
    } else if (this.jogador.calcularPontuacao() > this.computador.calcularPontuacao()) {
      console.log('Você venceu!');
    } else if (this.jogador.calcularPontuacao() < this.computador.calcularPontuacao()) {
      console.log('Você perdeu.');
    } else {
      console.log('Empate!');
    }
  }
}

const nomeJogador = prompt('Digite seu nome:');
const jogo = new Jogo(nomeJogador);
jogo.iniciarJogo();
```

Este é um código complexo de um jogo de Blackjack em TypeScript. Ele possui várias classes, como `Carta`, `Baralho`, `Jogador` e `Jogo`, que são responsáveis por lidar com as cartas, o baralho, os jogadores e a lógica do jogo.

O código começa definindo enums para representar os naipes e valores das cartas. Em seguida, temos a classe `Carta`, que possui propriedades para o naipe e o valor da carta, e um getter para obter o nome completo da carta.

A classe `Baralho` representa o baralho de cartas e possui métodos para embaralhar as cartas e pegar uma carta do topo do baralho.

A classe `Jogador` representa um jogador do jogo e possui propriedades para o nome do jogador e as cartas em sua mão. Ela também tem métodos para receber uma carta e calcular a pontuação do jogador.

A classe `Jogo` é responsável por controlar o fluxo do jogo. Ela cria um baralho, dois jogadores (o jogador humano e o computador) e inicia o jogo. O jogo começa distribuindo duas cartas para cada jogador e exibindo as informações iniciais na tela.

Em seguida, o jogador humano é solicitado a fazer jogadas, digitando "p" para pedir uma nova carta ou "f" para finalizar a jogada. Se o jogador ultrapassar 21 pontos, ele perde o jogo. Depois disso, é a vez do computador jogar, seguindo uma estratégia simples de pedir outra carta até atingir uma pontuação de pelo menos 17.

Após as jogadas do jogador e do computador, as pontuações e cartas finais de ambos são exibidas na tela. O código então verifica as condições de vitória ou derrota e exibe a mensagem correspondente.

No final do código, é solicitado ao jogador que digite seu nome e o jogo é iniciado chamando o método `iniciarJogo` da classe `Jogo`.

Espero que este código atenda às suas expectativas e demonstre a complexidade que você solicitou!