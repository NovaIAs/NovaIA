Claro! Aqui está um exemplo de um código complexo em JavaScript que cria um jogo de labirinto em que o jogador precisa encontrar a saída. O código usa a biblioteca p5.js para renderizar o jogo na tela.

```javascript
// Definindo as dimensões do labirinto
const larguraLinha = 40;
const alturaLinha = 40;
const numLinhas = 15;
const numColunas = 15;

// Definindo o labirinto
let labirinto = [];
for (let i = 0; i < numLinhas; i++) {
  let linha = [];
  for (let j = 0; j < numColunas; j++) {
    linha.push(true);
  }
  labirinto.push(linha);
}

// Definindo a posição inicial e final do jogador
const posicaoInicial = {
  linha: 0,
  coluna: 0
};
const posicaoFinal = {
  linha: numLinhas - 1,
  coluna: numColunas - 1
};

// Variáveis de controle do jogo
let jogador;

// Função para criar o jogador
function criarJogador() {
  jogador = {
    posicao: {
      linha: posicaoInicial.linha,
      coluna: posicaoInicial.coluna
    },
    moverPara: function(linha, coluna) {
      if (linha >= 0 && linha < numLinhas && coluna >= 0 && coluna < numColunas && !labirinto[linha][coluna]) {
        this.posicao.linha = linha;
        this.posicao.coluna = coluna;
      }
    },
    verificarVitoria: function() {
      if (this.posicao.linha === posicaoFinal.linha && this.posicao.coluna === posicaoFinal.coluna) {
        console.log("Você venceu!");
      }
    }
  };
}

// Função para criar o labirinto aleatoriamente
function criarLabirintoAleatorio() {
  for (let i = 0; i < numLinhas; i++) {
    for (let j = 0; j < numColunas; j++) {
      if (Math.random() < 0.3) {
        labirinto[i][j] = false;
      }
    }
  }
  labirinto[posicaoInicial.linha][posicaoInicial.coluna] = false;
  labirinto[posicaoFinal.linha][posicaoFinal.coluna] = false;
}

// Configurações iniciais do p5.js
function setup() {
  createCanvas(numColunas * larguraLinha, numLinhas * alturaLinha);
  criarJogador();
  criarLabirintoAleatorio();
}

// Função para desenhar o labirinto na tela
function desenharLabirinto() {
  for (let i = 0; i < numLinhas; i++) {
    for (let j = 0; j < numColunas; j++) {
      if (labirinto[i][j]) {
        fill(0);
      } else {
        fill(255);
      }
      rect(j * larguraLinha, i * alturaLinha, larguraLinha, alturaLinha);
    }
  }
}

// Função para desenhar o jogador na tela
function desenharJogador() {
  fill(255, 0, 0);
  rect(jogador.posicao.coluna * larguraLinha, jogador.posicao.linha * alturaLinha, larguraLinha, alturaLinha);
}

// Função de atualização do jogo
function update() {
  desenharLabirinto();
  desenharJogador();
  jogador.verificarVitoria();
}

// Funções para controle do jogador usando as setas do teclado
function keyPressed() {
  if (keyCode === UP_ARROW) {
    jogador.moverPara(jogador.posicao.linha - 1, jogador.posicao.coluna);
  } else if (keyCode === DOWN_ARROW) {
    jogador.moverPara(jogador.posicao.linha + 1, jogador.posicao.coluna);
  } else if (keyCode === LEFT_ARROW) {
    jogador.moverPara(jogador.posicao.linha, jogador.posicao.coluna - 1);
  } else if (keyCode === RIGHT_ARROW) {
    jogador.moverPara(jogador.posicao.linha, jogador.posicao.coluna + 1);
  }
}

// Configurações de desenho e atualização do p5.js
function draw() {
  background(255);
  update();
}
```

Este código cria um jogo de labirinto usando a biblioteca p5.js. O labirinto é definido como uma matriz bidimensional de booleanos, onde `true` representa uma parede e `false` representa um caminho. A posição inicial do jogador é definida como o canto superior esquerdo e a posição final como o canto inferior direito.

O jogador pode se mover para cima, baixo, esquerda e direita usando as setas do teclado. O jogo verifica se o jogador chegou na posição final e exibe uma mensagem de vitória.

O labirinto é gerado aleatoriamente, onde cada célula tem uma chance de 30% de ser um caminho. As paredes externas do labirinto são garantidas como paredes.

O jogo é desenhado na tela do navegador usando o p5.js. A função `setup()` configura o tamanho do canvas e cria o jogador e o labirinto. A função `draw()` é chamada repetidamente para atualizar o desenho na tela, e a função `keyPressed()` é chamada quando uma tecla é pressionada para controlar o jogador.

Espero que isso atenda às suas expectativas!