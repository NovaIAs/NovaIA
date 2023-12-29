Claro! Aqui está um código complexo em JavaScript que foi desenvolvido especialmente para você. Este código é um simulador de jogo de tabuleiro chamado "Batalha Naval". Vou explicar cada parte para que você possa entendê-lo completamente.

```javascript
// Definindo as dimensões do tabuleiro
const larguraTabuleiro = 10;
const alturaTabuleiro = 10;

// Criando o tabuleiro vazio
const tabuleiro = [];
for (let i = 0; i < alturaTabuleiro; i++) {
  const linha = [];
  for (let j = 0; j < larguraTabuleiro; j++) {
    linha.push("-");
  }
  tabuleiro.push(linha);
}

// Posicionando os navios no tabuleiro
const navios = [
  { nome: "Porta-Aviões", tamanho: 5 },
  { nome: "Encouraçado", tamanho: 4 },
  { nome: "Submarino", tamanho: 3 },
  { nome: "Contratorpedeiro", tamanho: 3 },
  { nome: "Navio Patrulha", tamanho: 2 }
];

for (const navio of navios) {
  let posicaoValida = false;
  while (!posicaoValida) {
    const linha = Math.floor(Math.random() * alturaTabuleiro);
    const coluna = Math.floor(Math.random() * larguraTabuleiro);
    const direcao = Math.random() < 0.5 ? "horizontal" : "vertical";

    // Verificando se a posição é válida para o navio
    let posicaoOcupada = false;
    if (direcao === "horizontal") {
      if (coluna + navio.tamanho > larguraTabuleiro) {
        continue;
      }
      for (let i = 0; i < navio.tamanho; i++) {
        if (tabuleiro[linha][coluna + i] !== "-") {
          posicaoOcupada = true;
          break;
        }
      }
    } else {
      if (linha + navio.tamanho > alturaTabuleiro) {
        continue;
      }
      for (let i = 0; i < navio.tamanho; i++) {
        if (tabuleiro[linha + i][coluna] !== "-") {
          posicaoOcupada = true;
          break;
        }
      }
    }

    if (!posicaoOcupada) {
      posicaoValida = true;

      // Posicionando o navio no tabuleiro
      if (direcao === "horizontal") {
        for (let i = 0; i < navio.tamanho; i++) {
          tabuleiro[linha][coluna + i] = navio.nome.charAt(0);
        }
      } else {
        for (let i = 0; i < navio.tamanho; i++) {
          tabuleiro[linha + i][coluna] = navio.nome.charAt(0);
        }
      }
    }
  }
}

// Função para exibir o tabuleiro na tela
function exibirTabuleiro() {
  console.log("\nTabuleiro:");

  // Exibindo os números das colunas
  let colunas = " ";
  for (let i = 0; i < larguraTabuleiro; i++) {
    colunas += " " + i;
  }
  console.log(colunas);

  // Exibindo as linhas do tabuleiro
  for (let i = 0; i < alturaTabuleiro; i++) {
    let linha = i + " ";
    for (let j = 0; j < larguraTabuleiro; j++) {
      linha += tabuleiro[i][j] + " ";
    }
    console.log(linha);
  }
}

// Função para realizar um ataque
function atacar(linha, coluna) {
  const alvo = tabuleiro[linha][coluna];

  if (alvo === "-") {
    console.log("Água!");

    // Marcando a água no tabuleiro
    tabuleiro[linha][coluna] = "O";
  } else {
    console.log("Acertou um navio!");

    // Marcando o acerto no tabuleiro
    tabuleiro[linha][coluna] = "X";

    // Verificando se todos os navios foram afundados
    let afundouTodos = true;
    for (const linhaTabuleiro of tabuleiro) {
      for (const alvoTabuleiro of linhaTabuleiro) {
        if (navios.some(navio => navio.nome.charAt(0) === alvoTabuleiro)) {
          afundouTodos = false;
          break;
        }
      }
      if (!afundouTodos) {
        break;
      }
    }

    if (afundouTodos) {
      console.log("Parabéns! Você afundou todos os navios!");
    }
  }
}

// Exibindo o tabuleiro inicialmente
exibirTabuleiro();

// Simulação de jogadas
atacar(2, 3);
atacar(4, 5);
atacar(6, 7);
atacar(8, 9);

// Exibindo o tabuleiro após as jogadas
exibirTabuleiro();
```

Este código simula o famoso jogo "Batalha Naval" onde você precisa afundar todos os navios inimigos no tabuleiro. O código começa definindo as dimensões do tabuleiro (10 por 10) e cria um tabuleiro vazio usando um array bidimensional.

Em seguida, posiciona os navios aleatoriamente no tabuleiro. Cada navio é representado por um objeto com um nome e um tamanho. O código utiliza um loop while para tentar posicionar cada navio em uma posição válida, garantindo que não haja colisões com outros navios já posicionados.

Após posicionar os navios, há uma função chamada "exibirTabuleiro" que exibe o tabuleiro na tela com os números das colunas e as marcações de água e navios.

A função "atacar" é responsável por receber uma coordenada de ataque (linha e coluna) e verificar se acertou um navio ou apenas água. Caso seja um acerto, o código marca a posição no tabuleiro com um "X" e verifica se todos os navios foram afundados. Se sim, exibe uma mensagem de parabéns.

Por fim, o código realiza algumas simulações de jogadas, chamando a função "atacar" em diferentes coordenadas, e em seguida, exibe o tabuleiro novamente para mostrar os resultados das jogadas.

Espero que você tenha achado esse código complexo e diferenciado! Se tiver alguma dúvida ou precisar de mais ajuda, é só me perguntar.