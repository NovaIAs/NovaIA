Claro! Aqui está um código complexo em JavaScript que envolve manipulação de DOM, eventos e animação. O código cria um jogo de memória onde o jogador precisa encontrar pares de cartas idênticas em um tabuleiro. Vou explicar o código passo a passo para que você possa entendê-lo completamente:

```javascript
// Definindo uma matriz de cartas com símbolos diferentes
const cartas = [
  "♠", "♠", "♣", "♣", "♥", "♥", "♦", "♦",
  "♫", "♫", "☺", "☺", "☻", "☻", "♞", "♞"
];

// Função para embaralhar as cartas usando o algoritmo Fisher-Yates
function embaralhar(array) {
  let currentIndex = array.length, temporaryValue, randomIndex;

  while (currentIndex !== 0) {
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }

  return array;
}

// Embaralhando as cartas
const cartasEmbaralhadas = embaralhar(cartas);

// Selecionando o elemento HTML onde as cartas serão exibidas
const tabuleiro = document.querySelector(".tabuleiro");

// Função para criar as cartas no tabuleiro
function criarCartas() {
  for (let i = 0; i < cartasEmbaralhadas.length; i++) {
    // Criando o elemento de carta
    const carta = document.createElement("div");
    carta.classList.add("carta");

    // Adicionando um valor personalizado (o símbolo da carta) ao elemento de carta
    carta.dataset.valor = cartasEmbaralhadas[i];

    // Criando o elemento de frente da carta
    const frente = document.createElement("div");
    frente.classList.add("frente");

    // Definindo o texto da frente da carta como o símbolo correspondente
    frente.textContent = cartasEmbaralhadas[i];

    // Criando o elemento de verso da carta
    const verso = document.createElement("div");
    verso.classList.add("verso");

    // Adicionando a frente e o verso ao elemento de carta
    carta.appendChild(frente);
    carta.appendChild(verso);

    // Adicionando a carta ao tabuleiro
    tabuleiro.appendChild(carta);

    // Adicionando um ouvinte de evento de clique à carta
    carta.addEventListener("click", virarCarta);
  }
}

// Função para virar a carta quando clicada
function virarCarta() {
  // Verificando se o elemento de carta já está virado ou emparelhado
  if (this.classList.contains("virada") || this.classList.contains("emparelhada")) {
    return;
  }

  // Adicionando a classe "virada" ao elemento de carta
  this.classList.add("virada");

  // Selecionando todas as cartas viradas
  const cartasViradas = document.querySelectorAll(".virada");

  // Verificando se duas cartas foram viradas
  if (cartasViradas.length === 2) {
    // Desativando os eventos de clique nas cartas
    tabuleiro.classList.add("disable-click");

    // Verificando se as cartas são iguais
    if (cartasViradas[0].dataset.valor === cartasViradas[1].dataset.valor) {
      // Adicionando a classe "emparelhada" às cartas
      cartasViradas.forEach(carta => carta.classList.add("emparelhada"));

      // Removendo a classe "virada" das cartas
      cartasViradas.forEach(carta => carta.classList.remove("virada"));
    } else {
      // Aguardando 1 segundo antes de desvirar as cartas viradas
      setTimeout(() => {
        cartasViradas.forEach(carta => carta.classList.remove("virada"));
        tabuleiro.classList.remove("disable-click");
      }, 1000);
    }
  }
}

// Chamando a função para criar as cartas no tabuleiro
criarCartas();
```

Neste código, começamos definindo uma matriz de cartas com símbolos diferentes, por exemplo, naipes de baralho e emoticons. Em seguida, temos a função `embaralhar` que utiliza o algoritmo Fisher-Yates para embaralhar as cartas aleatoriamente.

Depois de embaralhar as cartas, selecionamos o elemento HTML onde as cartas serão exibidas utilizando o método `querySelector`. Em seguida, temos a função `criarCartas`, que cria os elementos de carta no tabuleiro. Para cada carta, um elemento de carta é criado, contendo um elemento de frente e um elemento de verso. O valor personalizado da carta (o símbolo) é armazenado usando o atributo `dataset`.

Em seguida, é adicionado um ouvinte de evento de clique a cada carta utilizando o método `addEventListener`. Quando uma carta é clicada, a função `virarCarta` é chamada. Esta função verifica se a carta já está virada ou emparelhada. Se não estiver, a classe "virada" é adicionada à carta.

A função também verifica se duas cartas foram viradas. Se sim, os eventos de clique no tabuleiro são desativados temporariamente. Em seguida, é verificado se as cartas são iguais. Se forem, a classe "emparelhada" é adicionada às cartas e a classe "virada" é removida. Caso contrário, as cartas são desviradas após 1 segundo usando o método `setTimeout`.

Por fim, chamamos a função `criarCartas` para criar as cartas no tabuleiro.

Espero que este código complexo em JavaScript atenda às suas expectativas!