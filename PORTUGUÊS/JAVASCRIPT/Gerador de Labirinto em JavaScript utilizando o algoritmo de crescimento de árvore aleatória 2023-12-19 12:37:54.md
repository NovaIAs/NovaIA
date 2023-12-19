Claro! Aqui está um código em JavaScript mais complexo e diferenciado. Ele é chamado de "Gerador de Labirinto" e cria um labirinto aleatório usando o algoritmo de crescimento de árvore aleatória (Randomized Prim's Algorithm).

```javascript
// Definindo a largura e a altura do labirinto
const largura = 21;
const altura = 21;

// Criando uma matriz vazia para representar o labirinto
const labirinto = new Array(altura);
for (let i = 0; i < altura; i++) {
  labirinto[i] = new Array(largura).fill(0);
}

// Definindo uma função para checar se uma célula está dentro dos limites do labirinto
function dentroDosLimites(x, y) {
  return x >= 0 && x < largura && y >= 0 && y < altura;
}

// Definindo uma função para checar se uma célula já foi visitada (1) ou não (0)
function naoVisitada(x, y) {
  return labirinto[y][x] === 0;
}

// Definindo uma função para checar se uma célula é válida para ser adicionada à árvore
function celulaValida(x, y) {
  return dentroDosLimites(x, y) && naoVisitada(x, y);
}

// Definindo uma função para obter as células vizinhas de uma célula em uma direção específica
function obterVizinhos(x, y, direcao) {
  const vizinhos = [];

  switch (direcao) {
    case "cima":
      if (celulaValida(x, y - 2)) {
        vizinhos.push({ x: x, y: y - 2 });
      }
      break;
    case "baixo":
      if (celulaValida(x, y + 2)) {
        vizinhos.push({ x: x, y: y + 2 });
      }
      break;
    case "esquerda":
      if (celulaValida(x - 2, y)) {
        vizinhos.push({ x: x - 2, y: y });
      }
      break;
    case "direita":
      if (celulaValida(x + 2, y)) {
        vizinhos.push({ x: x + 2, y: y });
      }
      break;
  }

  return vizinhos;
}

// Definindo uma função para remover a parede entre duas células
function removerParede(celulaAtual, celulaVizinha) {
  const x = celulaVizinha.x + (celulaAtual.x - celulaVizinha.x) / 2;
  const y = celulaVizinha.y + (celulaAtual.y - celulaVizinha.y) / 2;
  labirinto[y][x] = 1;
}

// Definindo uma função para gerar o labirinto usando o algoritmo de crescimento de árvore aleatória
function gerarLabirinto() {
  // Selecionando uma célula inicial aleatória
  const xInicial = Math.floor(Math.random() * (largura - 2) + 1);
  const yInicial = Math.floor(Math.random() * (altura - 2) + 1);
  labirinto[yInicial][xInicial] = 1;

  // Criando uma pilha para armazenar as células visitadas
  const pilha = [];
  pilha.push({ x: xInicial, y: yInicial });

  while (pilha.length > 0) {
    const celulaAtual = pilha[pilha.length - 1];
    const x = celulaAtual.x;
    const y = celulaAtual.y;

    // Obtendo as células vizinhas válidas
    const vizinhosValidos = [];

    const vizinhosCima = obterVizinhos(x, y, "cima");
    if (vizinhosCima.length > 0) {
      vizinhosValidos.push(vizinhosCima[Math.floor(Math.random() * vizinhosCima.length)]);
    }

    const vizinhosBaixo = obterVizinhos(x, y, "baixo");
    if (vizinhosBaixo.length > 0) {
      vizinhosValidos.push(vizinhosBaixo[Math.floor(Math.random() * vizinhosBaixo.length)]);
    }

    const vizinhosEsquerda = obterVizinhos(x, y, "esquerda");
    if (vizinhosEsquerda.length > 0) {
      vizinhosValidos.push(vizinhosEsquerda[Math.floor(Math.random() * vizinhosEsquerda.length)]);
    }

    const vizinhosDireita = obterVizinhos(x, y, "direita");
    if (vizinhosDireita.length > 0) {
      vizinhosValidos.push(vizinhosDireita[Math.floor(Math.random() * vizinhosDireita.length)]);
    }

    if (vizinhosValidos.length > 0) {
      // Escolhendo uma célula vizinha aleatória
      const celulaVizinha = vizinhosValidos[Math.floor(Math.random() * vizinhosValidos.length)];

      // Removendo a parede entre as células
      removerParede(celulaAtual, celulaVizinha);

      // Marcando a célula vizinha como visitada
      labirinto[celulaVizinha.y][celulaVizinha.x] = 1;

      // Adicionando a célula vizinha à pilha
      pilha.push(celulaVizinha);
    } else {
      // Removendo a célula atual da pilha se não houver células vizinhas válidas
      pilha.pop();
    }
  }
}

// Chamando a função para gerar o labirinto
gerarLabirinto();

// Imprimindo o labirinto no console
for (let i = 0; i < altura; i++) {
  let linha = "";
  for (let j = 0; j < largura; j++) {
    if (labirinto[i][j] === 0) {
      linha += "#";
    } else {
      linha += " ";
    }
  }
  console.log(linha);
}
```

Este código gera um labirinto aleatório utilizando o algoritmo de crescimento de árvore aleatória. Ele começa criando uma matriz vazia que representa o labirinto. Em seguida, ele define várias funções auxiliares para checar se uma célula é válida, obter vizinhos de uma célula, remover paredes entre células e gerar o labirinto em si.

A função `gerarLabirinto` começa selecionando uma célula inicial aleatória e marcando-a como visitada. Em seguida, ela cria uma pilha para armazenar as células visitadas. O algoritmo continua até que a pilha esteja vazia. Em cada iteração, ele obtém as células vizinhas válidas da célula atual e escolhe uma célula vizinha aleatória. A parede entre a célula atual e a célula vizinha é removida e a célula vizinha é marcada como visitada. A célula vizinha é então adicionada à pilha. Se não houver células vizinhas válidas, a célula atual é removida da pilha.

No final, o labirinto é impresso no console, utilizando "#" para representar paredes e espaços em branco para representar caminhos. O tamanho do labirinto é definido pela largura e altura escolhidas no início do código.

Espero que este código atenda às suas expectativas! Se tiver mais alguma dúvida ou precisar de algo adicional, é só me dizer.