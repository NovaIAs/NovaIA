Claro! Aqui está um código em JavaScript que gera um labirinto aleatório usando o algoritmo de busca em profundidade (DFS):

```javascript
// Definindo as dimensões do labirinto
const rows = 20; // número de linhas
const cols = 20; // número de colunas

// Cria uma matriz vazia para representar o labirinto
const maze = new Array(rows);
for (let i = 0; i < rows; i++) {
  maze[i] = new Array(cols).fill(0);
}

// Define as direções possíveis para se mover no labirinto
const directions = [
  { dx: 0, dy: -1, opposite: 2 }, // cima
  { dx: 1, dy: 0, opposite: 3 }, // direita
  { dx: 0, dy: 1, opposite: 0 }, // baixo
  { dx: -1, dy: 0, opposite: 1 } // esquerda
];

// Função para checar se uma posição está dentro dos limites do labirinto
function isValid(x, y) {
  return x >= 0 && x < cols && y >= 0 && y < rows;
}

// Função para checar se uma posição já foi visitada
function isVisited(x, y) {
  return maze[y][x] === 1;
}

// Função para checar se uma posição tem vizinhos não visitados
function hasUnvisitedNeighbor(x, y) {
  for (let i = 0; i < directions.length; i++) {
    const nx = x + directions[i].dx;
    const ny = y + directions[i].dy;
    if (isValid(nx, ny) && !isVisited(nx, ny)) {
      return true;
    }
  }
  return false;
}

// Função para embaralhar as direções possíveis
function shuffleDirections() {
  for (let i = directions.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [directions[i], directions[j]] = [directions[j], directions[i]];
  }
}

// Função recursiva para gerar o labirinto usando DFS
function generateMaze(x, y) {
  maze[y][x] = 1; // marca a posição como visitada

  shuffleDirections(); // embaralha as direções possíveis

  for (let i = 0; i < directions.length; i++) {
    const nx = x + directions[i].dx;
    const ny = y + directions[i].dy;

    if (isValid(nx, ny) && !isVisited(nx, ny)) {
      maze[y][x] |= 1 << i; // marca a parede como aberta

      maze[ny][nx] |= 1 << directions[i].opposite; // marca a parede oposta como aberta

      generateMaze(nx, ny); // chama recursivamente para a próxima posição
    }
  }
}

// Chama a função para gerar o labirinto a partir da posição inicial (0, 0)
generateMaze(0, 0);

// Imprime o labirinto gerado
for (let i = 0; i < rows; i++) {
  for (let j = 0; j < cols; j++) {
    const cell = maze[i][j];
    console.log(`Célula (${j}, ${i}):`);

    if ((cell & 1) === 0) {
      console.log("  Parede superior aberta");
    }

    if ((cell & 2) === 0) {
      console.log("  Parede direita aberta");
    }

    if ((cell & 4) === 0) {
      console.log("  Parede inferior aberta");
    }

    if ((cell & 8) === 0) {
      console.log("  Parede esquerda aberta");
    }
  }
}
```

Este código utiliza o algoritmo de busca em profundidade para gerar um labirinto aleatório. A cada passo, a função `generateMaze` marca a posição atual como visitada e embaralha as direções possíveis. Em seguida, ela verifica se cada uma das direções leva a uma posição válida não visitada. Se sim, as paredes correspondentes são marcadas como abertas, e a função é chamada recursivamente para a próxima posição.

Após a geração do labirinto, o código imprime as informações de cada célula, indicando quais paredes estão abertas.