Claro, vou gerar um código complexo em JavaScript para você. Este código é um exemplo de uma simulação de um jogo de labirinto em que o jogador deve encontrar a saída. Ele utiliza o algoritmo de busca em profundidade para percorrer o labirinto e encontrar o caminho correto.

```javascript
// Definição do labirinto
const labirinto = [
  [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
  [1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
  [1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1],
  [1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
  [1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1],
  [1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1],
  [1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1],
  [1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1],
  [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
];

// Função de busca em profundidade
function buscaEmProfundidade(labirinto, linha, coluna, caminho) {
  if (linha < 0 || linha >= labirinto.length || coluna < 0 || coluna >= labirinto[0].length) {
    return false; // Verifica se está fora do labirinto
  }

  if (labirinto[linha][coluna] === 1 || labirinto[linha][coluna] === 2) {
    return false; // Verifica se é parede ou já foi visitado
  }

  if (labirinto[linha][coluna] === 3) {
    return true; // Verifica se é a saída
  }

  caminho.push([linha, coluna]); // Adiciona a posição atual ao caminho
  
  // Marca a posição como visitada
  labirinto[linha][coluna] = 2;

  // Tentativa de mover para cima
  if (buscaEmProfundidade(labirinto, linha - 1, coluna, caminho)) {
    return true;
  }

  // Tentativa de mover para a direita
  if (buscaEmProfundidade(labirinto, linha, coluna + 1, caminho)) {
    return true;
  }

  // Tentativa de mover para baixo
  if (buscaEmProfundidade(labirinto, linha + 1, coluna, caminho)) {
    return true;
  }

  // Tentativa de mover para a esquerda
  if (buscaEmProfundidade(labirinto, linha, coluna - 1, caminho)) {
    return true;
  }

  // Remove a posição atual do caminho, já que é um caminho sem saída
  caminho.pop();

  return false;
}

// Função principal
function encontrarSaida(labirinto) {
  const caminho = [];

  // Encontra a posição inicial
  let linhaInicial, colunaInicial;
  for (let i = 0; i < labirinto.length; i++) {
    for (let j = 0; j < labirinto[0].length; j++) {
      if (labirinto[i][j] === 0) {
        linhaInicial = i;
        colunaInicial = j;
        break;
      }
    }
  }

  // Busca a saída do labirinto
  buscaEmProfundidade(labirinto, linhaInicial, colunaInicial, caminho);

  // Imprime o caminho encontrado
  console.log("Caminho encontrado:");
  for (let i = 0; i < caminho.length; i++) {
    console.log("[" + caminho[i][0] + ", " + caminho[i][1] + "]");
  }
}

// Chamada da função principal
encontrarSaida(labirinto);
```

Este código cria um labirinto representado por uma matriz de números, onde 0 representa um caminho livre, 1 representa uma parede e 3 representa a saída. O algoritmo de busca em profundidade é utilizado para encontrar a saída do labirinto, percorrendo-o de forma recursiva. O caminho percorrido é armazenado em um array chamado `caminho`, que é impresso no final.

Este código é um exemplo de um jogo de labirinto simples, mas pode ser adaptado e estendido para criar jogos mais complexos.