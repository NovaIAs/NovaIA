Claro! Aqui está um exemplo de um código complexo em JavaScript que realiza uma série de tarefas:

```javascript
// Definindo uma função recursiva para cálculo do fatorial
function fatorial(n) {
  if (n === 0) {
    return 1;
  } else {
    return n * fatorial(n - 1);
  }
}

// Definindo uma classe para manipulação de matrizes
class Matriz {
  constructor(linhas, colunas) {
    this.linhas = linhas;
    this.colunas = colunas;
    this.dados = [];
  
    // Inicializando a matriz com zeros
    for (let i = 0; i < linhas; i++) {
      this.dados[i] = [];
      for (let j = 0; j < colunas; j++) {
        this.dados[i][j] = 0;
      }
    }
  }
  
  // Multiplica a matriz por um escalar
  escalar(valor) {
    for (let i = 0; i < this.linhas; i++) {
      for (let j = 0; j < this.colunas; j++) {
        this.dados[i][j] *= valor;
      }
    }
  }
  
  // Soma duas matrizes
  somar(outraMatriz) {
    if (this.linhas !== outraMatriz.linhas || this.colunas !== outraMatriz.colunas) {
      throw new Error("As matrizes devem ter a mesma dimensão!");
    }
    
    for (let i = 0; i < this.linhas; i++) {
      for (let j = 0; j < this.colunas; j++) {
        this.dados[i][j] += outraMatriz.dados[i][j];
      }
    }
  }
  
  // Retorna uma representação em string da matriz
  toString() {
    let resultado = "";
    for (let i = 0; i < this.linhas; i++) {
      resultado += this.dados[i].join(" ") + "\n";
    }
    return resultado;
  }
}

// Utilizando as funções e classes definidas acima
const num = 5;
console.log(`O fatorial de ${num} é ${fatorial(num)}`);

const m1 = new Matriz(2, 3);
m1.dados = [[1, 2, 3], [4, 5, 6]];
console.log("Matriz 1:");
console.log(m1.toString());

const m2 = new Matriz(2, 3);
m2.dados = [[7, 8, 9], [10, 11, 12]];
console.log("Matriz 2:");
console.log(m2.toString());

m1.escalar(2);
console.log("Matriz 1 multiplicada por 2:");
console.log(m1.toString());

m1.somar(m2);
console.log("Matriz 1 somada com Matriz 2:");
console.log(m1.toString());
```

Nesse exemplo, começamos definindo uma função recursiva `fatorial()` que calcula o fatorial de um número.

Em seguida, definimos uma classe `Matriz` que permite a manipulação de matrizes. Essa classe possui métodos para multiplicar a matriz por um escalar, somar duas matrizes e retornar uma representação em string da matriz.

Por fim, utilizamos as funções e classes definidas para realizar algumas operações. Calculamos o fatorial de um número, criamos duas matrizes, multiplicamos uma delas por um escalar, somamos as duas matrizes e exibimos os resultados.

Espero que esse código atenda às suas expectativas! Note que, como é um código complexo, é possível que haja espaço para otimizações e melhorias.