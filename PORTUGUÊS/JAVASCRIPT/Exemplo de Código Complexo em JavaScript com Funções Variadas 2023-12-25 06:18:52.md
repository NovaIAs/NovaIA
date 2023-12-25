Claro! Aqui está um exemplo de um código complexo em JavaScript que realiza uma série de tarefas diferentes:

```javascript
// Função para calcular o fatorial de um número
function fatorial(n) {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    return n * fatorial(n - 1);
  }
}

// Função para verificar se um número é primo
function verificarPrimo(num) {
  if (num <= 1) {
    return false;
  }
  for (let i = 2; i < num; i++) {
    if (num % i === 0) {
      return false;
    }
  }
  return true;
}

// Função para inverter uma string
function inverterString(str) {
  return str.split('').reverse().join('');
}

// Função para remover elementos duplicados de um array
function removerDuplicados(array) {
  return [...new Set(array)];
}

// Função para encontrar o maior número em um array
function encontrarMaiorNumero(array) {
  return Math.max(...array);
}

// Exemplo de uso das funções
const numero = 5;
console.log(`O fatorial de ${numero} é ${fatorial(numero)}`);

const numeroPrimo = 13;
console.log(`${numeroPrimo} é primo? ${verificarPrimo(numeroPrimo)}`);

const string = 'Olá, Mundo!';
console.log(`A string invertida é: ${inverterString(string)}`);

const numeros = [1, 2, 3, 4, 5, 3, 2, 1];
console.log(`Array sem elementos duplicados: ${removerDuplicados(numeros)}`);

const numeros2 = [10, 5, 20, 15, 8];
console.log(`O maior número no array é: ${encontrarMaiorNumero(numeros2)}`);
```

Neste código, temos diversas funções que realizam diferentes tarefas:

1. A função `fatorial` calcula o fatorial de um número utilizando recursão.
2. A função `verificarPrimo` verifica se um número é primo.
3. A função `inverterString` recebe uma string e retorna a mesma invertida.
4. A função `removerDuplicados` recebe um array e remove os elementos duplicados.
5. A função `encontrarMaiorNumero` recebe um array e retorna o maior número presente nele.

Após as definições das funções, há exemplos de uso das mesmas, onde diferentes valores são passados como argumentos e os resultados são exibidos no console.

Espero que este código complexo em JavaScript atenda às suas necessidades!