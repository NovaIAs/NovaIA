Claro! Aqui está um exemplo de código em TypeScript que gera uma sequência Fibonacci e verifica se os números são primos:

```typescript
function gerarSequenciaFibonacci(n: number): number[] {
  const sequencia: number[] = [];
  
  for (let i = 0; i < n; i++) {
    if (i < 2) {
      sequencia.push(i);
    } else {
      sequencia.push(sequencia[i - 1] + sequencia[i - 2]);
    }
  }
  
  return sequencia;
}

function isPrimo(numero: number): boolean {
  if (numero <= 1) {
    return false;
  }
  
  for (let i = 2; i < numero; i++) {
    if (numero % i === 0) {
      return false;
    }
  }
  
  return true;
}

function verificarPrimos(sequencia: number[]): number[] {
  const primos: number[] = [];
  
  for (const numero of sequencia) {
    if (isPrimo(numero)) {
      primos.push(numero);
    }
  }
  
  return primos;
}

const n = 10;
const sequenciaFibonacci = gerarSequenciaFibonacci(n);
const primosNaSequencia = verificarPrimos(sequenciaFibonacci);

console.log(`Sequência de Fibonacci com ${n} números: ${sequenciaFibonacci.join(', ')}`);
console.log(`Números primos na sequência: ${primosNaSequencia.join(', ')}`);
```

Neste código, temos três funções principais:

1. A função `gerarSequenciaFibonacci` recebe um número `n` como parâmetro e retorna uma matriz com os `n` primeiros números da sequência de Fibonacci.
2. A função `isPrimo` verifica se um número é primo. Retorna `true` se o número for primo e `false` caso contrário.
3. A função `verificarPrimos` recebe uma sequência de números e retorna uma matriz com os números primos encontrados nessa sequência.

No exemplo, definimos um valor `n` igual a 10. A sequência de Fibonacci com 10 números será gerada e armazenada na variável `sequenciaFibonacci`. Em seguida, a função `verificarPrimos` é chamada para encontrar os números primos na sequência e armazená-los na variável `primosNaSequencia`. Por fim, os resultados são exibidos no console.

Espero que isso atenda aos seus requisitos de complexidade e originalidade!