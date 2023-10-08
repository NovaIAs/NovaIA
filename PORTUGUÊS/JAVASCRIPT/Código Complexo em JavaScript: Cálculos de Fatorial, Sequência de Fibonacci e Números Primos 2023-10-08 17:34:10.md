Claro! Aqui está um exemplo de um código complexo em JavaScript:

```javascript
// Função para calcular o fatorial de um número
function calcularFatorial(numero) {
  if (numero === 0 || numero === 1) {
    return 1;
  } else {
    return numero * calcularFatorial(numero - 1);
  }
}

// Função para calcular a sequência de Fibonacci até um determinado limite
function calcularSequenciaFibonacci(limite) {
  let sequencia = [0, 1];
  
  for (let i = 2; i <= limite; i++) {
    sequencia.push(sequencia[i - 1] + sequencia[i - 2]);
  }
  
  return sequencia;
}

// Função para verificar se um número é primo
function verificarNumeroPrimo(numero) {
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

// Função principal
function main() {
  const numero = 10;
  
  console.log(`O fatorial de ${numero} é: ${calcularFatorial(numero)}`);
  
  const limiteFibonacci = 10;
  const sequenciaFibonacci = calcularSequenciaFibonacci(limiteFibonacci);
  
  console.log(`A sequência de Fibonacci até ${limiteFibonacci} é:`);
  console.log(sequenciaFibonacci);
  
  const numeroPrimo = 17;
  
  if (verificarNumeroPrimo(numeroPrimo)) {
    console.log(`${numeroPrimo} é um número primo.`);
  } else {
    console.log(`${numeroPrimo} não é um número primo.`);
  }
}

// Chamada da função principal
main();
```

Este código contém três funções: `calcularFatorial`, `calcularSequenciaFibonacci` e `verificarNumeroPrimo`, e uma função `main` que é a função principal que chama as outras funções e exibe os resultados no console.

A função `calcularFatorial` é uma função recursiva que recebe um número como parâmetro e retorna o fatorial desse número. Ela usa uma condição de parada quando o número for 0 ou 1, e caso contrário, chama a função novamente com um número decrementado e multiplica pelo número atual.

A função `calcularSequenciaFibonacci` recebe um limite como parâmetro e retorna um array com a sequência de Fibonacci até o limite especificado. A função utiliza um loop for para calcular cada número da sequência somando os dois números anteriores.

A função `verificarNumeroPrimo` recebe um número como parâmetro e retorna um valor booleano indicando se o número é primo ou não. A função utiliza um loop for para verificar se o número é divisível por algum número além de 1 e ele próprio.

A função `main` é a função principal do código, onde são definidas as constantes `numero`, `limiteFibonacci` e `numeroPrimo`. Em seguida, ela chama as outras funções passando os valores dessas constantes como parâmetros e exibe os resultados no console.

Espero que este código complexo em JavaScript atenda às suas expectativas!