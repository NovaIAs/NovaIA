Claro! Aqui está um exemplo de um código complexo em JavaScript:

```javascript
// Função para verificar se um número é primo
function verificarPrimo(numero) {
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

// Função para gerar uma sequência de números primos
function gerarSequenciaPrimos(quantidade) {
  let sequencia = [];
  let numeroAtual = 2;
  
  while (sequencia.length < quantidade) {
    if (verificarPrimo(numeroAtual)) {
      sequencia.push(numeroAtual);
    }
    numeroAtual++;
  }
  
  return sequencia;
}

// Função para calcular o fatorial de um número
function calcularFatorial(numero) {
  let fatorial = 1;
  
  for (let i = 2; i <= numero; i++) {
    fatorial *= i;
  }
  
  return fatorial;
}

// Função para calcular o número de combinações possíveis
function calcularCombinacoes(n, r) {
  let numerador = calcularFatorial(n);
  let denominador = calcularFatorial(r) * calcularFatorial(n - r);
  
  return numerador / denominador;
}

// Função para imprimir uma tabela de combinações
function imprimirTabelaCombinacoes(maiorNumero) {
  let tabela = "";
  
  for (let i = 0; i <= maiorNumero; i++) {
    let linha = "";
    
    for (let j = 0; j <= i; j++) {
      linha += calcularCombinacoes(i, j) + " ";
    }
    
    tabela += linha.trim() + "\n";
  }
  
  console.log(tabela);
}

// Testando as funções
let quantidadePrimos = 10;
let sequenciaPrimos = gerarSequenciaPrimos(quantidadePrimos);
console.log("Sequência de " + quantidadePrimos + " números primos: " + sequenciaPrimos.join(", "));

let maiorNumeroTabela = 5;
console.log("Tabela de combinações até o número " + maiorNumeroTabela + ":");
imprimirTabelaCombinacoes(maiorNumeroTabela);
```

Este código possui várias funcionalidades interessantes. Primeiro, temos a função `verificarPrimo(numero)`, que recebe um número como parâmetro e retorna `true` se o número for primo ou `false` caso contrário.

Em seguida, temos a função `gerarSequenciaPrimos(quantidade)`, que recebe a quantidade de números primos a serem gerados e retorna uma sequência desses números. Essa função utiliza a função `verificarPrimo(numero)` para verificar se cada número é primo.

Também temos a função `calcularFatorial(numero)`, que recebe um número como parâmetro e retorna o fatorial desse número.

A função `calcularCombinacoes(n, r)` calcula o número de combinações possíveis de r elementos em um conjunto de n elementos. Ela utiliza a função `calcularFatorial(numero)` para calcular os fatoriais necessários.

A função `imprimirTabelaCombinacoes(maiorNumero)` imprime uma tabela de combinações até o número especificado. Ela utiliza a função `calcularCombinacoes(n, r)` para calcular cada valor da tabela.

Por fim, são realizados testes das funções, gerando uma sequência de 10 números primos e imprimindo uma tabela de combinações até o número 5.

Espero que isso tenha atendido às suas expectativas! Se você tiver alguma dúvida ou precisar de mais ajuda, é só me avisar.