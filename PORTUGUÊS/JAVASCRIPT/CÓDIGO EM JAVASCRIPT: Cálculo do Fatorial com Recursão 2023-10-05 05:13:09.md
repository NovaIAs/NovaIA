Claro! Aqui está um exemplo de um código complexo em JavaScript que usa recursão para calcular o fatorial de um número:

```javascript
function calcularFatorial(n) {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    return n * calcularFatorial(n - 1);
  }
}

function imprimirFatorial(numero) {
  console.log(`O fatorial de ${numero} é: ${calcularFatorial(numero)}`);
}

imprimirFatorial(5);
```

Neste código, temos duas funções: `calcularFatorial` e `imprimirFatorial`.

A função `calcularFatorial` é uma função recursiva que recebe um número `n` como parâmetro. Ela verifica se `n` é igual a 0 ou 1, pois nesses casos o fatorial é sempre 1. Caso contrário, ela chama a função `calcularFatorial` novamente, passando `n - 1` como argumento, e multiplica `n` pelo resultado retornado pela chamada recursiva.

A função `imprimirFatorial` recebe um número como parâmetro e chama a função `calcularFatorial` para calcular o fatorial desse número. Em seguida, ela imprime no console o número e seu respectivo fatorial.

No exemplo acima, a função `imprimirFatorial(5)` é chamada, o que resulta na impressão do seguinte resultado no console: "O fatorial de 5 é: 120".

Esse código utiliza a recursão para calcular o fatorial, o que significa que a função é chamada várias vezes até que a condição de parada (n === 0 ou n === 1) seja atingida. Essa implementação é um exemplo de algoritmo recursivo eficiente para calcular o fatorial de um número em JavaScript.