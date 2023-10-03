Claro! Aqui está um código em TypeScript que utiliza a recursão para calcular o fatorial de um número:

```typescript
function fatorial(n: number): number {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    return n * fatorial(n - 1);
  }
}

const numero = 5;
const resultado = fatorial(numero);
console.log(`O fatorial de ${numero} é ${resultado}.`);
```

Neste código, temos uma função chamada `fatorial` que recebe um número `n` como parâmetro e retorna o fatorial desse número. 

A função utiliza a recursão para calcular o fatorial. Ela verifica se o número é igual a 0 ou 1, e caso seja, retorna 1, pois o fatorial de 0 e 1 é sempre 1. Caso contrário, a função chama a si mesma, passando `n - 1` como parâmetro, e multiplica o número `n` pelo resultado da chamada recursiva.

No exemplo, o código calcula o fatorial do número 5 e armazena o resultado na variável `resultado`. Em seguida, imprime uma mensagem no console mostrando o número original e o resultado do fatorial.

Esse código é um exemplo simples de como utilizar a recursão para calcular o fatorial de um número em TypeScript. A recursão é uma técnica poderosa na programação e pode ser utilizada em diversos contextos para resolver problemas de forma elegante e eficiente.