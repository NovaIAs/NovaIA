Claro! Abaixo está um exemplo de código TypeScript que cria uma classe chamada "Calculadora" que implementa operações básicas de matemática.

```
class Calculadora {
  private valorAtual: number;

  constructor(valorInicial: number) {
    this.valorAtual = valorInicial;
  }

  somar(numero: number): void {
    this.valorAtual += numero;
  }

  subtrair(numero: number): void {
    this.valorAtual -= numero;
  }

  multiplicar(numero: number): void {
    this.valorAtual *= numero;
  }

  dividir(numero: number): void {
    if (numero !== 0) {
      this.valorAtual /= numero;
    } else {
      console.error("Não é possível dividir por zero!");
    }
  }

  obterResultado(): number {
    return this.valorAtual;
  }
}

// Uso da classe Calculadora
const minhaCalculadora = new Calculadora(10);
console.log("Valor inicial:", minhaCalculadora.obterResultado());

minhaCalculadora.somar(5);
console.log("Após somar 5:", minhaCalculadora.obterResultado());

minhaCalculadora.subtrair(3);
console.log("Após subtrair 3:", minhaCalculadora.obterResultado());

minhaCalculadora.multiplicar(4);
console.log("Após multiplicar por 4:", minhaCalculadora.obterResultado());

minhaCalculadora.dividir(2);
console.log("Após dividir por 2:", minhaCalculadora.obterResultado());
```

Neste exemplo, temos a classe "Calculadora" que permite realizar operações matemáticas básicas, como soma, subtração, multiplicação e divisão. O valor inicial da calculadora é definido no momento da criação do objeto.

Os métodos `somar`, `subtrair`, `multiplicar` e `dividir` atualizam o valor atual da calculadora de acordo com a operação desejada. O método `obterResultado` retorna o valor atual.

No uso da classe, criamos uma instância da Calculadora com o valor inicial de 10. Em seguida, realizamos algumas operações e exibimos o resultado após cada operação.

Esse é apenas um exemplo básico, mas a classe pode ser expandida para incluir mais funcionalidades ou melhorias. A orientação a objetos é uma abordagem poderosa para organizar e reutilizar código de forma eficiente.