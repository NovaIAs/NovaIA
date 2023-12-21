Claro! Aqui está um exemplo de código complexo em TypeScript:

```typescript
// Classe abstrata que representa uma forma geométrica
abstract class FormaGeometrica {
  abstract calcularArea(): number;
  abstract calcularPerimetro(): number;
  abstract desenhar(): void;
}

// Classe que representa um retângulo
class Retangulo extends FormaGeometrica {
  constructor(private altura: number, private largura: number) {
    super();
  }

  calcularArea(): number {
    return this.altura * this.largura;
  }

  calcularPerimetro(): number {
    return 2 * (this.altura + this.largura);
  }

  desenhar(): void {
    console.log(`Desenhando um retângulo com altura ${this.altura} e largura ${this.largura}`);
  }
}

// Classe que representa um círculo
class Circulo extends FormaGeometrica {
  constructor(private raio: number) {
    super();
  }

  calcularArea(): number {
    return Math.PI * this.raio ** 2;
  }

  calcularPerimetro(): number {
    return 2 * Math.PI * this.raio;
  }

  desenhar(): void {
    console.log(`Desenhando um círculo com raio ${this.raio}`);
  }
}

// Função que recebe um array de formas geométricas e exibe suas informações
function exibirInformacoesFormas(formas: FormaGeometrica[]): void {
  formas.forEach((forma) => {
    forma.desenhar();
    console.log(`Área: ${forma.calcularArea()}`);
    console.log(`Perímetro: ${forma.calcularPerimetro()}`);
    console.log();
  });
}

// Criando instâncias das formas geométricas
const retangulo = new Retangulo(5, 10);
const circulo = new Circulo(3);
const formas = [retangulo, circulo];

// Exibindo as informações das formas geométricas
exibirInformacoesFormas(formas);
```

Neste código, temos uma estrutura orientada a objetos em TypeScript. Primeiro, temos uma classe abstrata `FormaGeometrica`, que possui três métodos abstratos (`calcularArea`, `calcularPerimetro` e `desenhar`). Essa classe serve como base para as classes `Retangulo` e `Circulo`, que herdam dela.

A classe `Retangulo` representa um retângulo e possui dois atributos: `altura` e `largura`. Ela implementa os métodos abstratos da classe `FormaGeometrica`, fornecendo as fórmulas corretas para calcular a área e o perímetro de um retângulo, além de um método `desenhar` que exibe uma mensagem informando a altura e a largura do retângulo.

A classe `Circulo` representa um círculo e possui um atributo `raio`. Ela também implementa os métodos abstratos da classe `FormaGeometrica`, fornecendo as fórmulas corretas para calcular a área e o perímetro de um círculo, além do método `desenhar` que exibe uma mensagem informando o raio do círculo.

A função `exibirInformacoesFormas` recebe um array de formas geométricas e itera sobre ele, chamando o método `desenhar` de cada forma e exibindo as informações de área e perímetro.

No exemplo final, são criadas instâncias de um retângulo e de um círculo, e essas formas são adicionadas a um array. Em seguida, chamamos a função `exibirInformacoesFormas` passando esse array como argumento. O resultado será a exibição das informações de cada forma geométrica.