Claro! Aqui está um código complexo em TypeScript que faz uso de classes, interfaces e recursos avançados da linguagem.

```typescript
// Definição da interface 'Forma'
interface Forma {
  calcularArea(): number;
  calcularPerimetro(): number;
}

// Classe abstrata 'FormaGeometrica' que implementa a interface 'Forma'
abstract class FormaGeometrica implements Forma {
  abstract calcularArea(): number;
  abstract calcularPerimetro(): number;
}

// Classe 'Retangulo' que herda da classe abstrata 'FormaGeometrica'
class Retangulo extends FormaGeometrica {
  constructor(private largura: number, private altura: number) {
    super();
  }

  calcularArea(): number {
    return this.largura * this.altura;
  }

  calcularPerimetro(): number {
    return 2 * (this.largura + this.altura);
  }
}

// Classe 'Circulo' que herda da classe abstrata 'FormaGeometrica'
class Circulo extends FormaGeometrica {
  constructor(private raio: number) {
    super();
  }

  calcularArea(): number {
    return Math.PI * Math.pow(this.raio, 2);
  }

  calcularPerimetro(): number {
    return 2 * Math.PI * this.raio;
  }
}

// Classe 'Triangulo' que herda da classe abstrata 'FormaGeometrica'
class Triangulo extends FormaGeometrica {
  constructor(private base: number, private altura: number) {
    super();
  }

  calcularArea(): number {
    return (this.base * this.altura) / 2;
  }

  calcularPerimetro(): number {
    // O perímetro de um triângulo não pode ser calculado apenas com a base e altura
    throw new Error("Operação não suportada: cálculo do perímetro de um triângulo");
  }
}

// Função para calcular a área total das formas geométricas
function calcularAreaTotal(formas: Forma[]): number {
  let areaTotal = 0;
  for (const forma of formas) {
    areaTotal += forma.calcularArea();
  }
  return areaTotal;
}

// Função para calcular o perímetro total das formas geométricas
function calcularPerimetroTotal(formas: Forma[]): number {
  let perimetroTotal = 0;
  for (const forma of formas) {
    perimetroTotal += forma.calcularPerimetro();
  }
  return perimetroTotal;
}

// Exemplo de uso das classes e funções
const retangulo = new Retangulo(5, 3);
const circulo = new Circulo(4);
const triangulo = new Triangulo(6, 2);

const formas: Forma[] = [retangulo, circulo, triangulo];

const areaTotal = calcularAreaTotal(formas);
const perimetroTotal = calcularPerimetroTotal(formas);

console.log("Área total das formas geométricas:", areaTotal);
console.log("Perímetro total das formas geométricas:", perimetroTotal);
```

Neste código, utilizamos a linguagem TypeScript para criar uma hierarquia de classes que representam diferentes formas geométricas (retângulo, círculo e triângulo). Essas classes implementam a interface 'Forma', que define os métodos 'calcularArea()' e 'calcularPerimetro()' para calcular a área e perímetro dessas formas, respectivamente.

A classe abstrata 'FormaGeometrica' serve como uma base para as classes concretas, implementando a interface 'Forma' e fornecendo um esqueleto básico para os cálculos. Cada classe concreta (Retangulo, Circulo e Triangulo) herda da classe abstrata e implementa os métodos específicos para cada forma.

Além disso, temos duas funções auxiliares: 'calcularAreaTotal()' e 'calcularPerimetroTotal()', que recebem um array de formas e calculam a área e o perímetro total, respectivamente, somando os valores de cada forma.

No exemplo de uso no final do código, criamos uma instância de cada forma geométrica, adicionamos todas as formas em um array e chamamos as funções auxiliares para calcular a área e o perímetro total das formas. Os resultados são exibidos no console.

Espero que esse código seja útil para você!