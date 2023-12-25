Claro! Aqui está um código complexo em TypeScript:

```typescript
// Classe abstrata para representar uma forma geométrica
abstract class FormaGeometrica {
  abstract calcularArea(): number;
  abstract calcularPerimetro(): number;
  abstract desenhar(): void;
}

// Classe para representar um retângulo
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

  desenhar(): void {
    let desenho: string = '';

    for (let i = 0; i < this.altura; i++) {
      for (let j = 0; j < this.largura; j++) {
        desenho += '* ';
      }
      desenho += '\n';
    }

    console.log(desenho);
  }
}

// Classe para representar um círculo
class Circulo extends FormaGeometrica {
  constructor(private raio: number) {
    super();
  }

  calcularArea(): number {
    return Math.PI * this.raio * this.raio;
  }

  calcularPerimetro(): number {
    return 2 * Math.PI * this.raio;
  }

  desenhar(): void {
    const diametro = 2 * this.raio;
    const raioInterno = this.raio * 0.8;
    const raioExterno = this.raio * 1.2;

    let desenho: string = '';

    for (let i = 0; i < diametro; i++) {
      for (let j = 0; j < diametro; j++) {
        const distancia = Math.sqrt((i - this.raio) ** 2 + (j - this.raio) ** 2);
        if (distancia > raioInterno && distancia < raioExterno) {
          desenho += '* ';
        } else {
          desenho += '  ';
        }
      }
      desenho += '\n';
    }

    console.log(desenho);
  }
}

// Cria uma instância de Retangulo
const retangulo = new Retangulo(5, 3);
console.log('Retangulo:');
console.log('Área:', retangulo.calcularArea());
console.log('Perímetro:', retangulo.calcularPerimetro());
console.log('Desenho:');
retangulo.desenhar();

// Cria uma instância de Circulo
const circulo = new Circulo(5);
console.log('Círculo:');
console.log('Área:', circulo.calcularArea());
console.log('Perímetro:', circulo.calcularPerimetro());
console.log('Desenho:');
circulo.desenhar();
```

Este código em TypeScript cria duas classes: `Retangulo` e `Circulo`, que herdam da classe abstrata `FormaGeometrica`. A classe `Retangulo` representa um retângulo e possui métodos para calcular a área, o perímetro e desenhar o retângulo utilizando asteriscos. A classe `Circulo` representa um círculo e possui métodos para calcular a área, o perímetro e desenhar o círculo utilizando asteriscos. O código também cria instâncias das duas classes e exibe os resultados dos cálculos e os desenhos na saída do console.