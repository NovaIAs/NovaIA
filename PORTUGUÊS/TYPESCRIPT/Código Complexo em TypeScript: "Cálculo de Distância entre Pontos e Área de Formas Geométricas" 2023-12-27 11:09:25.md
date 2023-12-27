Claro! Aqui está um código complexo em TypeScript:

```
// Classe Ponto representa um ponto no espaço bidimensional
class Ponto {
  constructor(public x: number, public y: number) {}

  // Método de instância para calcular a distância entre dois pontos
  calcularDistancia(outroPonto: Ponto): number {
    const diferencaX = outroPonto.x - this.x;
    const diferencaY = outroPonto.y - this.y;
    return Math.sqrt(diferencaX ** 2 + diferencaY ** 2);
  }
}

// Classe Forma representa uma forma geométrica genérica
abstract class Forma {
  constructor(public cor: string) {}

  // Método abstrato para calcular a área da forma
  abstract calcularArea(): number;

  // Método de instância para imprimir informações da forma
  imprimirInformacoes(): void {
    console.log(`Cor: ${this.cor}`);
    console.log(`Área: ${this.calcularArea()}`);
  }
}

// Classe Retangulo representa um retângulo
class Retangulo extends Forma {
  constructor(public largura: number, public altura: number, cor: string) {
    super(cor);
  }

  // Implementação do método calcularArea para retângulo
  calcularArea(): number {
    return this.largura * this.altura;
  }
}

// Classe Circulo representa um círculo
class Circulo extends Forma {
  constructor(public raio: number, cor: string) {
    super(cor);
  }

  // Implementação do método calcularArea para círculo
  calcularArea(): number {
    return Math.PI * this.raio ** 2;
  }
}

// Função para calcular a área total das formas em um array
function calcularAreaTotal(formas: Forma[]): number {
  let areaTotal = 0;
  for (const forma of formas) {
    areaTotal += forma.calcularArea();
  }
  return areaTotal;
}

// Exemplo de uso das classes e função
const pontoA = new Ponto(0, 0);
const pontoB = new Ponto(3, 4);
const retangulo = new Retangulo(5, 10, 'vermelho');
const circulo = new Circulo(7, 'azul');
const formas: Forma[] = [retangulo, circulo];
const areaTotal = calcularAreaTotal(formas);

console.log('Distância entre os pontos A e B:', pontoA.calcularDistancia(pontoB));
console.log('Área total das formas:', areaTotal);
```

Neste código, temos a definição das classes `Ponto`, `Forma`, `Retangulo` e `Circulo`. A classe `Ponto` representa um ponto no espaço bidimensional e possui um método para calcular a distância entre dois pontos. A classe abstrata `Forma` representa uma forma geométrica genérica, com uma propriedade de cor e um método abstrato para calcular a área. As classes `Retangulo` e `Circulo` herdam da classe `Forma` e implementam o método `calcularArea` conforme suas próprias fórmulas. 

Temos também a função `calcularAreaTotal`, que recebe um array de formas e calcula a área total somando as áreas de cada forma.

No exemplo de uso, criamos um ponto A e um ponto B, calculamos a distância entre eles usando o método da classe `Ponto`. Em seguida, criamos um retângulo e um círculo, adicionamos essas formas em um array e calculamos a área total usando a função `calcularAreaTotal`. Por fim, imprimimos as distâncias e a área total no console.