Claro! Aqui está um exemplo de código em TypeScript que envolve o uso de classes, herança, interfaces e manipulação de dados:

```typescript
// Definindo uma interface para representar um veículo
interface IVeiculo {
  marca: string;
  modelo: string;
  ano: number;
  acelerar(): void;
  frear(): void;
}

// Classe base para veículos
abstract class Veiculo implements IVeiculo {
  marca: string;
  modelo: string;
  ano: number;

  constructor(marca: string, modelo: string, ano: number) {
    this.marca = marca;
    this.modelo = modelo;
    this.ano = ano;
  }

  acelerar(): void {
    console.log(`O veículo ${this.modelo} está acelerando.`);
  }

  frear(): void {
    console.log(`O veículo ${this.modelo} está freando.`);
  }
}

// Classe específica para carros
class Carro extends Veiculo {
  numeroPortas: number;

  constructor(marca: string, modelo: string, ano: number, numeroPortas: number) {
    super(marca, modelo, ano);
    this.numeroPortas = numeroPortas;
  }

  acelerar(): void {
    console.log(`O carro ${this.modelo} está acelerando.`);
  }

  frear(): void {
    console.log(`O carro ${this.modelo} está freando.`);
  }

  abrirPortas(): void {
    console.log(`As portas do carro ${this.modelo} estão abertas.`);
  }
}

// Classe específica para motos
class Moto extends Veiculo {
  cilindradas: number;

  constructor(marca: string, modelo: string, ano: number, cilindradas: number) {
    super(marca, modelo, ano);
    this.cilindradas = cilindradas;
  }

  acelerar(): void {
    console.log(`A moto ${this.modelo} está acelerando.`);
  }

  frear(): void {
    console.log(`A moto ${this.modelo} está freando.`);
  }

  empinar(): void {
    console.log(`A moto ${this.modelo} está empinando.`);
  }
}

// Utilização das classes
const meuCarro = new Carro("Ford", "Fiesta", 2021, 4);
meuCarro.acelerar(); // O carro Fiesta está acelerando.
meuCarro.frear(); // O carro Fiesta está freando.
meuCarro.abrirPortas(); // As portas do carro Fiesta estão abertas.

const minhaMoto = new Moto("Honda", "CBR 1000RR", 2021, 1000);
minhaMoto.acelerar(); // A moto CBR 1000RR está acelerando.
minhaMoto.frear(); // A moto CBR 1000RR está freando.
minhaMoto.empinar(); // A moto CBR 1000RR está empinando.
```

Nesse código, temos a definição de uma interface `IVeiculo` que define as propriedades e métodos que um veículo deve ter. Em seguida, temos uma classe abstrata `Veiculo` que implementa essa interface e fornece uma implementação básica dos métodos `acelerar` e `frear`. 

Em seguida, temos duas classes concretas `Carro` e `Moto`, que herdam da classe `Veiculo`. Essas classes especializam-se em carros e motos respectivamente, adicionando propriedades e métodos específicos para cada tipo de veículo.

Por fim, temos o código de utilização das classes, onde criamos uma instância de `Carro` e `Moto`, e invocamos seus métodos.