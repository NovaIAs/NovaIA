Claro! Aqui está um exemplo de código complexo em TypeScript:

```typescript
// Classe para representar um veículo
class Veiculo {
  private marca: string;
  private modelo: string;
  private ano: number;
  private velocidade: number;

  constructor(marca: string, modelo: string, ano: number) {
    this.marca = marca;
    this.modelo = modelo;
    this.ano = ano;
    this.velocidade = 0;
  }

  acelerar() {
    this.velocidade += 10;
    console.log(`O ${this.modelo} ${this.marca} está acelerando. Velocidade atual: ${this.velocidade} km/h`);
  }

  frear() {
    if (this.velocidade > 0) {
      this.velocidade -= 10;
      console.log(`O ${this.modelo} ${this.marca} está freando. Velocidade atual: ${this.velocidade} km/h`);
    } else {
      console.log(`O ${this.modelo} ${this.marca} já está parado`);
    }
  }
}

// Classe para representar um carro, que herda de Veiculo
class Carro extends Veiculo {
  private numeroPortas: number;

  constructor(marca: string, modelo: string, ano: number, numeroPortas: number) {
    super(marca, modelo, ano);
    this.numeroPortas = numeroPortas;
  }

  ligar() {
    console.log(`O ${this.modelo} ${this.marca} está ligando...`);
    console.log(`O ${this.modelo} ${this.marca} está pronto para uso!`);
  }
}

// Classe para representar uma moto, que herda de Veiculo
class Moto extends Veiculo {
  private cilindradas: number;

  constructor(marca: string, modelo: string, ano: number, cilindradas: number) {
    super(marca, modelo, ano);
    this.cilindradas = cilindradas;
  }

  empinar() {
    console.log(`A ${this.modelo} ${this.marca} está empinando! Cuidado!`);
  }
}

// Uso das classes
const meuCarro = new Carro("Ford", "Fusion", 2020, 4);
meuCarro.ligar();
meuCarro.acelerar();
meuCarro.frear();

const minhaMoto = new Moto("Honda", "CBR 1000RR", 2021, 1000);
minhaMoto.acelerar();
minhaMoto.empinar();
```

Neste exemplo, temos a definição de três classes em TypeScript: `Veiculo`, `Carro` e `Moto`. A classe `Veiculo` é a classe base que contém propriedades e métodos comuns a todos os veículos, como marca, modelo, ano e velocidade. As classes `Carro` e `Moto` herdam da classe `Veiculo` e adicionam propriedades e métodos específicos para carros e motos, respectivamente.

No exemplo, criamos uma instância de um carro e uma instância de uma moto, definimos suas características e chamamos alguns métodos, como `ligar`, `acelerar`, `frear` e `empinar`. Os métodos exibem mensagens no console para informar as ações realizadas pelo veículo.

Espero que este exemplo atenda às suas expectativas!