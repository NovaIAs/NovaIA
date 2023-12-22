Claro! Aqui está um código complexo em TypeScript que envolve o uso de classes, herança, interfaces, métodos assíncronos e manipulação de arquivos:

```typescript
import fs from 'fs';

interface Animal {
  nome: string;
  idade: number;
  som(): void;
}

class AnimalBase {
  constructor(public nome: string, public idade: number) {}

  som(): void {
    console.log('Fazendo som...');
  }
}

class Cachorro extends AnimalBase implements Animal {
  constructor(nome: string, idade: number) {
    super(nome, idade);
  }

  som(): void {
    console.log('Au Au!');
  }
}

class Gato extends AnimalBase implements Animal {
  constructor(nome: string, idade: number) {
    super(nome, idade);
  }

  som(): void {
    console.log('Miau!');
  }
}

class PetShop {
  private animais: Animal[];

  constructor() {
    this.animais = [];
  }

  adicionarAnimal(animal: Animal): void {
    this.animais.push(animal);
  }

  async salvarAnimaisEmArquivo(nomeArquivo: string): Promise<void> {
    try {
      const animaisJSON = JSON.stringify(this.animais);
      await fs.promises.writeFile(nomeArquivo, animaisJSON);
      console.log('Animais salvos com sucesso!');
    } catch (error) {
      console.error('Erro ao salvar animais:', error);
    }
  }

  async carregarAnimaisDoArquivo(nomeArquivo: string): Promise<void> {
    try {
      const animaisJSON = await fs.promises.readFile(nomeArquivo, 'utf-8');
      const animais = JSON.parse(animaisJSON);
      this.animais = animais;
      console.log('Animais carregados com sucesso!');
    } catch (error) {
      console.error('Erro ao carregar animais:', error);
    }
  }

  listarAnimais(): void {
    console.log('Lista de animais:');
    this.animais.forEach((animal) => {
      console.log(`- ${animal.nome}, ${animal.idade} anos`);
    });
  }
}

async function main(): Promise<void> {
  const petShop = new PetShop();

  const cachorro = new Cachorro('Rex', 3);
  const gato = new Gato('Felix', 2);

  petShop.adicionarAnimal(cachorro);
  petShop.adicionarAnimal(gato);

  await petShop.salvarAnimaisEmArquivo('animais.json');

  await petShop.carregarAnimaisDoArquivo('animais.json');

  petShop.listarAnimais();
}

main().catch((error) => console.error('Erro na execução:', error));
```

Neste código, temos a definição de três classes: `AnimalBase`, `Cachorro` e `Gato`. A classe `AnimalBase` é uma classe abstrata que serve como base para as classes `Cachorro` e `Gato`, implementando a interface `Animal`. As classes `Cachorro` e `Gato` herdam da classe `AnimalBase` e sobrescrevem o método `som`, definindo o som específico de cada animal.

Temos também a classe `PetShop`, que possui um array de animais e métodos para adicionar animais, salvar os animais em um arquivo, carregar os animais de um arquivo e listar os animais. Os métodos `salvarAnimaisEmArquivo` e `carregarAnimaisDoArquivo` utilizam a biblioteca `fs` para manipulação de arquivos.

No final, no bloco `main`, criamos uma instância do `PetShop`, criamos um cachorro e um gato, adicionamos esses animais ao pet shop, salvamos os animais em um arquivo chamado "animais.json", carregamos os animais desse arquivo e, por fim, listamos os animais na saída do console.

Espero que este código atenda aos seus requisitos!