Claro! Aqui está um exemplo de um código complexo em TypeScript:

```typescript
// Classe base que representa um animal
class Animal {
    private nome: string;
    private idade: number;

    constructor(nome: string, idade: number) {
        this.nome = nome;
        this.idade = idade;
    }

    public emitirSom(): void {
        console.log("O animal está emitindo um som.");
    }
}

// Classe que representa um cachorro, que é uma subclasse de Animal
class Cachorro extends Animal {
    private raca: string;

    constructor(nome: string, idade: number, raca: string) {
        super(nome, idade);
        this.raca = raca;
    }

    public latir(): void {
        console.log("O cachorro está latindo.");
    }
}

// Classe que representa um gato, que é uma subclasse de Animal
class Gato extends Animal {
    private cor: string;

    constructor(nome: string, idade: number, cor: string) {
        super(nome, idade);
        this.cor = cor;
    }

    public miar(): void {
        console.log("O gato está miando.");
    }
}

// Função que cria um array de animais e realiza ações com eles
function criarAnimais(): void {
    const animais: Animal[] = [
        new Cachorro("Rex", 3, "Labrador"),
        new Gato("Bolinha", 2, "Preto"),
        new Cachorro("Fido", 5, "Vira-lata"),
        new Gato("Mimi", 1, "Branco")
    ];

    // Itera sobre os animais e realiza ações específicas de cada um
    animais.forEach(animal => {
        console.log(`Nome: ${animal.nome}`);
        console.log(`Idade: ${animal.idade}`);

        if (animal instanceof Cachorro) {
            animal.emitirSom();
            animal.latir();
        } else if (animal instanceof Gato) {
            animal.emitirSom();
            animal.miar();
        }

        console.log("---");
    });
}

// Chama a função para criar os animais e realizar as ações
criarAnimais();
```

Neste exemplo, temos uma hierarquia de classes em que a classe `Animal` é a classe base, e as classes `Cachorro` e `Gato` são subclasses de `Animal`. Cada uma dessas classes tem propriedades e métodos específicos.

A função `criarAnimais` cria um array de animais, que contém instâncias de `Cachorro` e `Gato`, e itera sobre cada um deles realizando ações específicas para cada tipo de animal. A ação específica é determinada utilizando o operador `instanceof` para verificar se a instância é de um determinado tipo.

Esse código demonstra o uso de classes, herança, polimorfismo e verificação de tipos em TypeScript, permitindo uma abordagem orientada a objetos para modelar diferentes tipos de animais e realizar ações específicas para cada um deles.