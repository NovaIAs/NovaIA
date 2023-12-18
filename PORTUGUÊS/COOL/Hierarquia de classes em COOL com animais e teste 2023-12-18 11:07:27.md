Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language), um linguagem de programação orientada a objetos desenvolvida na Universidade de Berkeley:

```cool
class Animal {

    attribute nome : String;
    attribute idade : Int;
    attribute tipo : String;

    method init(n : String, i : Int, t : String) : SELF_TYPE {
        nome <- n;
        idade <- i;
        tipo <- t;
        return self;
    };

    method comer() : String {
        return "O animal " + nome + " está comendo.";
    };

    method dormir() : String {
        return "O animal " + nome + " está dormindo.";
    };

    method fazerBarulho() : String {
        return "O animal " + nome + " está fazendo barulho.";
    };
};

class Cachorro inherits Animal {

    method fazerBarulho() : String {
        return "O cachorro " + nome + " está latindo.";
    };
};

class Gato inherits Animal {

    method fazerBarulho() : String {
        return "O gato " + nome + " está miando.";
    };
};

class Teste {

    method main() : Object {
        let animal1 : Animal <- new Animal.init("Rex", 5, "Cachorro");
        let animal2 : Animal <- new Animal.init("Mia", 3, "Gato");
        let animal3 : Cachorro <- new Cachorro.init("Rufus", 2, "Cachorro");
        let animal4 : Gato <- new Gato.init("Luna", 1, "Gato");

        out_string(animal1.comer());
        out_string(animal2.dormir());
        out_string(animal3.fazerBarulho());
        out_string(animal4.fazerBarulho());

        return void;
    };
};
```

Este código em COOL apresenta uma hierarquia de classes onde temos a classe `Animal` como classe base, e as classes `Cachorro` e `Gato` herdam dela. Cada classe tem atributos específicos, como `nome`, `idade` e `tipo`, e também métodos específicos, como `comer`, `dormir` e `fazerBarulho`.

Na classe `Animal`, temos o método `init` que inicializa os atributos da classe e retorna uma instância da classe. Os outros métodos simplesmente retornam uma string indicando a ação do animal.

Nas classes `Cachorro` e `Gato`, o método `fazerBarulho` é sobrescrito para fornecer uma descrição específica do barulho feito por cada animal.

Na classe `Teste`, o método `main` cria instâncias das classes `Animal`, `Cachorro` e `Gato` e chama seus respectivos métodos para imprimir as ações dos animais no console.

Espero que este código complexo em COOL atenda às suas expectativas e demonstre a linguagem de maneira interessante!