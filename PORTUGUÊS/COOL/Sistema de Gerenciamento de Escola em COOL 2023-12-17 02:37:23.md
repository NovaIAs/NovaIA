Claro! Aqui está um código complexo em COOL (Classroom Object Oriented Language) que implementa um sistema de gerenciamento de escola, com classes para representar alunos, professores, disciplinas e turmas. Vou explicar o código linha por linha para que você possa entender como ele funciona:

```
class Pessoa {
    atributo nome : String;
    atributo idade : Int;
    
    metodo inicializar(nome : String, idade : Int) : Pessoa {
        self.nome <- nome;
        self.idade <- idade;
        return self;
    };
    
    metodo imprimir() : Object {
        out_string(nome);
        out_string(", ");
        out_int(idade);
        out_string("\n");
        return self;
    };
};

class Aluno : Pessoa {
    atributo numeroMatricula : Int;
    atributo turma : Turma;
    
    metodo inicializar(nome : String, idade : Int, numeroMatricula : Int, turma : Turma) : Aluno {
        super.inicializar(nome, idade);
        self.numeroMatricula <- numeroMatricula;
        self.turma <- turma;
        return self;
    };
    
    metodo imprimir() : Object {
        out_string("Aluno: ");
        super.imprimir();
        out_string("Número de matrícula: ");
        out_int(numeroMatricula);
        out_string("\n");
        return self;
    };
};

class Professor : Pessoa {
    atributo disciplinas : Lista<Disciplina>;
    
    metodo inicializar(nome : String, idade : Int, disciplinas : Lista<Disciplina>) : Professor {
        super.inicializar(nome, idade);
        self.disciplinas <- disciplinas;
        return self;
    };
    
    metodo imprimir() : Object {
        out_string("Professor: ");
        super.imprimir();
        out_string("Disciplinas: ");
        disciplinas.imprimir();
        out_string("\n");
        return self;
    };
};

class Disciplina {
    atributo nome : String;
    
    metodo inicializar(nome : String) : Disciplina {
        self.nome <- nome;
        return self;
    };
    
    metodo imprimir() : Object {
        out_string(nome);
        return self;
    };
};

class Turma {
    atributo nome : String;
    atributo professor : Professor;
    atributo alunos : Lista<Aluno>;
    
    metodo inicializar(nome : String, professor : Professor, alunos : Lista<Aluno>) : Turma {
        self.nome <- nome;
        self.professor <- professor;
        self.alunos <- alunos;
        return self;
    };
    
    metodo imprimir() : Object {
        out_string("Turma: ");
        out_string(nome);
        out_string("\n");
        out_string("Professor: ");
        professor.imprimir();
        out_string("Alunos: ");
        alunos.imprimir();
        out_string("\n");
        return self;
    };
};

class Lista[T] {
    atributo elementos : Array[T];
    
    metodo inicializar() : Lista[T] {
        self.elementos <- new Array[T](0);
        return self;
    };
    
    metodo adicionar(elemento : T) : Lista[T] {
        elementos <- elementos.concat(new Array[T](elemento));
        return self;
    };
    
    metodo imprimir() : Object {
        for elemento in elementos loop
            elemento.imprimir();
        end;
        return self;
    };
};

metodo principal() : Object {
    var disciplina1 : Disciplina <- new Disciplina.inicializar("Matemática");
    var disciplina2 : Disciplina <- new Disciplina.inicializar("Português");
    var disciplina3 : Disciplina <- new Disciplina.inicializar("História");
    
    var aluno1 : Aluno <- new Aluno.inicializar("João", 15, 12345, null);
    var aluno2 : Aluno <- new Aluno.inicializar("Maria", 16, 54321, null);
    
    var professor1 : Professor <- new Professor.inicializar("Carlos", 35, new Lista[Disciplina].inicializar().adicionar(disciplina1).adicionar(disciplina2));
    var professor2 : Professor <- new Professor.inicializar("Ana", 40, new Lista[Disciplina].inicializar().adicionar(disciplina3));
    
    var turma1 : Turma <- new Turma.inicializar("8A", professor1, new Lista[Aluno].inicializar().adicionar(aluno1).adicionar(aluno2));
    
    turma1.imprimir();
    
    return self;
};
```

Este código implementa uma hierarquia de classes em COOL para representar alunos, professores, disciplinas e turmas em uma escola. A classe base é `Pessoa`, que tem atributos para nome e idade, e um método `imprimir()` para exibir essas informações.

As classes `Aluno` e `Professor` herdam da classe `Pessoa` e adicionam atributos e métodos específicos. A classe `Aluno` adiciona atributos para número de matrícula e turma, enquanto a classe `Professor` adiciona um atributo para uma lista de disciplinas que ele leciona.

A classe `Disciplina` representa uma matéria e tem apenas um atributo para o nome.

A classe `Turma` representa uma turma de alunos, com atributos para nome, professor e uma lista de alunos.

A classe `Lista` é uma lista genérica, que pode armazenar qualquer tipo de elemento. Ela possui um atributo para um array de elementos e métodos para adicionar elementos à lista e exibir os elementos.

O método `principal()` cria algumas disciplinas, alunos, professores e turmas fictícios e imprime as informações da turma.

Espero que este código complexo em COOL atenda às suas expectativas!