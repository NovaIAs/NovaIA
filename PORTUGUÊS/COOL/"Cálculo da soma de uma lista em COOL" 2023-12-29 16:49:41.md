Claro! Aqui está um exemplo de código COOL que realiza uma tarefa complexa de forma diferenciada:

```
class Main {
    io : IO <- new IO;
    class IO {
        out_string(s : String) : SELF_TYPE {
            out_string_impl(s)
        };
        out_string_impl(s : String) : SELF_TYPE {
            "Imprimindo: " + s + "\n"
        };
    };
    main() : Object {
        let lista : List <- new List;
        lista.add(10);
        lista.add(20);
        lista.add(30);
        lista.add(40);
        lista.add(50);
        
        let soma : Int <- 0;
        suma_lista(lista, soma);
        
        io.out_string("A soma dos elementos da lista é: " + soma + "\n");
    };
    suma_lista(lista : List, soma : Int) : Int {
        if lista.is_empty() then
            soma
        else
            suma_lista(lista.tail(), soma + lista.head())
        fi;
    };
    class List {
        head : Int;
        tail : List;
        init() : SELF_TYPE {
            self
        };
        add(element : Int) : SELF_TYPE {
            tail <- new List;
            tail.init();
            head <- element;
            self
        };
        is_empty() : Bool {
            head = void
        };
        tail() : List {
            tail
        };
        head() : Int {
            head
        };
    };
};
```

Este código COOL realiza a soma dos elementos de uma lista de inteiros. Ele define uma classe `Main` que contém uma classe `IO` para lidar com a entrada/saída, uma classe `List` que representa uma lista encadeada de inteiros e um método `suma_lista` para calcular a soma dos elementos da lista.

O método `main` cria uma nova lista e adiciona alguns elementos. Em seguida, chama o método `suma_lista` passando a lista e uma variável `soma` inicializada como zero. O resultado da soma é impresso na saída padrão.

O método `suma_lista` é uma função recursiva que verifica se a lista está vazia. Se estiver, retorna o valor atual da soma. Caso contrário, chama-se a função novamente passando a cauda da lista (ou seja, todos os elementos menos o primeiro) e a soma atualizada com o valor do elemento atual.

Espero que este código complexo e diferenciado em COOL atenda às suas expectativas!