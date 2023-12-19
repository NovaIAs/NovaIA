Claro! Aqui está um código em COOL bastante complexo e diferenciado:

```
class Pessoa {
    atributo nome : String <- "John Doe";
    atributo idade : Int <- 25;

    metódo init(nome_pessoa : String, idade_pessoa : Int) : SELF_TYPE {
        nome <- nome_pessoa;
        idade <- idade_pessoa;
        retorne eu;
    };

    metódo imprimir_dados() : SELF_TYPE {
        out_string("Nome: ");
        out_string(nome);
        out_string(", Idade: ");
        out_int(idade);
        out_string("\n");
        retorne eu;
    };

    metódo calcular_aniversario() : Int {
        se idade < 0 então {
            retorne -1;
        }
        senão {
            retorne idade + 1;
        };
    };
};

metódo principal() : Int {
    var pessoa : Pessoa <- new Pessoa.init("Jane Smith", 30);
    pessoa.imprimir_dados();  // Imprime "Nome: Jane Smith, Idade: 30"
    out_string("Próximo aniversário: ");
    out_int(pessoa.calcular_aniversario());  // Imprime "Próximo aniversário: 31"
    retorne 0;
};
```

Neste código, estamos definindo uma classe chamada "Pessoa" que possui dois atributos: "nome" do tipo String, com um valor padrão de "John Doe", e "idade" do tipo Int, com um valor padrão de 25. A classe também possui dois métodos: "init" que inicializa os atributos com valores fornecidos e retorna a instância da classe, e "imprimir_dados" que imprime na saída o nome e a idade da pessoa.

No método principal, criamos uma instância da classe Pessoa chamada "pessoa" com o nome "Jane Smith" e idade 30. Em seguida, chamamos o método "imprimir_dados" para imprimir na saída os dados da pessoa. Por fim, chamamos o método "calcular_aniversario" para calcular o próximo aniversário da pessoa e imprimir na saída.

Espero que este código complexo em COOL atenda às suas expectativas!