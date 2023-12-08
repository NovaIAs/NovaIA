Claro! Aqui está um código complexo em COOL que atende aos seus requisitos:

```
class Pessoa {

  atributo nome : String;

  atributo idade : Inteiro;
  
  atributo endereco : Endereco;

  metodo construtor(nome: String, idade: Inteiro, endereco: Endereco) : Pessoa {{
    self.nome <- nome;
    self.idade <- idade;
    self.endereco <- endereco;
  }}

  metodo imprimirDetalhes() : Object {{
    out_string("Nome: ");
    out_string(self.nome);
    out_string("- Idade: ");
    out_int(self.idade);

    self.endereco.imprimirDetalhes();
  }}

};

class Endereco {

  atributo rua : String;

  atributo numero : Inteiro;

  atributo cidade : String;

  metodo construtor(rua: String, numero: Inteiro, cidade: String) : Endereco {{
    self.rua <- rua;
    self.numero <- numero;
    self.cidade <- cidade;
  }}

  metodo imprimirDetalhes() : Object {{
    out_string("\nEndereco: ");
    out_string(self.rua);
    out_string(", Numero: ");
    out_int(self.numero);
    out_string(", Cidade: ");
    out_string(self.cidade);
  }}
};

metodo principal() : Object {
  var endereco1 : Endereco <- new Endereco("Rua dos Exemplo", 123, "Exemplolandia");
  var pessoa1 : Pessoa <- new Pessoa("Alice", 25, endereco1);

  pessoa1.imprimirDetalhes();

  var endereco2 : Endereco <- new Endereco("Rua das Demonstracoes", 456, "DemoCity");
  var pessoa2 : Pessoa <- new Pessoa("Bob", 30, endereco2);

  pessoa2.imprimirDetalhes();

  sair;
};
```

Neste código em COOL, criamos duas classes: Pessoa e Endereco. A classe Pessoa possui três atributos (nome, idade e endereco) e dois métodos (construtor e imprimirDetalhes). A classe Endereco também possui três atributos (rua, numero e cidade) e dois métodos (construtor e imprimirDetalhes).

No método principal, instanciamos duas pessoas com seus respectivos endereços e chamamos o método imprimirDetalhes para mostrar as informações na saída. O código demonstra a composição de objetos e a interação entre eles.

Espero que este código atenda às suas expectativas! Se você precisar de mais informações ou de um código diferente, estou à disposição para ajudar.