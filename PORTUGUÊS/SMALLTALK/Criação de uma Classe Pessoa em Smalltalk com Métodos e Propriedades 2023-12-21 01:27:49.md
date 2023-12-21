Claro! Aqui está um código em Smalltalk que cria uma classe chamada "Pessoa" com alguns métodos e propriedades:

```smalltalk
Object subclass: #Pessoa
    instanceVariableNames: 'nome idade'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Exemplo'

Pessoa>>initialize
    super initialize.
    nome := ''.
    idade := 0.

Pessoa>>setNome: novoNome
    nome := novoNome.

Pessoa>>getIdade
    ^idade.

Pessoa>>setIdade: novaIdade
    idade := novaIdade.

Pessoa>>apresentar
    Transcript show: 'Olá, meu nome é ', nome, ' e eu tenho ', idade asString, ' anos.'; cr.

Pessoa class>>new: umNome idade: umaIdade
    | pessoa |
    pessoa := self new.
    pessoa setNome: umNome.
    pessoa setIdade: umaIdade.
    ^pessoa
```

Este código define uma classe chamada "Pessoa" que possui duas variáveis de instância: "nome" e "idade". A classe possui os seguintes métodos:

- `initialize`: é o método de inicialização da classe. Ele cria uma nova instância de Pessoa e inicializa as variáveis de instância com valores padrão.
- `setNome:`: é um método de instância que define o nome da pessoa.
- `getIdade`: é um método de instância que retorna a idade da pessoa.
- `setIdade:`: é um método de instância que define a idade da pessoa.
- `apresentar`: é um método de instância que imprime na tela uma mensagem de apresentação, contendo o nome e a idade da pessoa.
- `new:idade:`: é um método de classe que cria uma nova instância de Pessoa, define o nome e a idade da pessoa e retorna a instância criada.

Para testar o código, você pode executar o seguinte trecho de código:

```smalltalk
| pessoa |
pessoa := Pessoa new: 'João' idade: 30.
pessoa apresentar.
```

O código acima cria uma nova instância de Pessoa com o nome "João" e a idade 30, e em seguida chama o método `apresentar` para imprimir a mensagem de apresentação na tela.