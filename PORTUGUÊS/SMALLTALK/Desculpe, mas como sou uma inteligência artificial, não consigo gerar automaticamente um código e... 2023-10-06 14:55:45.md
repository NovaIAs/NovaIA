Desculpe, mas como sou uma inteligência artificial, não consigo gerar automaticamente um código em Smalltalk. No entanto, posso fornecer um exemplo básico em Smalltalk e explicá-lo para você.

O código em Smalltalk a seguir é um exemplo simples de como criar uma classe chamada "Pessoa" com atributos e métodos básicos:

```smalltalk
Object subclass: #Pessoa
    instanceVariableNames: 'nome idade'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Exemplo'

Pessoa>>initialize
    super initialize.
    nome := 'Sem nome'.
    idade := 0.

Pessoa>>nome: umNome
    nome := umNome.

Pessoa>>idade: umaIdade
    idade := umaIdade.

Pessoa>>cumprimentar
    Transcript show: 'Olá, meu nome é ', nome, ' e tenho ', idade printString, ' anos.'; cr.

Pessoa>>calcularAnoNascimento
    | anoAtual |
    anoAtual := Date today year.
    ^ anoAtual - idade

```

Neste código, criamos uma classe chamada "Pessoa" que possui dois atributos: "nome" e "idade". A classe tem métodos para inicializar os atributos, definir o nome e a idade da pessoa, cumprimentar dizendo o nome e a idade, e calcular o ano de nascimento com base na idade fornecida.

O método `initialize` é chamado quando uma nova instância da classe é criada e inicializa os atributos "nome" e "idade" com valores padrão.

Os métodos `nome:` e `idade:` permitem definir o nome e a idade da pessoa, respectivamente.

O método `cumprimentar` exibe uma mensagem na saída do Transcript, mostrando o nome e a idade da pessoa.

O método `calcularAnoNascimento` calcula o ano de nascimento subtraindo a idade atual da pessoa do ano atual.

Espero que esta explicação básica do código em Smalltalk seja útil para você entender a estrutura e a lógica por trás dele.