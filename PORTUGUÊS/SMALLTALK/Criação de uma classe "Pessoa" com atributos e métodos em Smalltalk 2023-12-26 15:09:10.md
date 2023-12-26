Claro! Aqui está um exemplo de código em Smalltalk que cria uma classe chamada "Pessoa" com atributos e métodos relacionados:

```
"Definição da classe Pessoa"
Object subclass: #Pessoa
    instanceVariableNames: 'nome idade altura'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'MinhaApp'

"Definição dos métodos para a classe Pessoa"
Pessoa>>initialize
    "Inicializa os atributos da classe"
    super initialize.
    nome := ''.
    idade := 0.
    altura := 0.0.

Pessoa>>setNome: aNome
    "Define o nome da pessoa"
    nome := aNome.

Pessoa>>setIdade: aIdade
    "Define a idade da pessoa"
    idade := aIdade.

Pessoa>>setAltura: aAltura
    "Define a altura da pessoa"
    altura := aAltura.

Pessoa>>getNome
    "Retorna o nome da pessoa"
    ^ nome.

Pessoa>>getIdade
    "Retorna a idade da pessoa"
    ^ idade.

Pessoa>>getAltura
    "Retorna a altura da pessoa"
    ^ altura.

Pessoa>>saudacao
    "Imprime uma saudação personalizada"
    Transcript show: 'Olá, eu sou ', self getNome, '!'.

"Exemplo de uso da classe Pessoa"
| pessoa1 |
pessoa1 := Pessoa new.
pessoa1 setNome: 'João'.
pessoa1 setIdade: 25.
pessoa1 setAltura: 1.75.
pessoa1 saudacao.
Transcript show: ' Minha idade é ', pessoa1 getIdade asString, ' anos.'.
Transcript show: ' Minha altura é ', pessoa1 getAltura asString, ' metros.'.
```

Neste código, criamos uma classe chamada "Pessoa", que possui três atributos: nome, idade e altura. Em seguida, definimos os métodos para a classe, como "initialize" para inicializar os atributos, "setNome:", "setIdade:" e "setAltura:" para definir os valores dos atributos, "getNome", "getIdade" e "getAltura" para retornar os valores dos atributos, e "saudacao" para imprimir uma saudação personalizada.

No exemplo de uso da classe, criamos uma instância da classe Pessoa chamada "pessoa1" e definimos os valores dos atributos usando os métodos correspondentes. Em seguida, chamamos o método "saudacao" para imprimir a saudação personalizada na saída do Transcript. Por fim, mostramos a idade e altura da pessoa usando os métodos "getIdade" e "getAltura".