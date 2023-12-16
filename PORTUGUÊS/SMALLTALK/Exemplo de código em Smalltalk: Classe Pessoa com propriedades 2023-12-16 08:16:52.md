Claro! Aqui está um exemplo de código em Smalltalk que cria uma classe chamada "Pessoa" com propriedades como nome, idade e país de origem.

```smalltalk
"Definindo a classe Pessoa"
Object subclass: #Pessoa
    instanceVariableNames: 'nome idade paisDeOrigem'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Exemplo'

"Definindo o método de inicialização"
Pessoa>>initialize
    super initialize.
    nome := ''.
    idade := 0.
    paisDeOrigem := ''.

"Definindo os getters e setters"
Pessoa>>nome: umNome
    nome := umNome.

Pessoa>>nome
    ^ nome.

Pessoa>>idade: umaIdade
    idade := umaIdade.

Pessoa>>idade
    ^ idade.

Pessoa>>paisDeOrigem: umPais
    paisDeOrigem := umPais.

Pessoa>>paisDeOrigem
    ^ paisDeOrigem.

"Exemplo de uso"
| pessoa |
pessoa := Pessoa new.
pessoa nome: 'João'.
pessoa idade: 25.
pessoa paisDeOrigem: 'Brasil'.

"Exibindo os valores da pessoa"
Transcript show: 'Nome: ', pessoa nome; cr.
Transcript show: 'Idade: ', pessoa idade asString; cr.
Transcript show: 'País de origem: ', pessoa paisDeOrigem; cr.
```

Neste código, a classe "Pessoa" é criada utilizando o conceito de herança da classe "Object". A classe possui três variáveis de instância: "nome", "idade" e "paisDeOrigem". 

Em seguida, são definidos os métodos de inicialização (initialize) e os getters e setters para cada uma das variáveis de instância. 

No exemplo de uso, é criada uma instância da classe "Pessoa", atribuídos valores às variáveis de instância e, por fim, são exibidos os valores utilizando o objeto "Transcript" para exibir as informações no console.

Espero que este exemplo possa te ajudar a entender melhor a linguagem Smalltalk!