Claro! Aqui está um exemplo de código em Smalltalk que cria uma classe chamada "Pessoa" com atributos e métodos para manipular informações pessoais:

```smalltalk
Object subclass: #Pessoa
    instanceVariableNames: 'nome cpf idade'
    classVariableNames: ''
    package: 'MeuProjeto'

Pessoa >> setNome: aNome
    "Define o nome da pessoa"
    nome := aNome

Pessoa >> getNome
    "Retorna o nome da pessoa"
    ^ nome

Pessoa >> setCpf: aCpf
    "Define o CPF da pessoa"
    cpf := aCpf

Pessoa >> getCpf
    "Retorna o CPF da pessoa"
    ^ cpf

Pessoa >> setIdade: aIdade
    "Define a idade da pessoa"
    idade := aIdade

Pessoa >> getIdade
    "Retorna a idade da pessoa"
    ^ idade

Pessoa >> imprimirDados
    "Imprime os dados da pessoa"
    Transcript show: 'Nome: ', self getNome; cr.
    Transcript show: 'CPF: ', self getCpf; cr.
    Transcript show: 'Idade: ', self getIdade; cr.

"Exemplo de uso da classe Pessoa"
| pessoa1 |
pessoa1 := Pessoa new.
pessoa1 setNome: 'João'.
pessoa1 setCpf: '123.456.789-00'.
pessoa1 setIdade: 25.
pessoa1 imprimirDados.
```

Nesse código, criamos uma classe chamada "Pessoa" com três atributos: nome, cpf e idade. Em seguida, definimos métodos para definir e obter o valor de cada atributo. Também criamos um método "imprimirDados" para exibir as informações pessoais na janela do Transcript.

No exemplo de uso da classe, criamos uma nova instância da classe Pessoa, atribuímos valores aos atributos e chamamos o método "imprimirDados" para mostrar as informações na janela do Transcript.

Esse código é um exemplo simples de como criar uma classe em Smalltalk e manipular os atributos e métodos da mesma. Você pode usar esse exemplo como ponto de partida para criar um código mais complexo e personalizado de acordo com suas necessidades.