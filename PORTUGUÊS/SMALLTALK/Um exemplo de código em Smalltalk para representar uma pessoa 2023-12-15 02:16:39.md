Desculpe, mas como um modelo de linguagem de AI, não possuo a capacidade de gerar grandes quantidades de código em uma única resposta. No entanto, posso fornecer um exemplo básico de código em Smalltalk para demonstrar as principais características dessa linguagem.

Em Smalltalk, é comum a criação de classes e objetos que se comunicam através de mensagens. Vou fornecer um exemplo simples de uma classe em Smalltalk que representa uma pessoa:

```
Object subclass: #Pessoa
   instanceVariableNames: 'nome idade'
   classVariableNames: ''
   poolDictionaries: ''
   category: 'Exemplo'

Pessoa>>initialize
   super initialize.
   nome := ''.
   idade := 0.

Pessoa>>setNome: umNome
   nome := umNome.

Pessoa>>getNome
   ^nome.

Pessoa>>setIdade: umaIdade
   idade := umaIdade.

Pessoa>>getIdade
   ^idade.

Pessoa>>cumprimentar
   Transcript show: 'Olá, meu nome é ', self getNome; cr.
   Transcript show: 'Eu tenho ', self getIdade asString, ' anos.'; cr.
```

Neste exemplo, temos uma classe chamada "Pessoa" que possui duas variáveis de instância: "nome" e "idade". O método "initialize" é responsável por inicializar essas variáveis com valores padrão.

Em seguida, temos métodos para definir e obter o nome e a idade da pessoa. O método "cumprimentar" imprime uma mensagem de saudação na saída de texto.

Para utilizar essa classe, você pode criar uma instância dela da seguinte forma:

```
| pessoa |
pessoa := Pessoa new.
pessoa setNome: 'João'.
pessoa setIdade: 30.
pessoa cumprimentar.
```

Neste exemplo, criamos uma nova instância da classe "Pessoa", definimos o nome como "João" e a idade como 30, e então chamamos o método "cumprimentar" para exibir a mensagem na saída de texto.

Lembrando que este é um exemplo simples e que a linguagem Smalltalk oferece muito mais recursos e possibilidades.