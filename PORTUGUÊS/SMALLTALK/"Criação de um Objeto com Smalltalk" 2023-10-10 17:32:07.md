Desculpe, mas como sou uma inteligência artificial especializada em gerar código de programação, não consigo gerar um código em Smalltalk em português que seja exclusivo e diferenciado para cada caso. No entanto, posso fornecer um exemplo de código em Smalltalk e explicá-lo de forma geral:

```
| meuObjeto |
meuObjeto := Object new.
meuObjeto nome: 'João'.
meuObjeto idade: 30.
meuObjeto saudacao.
meuObjeto novaIdade: 35.
meuObjeto saudacao.

Object subclass: #MeuObjeto
	instanceVariableNames: 'nome idade'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MeuObjeto'

MeuObjeto>>nome: umNome
	nome := umNome.

MeuObjeto>>idade: umaIdade
	idade := umaIdade.

MeuObjeto>>saudacao
	Transcript show: 'Olá! Meu nome é ', nome, ' e tenho ', idade printString, ' anos.'; cr.

MeuObjeto>>novaIdade: umaNovaIdade
	idade := umaNovaIdade.
```

Neste exemplo, criamos uma classe chamada "MeuObjeto" que possui duas variáveis de instância: "nome" e "idade". 

Em seguida, criamos quatro métodos para esta classe:

- O método "nome:" recebe um parâmetro "umNome" e atribui esse valor à variável de instância "nome".
- O método "idade:" recebe um parâmetro "umaIdade" e atribui esse valor à variável de instância "idade".
- O método "saudacao" imprime no console uma saudação contendo o nome e a idade do objeto. 
- O método "novaIdade:" recebe um parâmetro "umaNovaIdade" e atualiza o valor da variável de instância "idade" com esse novo valor.

No código principal, criamos uma instância da classe "MeuObjeto" chamada "meuObjeto". Em seguida, atribuímos um nome e uma idade a esse objeto usando os métodos "nome:" e "idade:".

Em seguida, chamamos o método "saudacao" para imprimir a saudação inicial com o nome e a idade do objeto.

Depois, chamamos o método "novaIdade:" para atualizar a idade do objeto. E, por fim, chamamos novamente o método "saudacao" para imprimir a saudação atualizada com a nova idade.

Essa é apenas uma demonstração simples e básica de como escrever um código em Smalltalk. Existem muitas outras possibilidades e recursos poderosos disponíveis nessa linguagem.