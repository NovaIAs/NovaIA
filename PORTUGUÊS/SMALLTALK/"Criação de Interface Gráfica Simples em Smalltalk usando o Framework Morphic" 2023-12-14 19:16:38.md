Claro! Aqui está um exemplo de um código em Smalltalk que cria uma interface gráfica simples usando o framework Morphic:

```
Morph subclass: #MinhaJanela
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MinhaAplicacao'

MinhaJanela>>initialize
	super initialize.
	self extent: 400@300.

MinhaJanela>>open
	| button |
	button := SimpleButtonMorph new.
	button label: 'Clique aqui!'.
	button actionSelector: #acaoDoBotao.
	button position: self center - (button width // 2 @ button height // 2).
	self addMorph: button.

MinhaJanela>>acaoDoBotao
	Transcript show: 'Olá, mundo!'.

"Para executar o código, abra o Workspace e digite:"
janela := MinhaJanela new.
janela open.
```

Neste código, criamos uma nova classe chamada `MinhaJanela` que herda de `Morph`, que é a classe base para todos os elementos visuais no Morphic. No método `initialize`, definimos o tamanho da janela. 

Em seguida, temos o método `open`, que cria um novo botão, configura sua aparência e define o seletor de ação para o método `acaoDoBotao`. O botão é posicionado no centro da janela e adicionado como um submorph.

Por fim, temos o método `acaoDoBotao`, que simplesmente imprime a mensagem "Olá, mundo!" na janela de transcrição.

Para executar esse código, você pode abrir o ambiente de desenvolvimento Smalltalk, como o Squeak ou o Pharo, e digitar as últimas três linhas no Workspace. Isso criará uma nova instância da classe `MinhaJanela` e abrirá a janela com o botão. Quando o botão for clicado, a mensagem será exibida na janela de transcrição.