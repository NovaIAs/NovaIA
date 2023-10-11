Claro! Aqui está um exemplo de código em SMALLTALK que cria uma calculadora básica com uma interface de usuário simples:

```smalltalk
| janela numero1 numero2 resultado |
janela := DialogWindow new.
janela title: 'Calculadora'.
janela extent: 400@200.

numero1 := TextField new.
numero1 extent: 100@30.
numero1 position: 20@20.
janela addSubmorph: numero1.

numero2 := TextField new.
numero2 extent: 100@30.
numero2 position: 20@60.
janela addSubmorph: numero2.

resultado := TextField new.
resultado extent: 100@30.
resultado position: 20@100.
resultado readOnly: true.
janela addSubmorph: resultado.

| botaoSoma botaoSubtracao botaoMultiplicacao botaoDivisao |
botaoSoma := SimpleButton new.
botaoSoma label: '+'.
botaoSoma action: [resultado text: (numero1 text asNumber + numero2 text asNumber) asString].
botaoSoma extent: 30@30.
botaoSoma position: 140@30.
janela addSubmorph: botaoSoma.

botaoSubtracao := SimpleButton new.
botaoSubtracao label: '-'.
botaoSubtracao action: [resultado text: (numero1 text asNumber - numero2 text asNumber) asString].
botaoSubtracao extent: 30@30.
botaoSubtracao position: 180@30.
janela addSubmorph: botaoSubtracao.

botaoMultiplicacao := SimpleButton new.
botaoMultiplicacao label: '*'.
botaoMultiplicacao action: [resultado text: (numero1 text asNumber * numero2 text asNumber) asString].
botaoMultiplicacao extent: 30@30.
botaoMultiplicacao position: 220@30.
janela addSubmorph: botaoMultiplicacao.

botaoDivisao := SimpleButton new.
botaoDivisao label: '/'.
botaoDivisao action: [resultado text: (numero1 text asNumber / numero2 text asNumber) asString].
botaoDivisao extent: 30@30.
botaoDivisao position: 260@30.
janela addSubmorph: botaoDivisao.

janela open.
```

Neste código, começamos criando uma janela de diálogo (`DialogWindow`) para exibir a calculadora. Em seguida, criamos três campos de texto (`TextField`) para os números de entrada (`numero1` e `numero2`) e o resultado (`resultado`).

Em seguida, criamos quatro botões (`SimpleButton`) para realizar as operações de soma, subtração, multiplicação e divisão. Cada botão é associado a uma ação que atualiza o campo de texto do resultado com o resultado da operação correspondente.

Por fim, adicionamos todos os componentes à janela e a abrimos.

Dessa forma, o usuário pode inserir dois números nos campos de texto e clicar em um dos botões para realizar a operação escolhida. O resultado será exibido no campo de texto do resultado.

Este código em SMALLTALK é apenas um exemplo básico de como criar uma calculadora simples. Você pode adicionar mais recursos e funcionalidades conforme necessário.