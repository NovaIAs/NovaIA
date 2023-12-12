Claro! Aqui está um código em Smalltalk que cria um programa simples para calcular a média de notas de um aluno:

```smalltalk
| notas soma_media quantidade_media |
notas := OrderedCollection new.
soma_media := 0.
quantidade_media := 0.

[true] whileTrue: [
    Transcript show: 'Digite uma nota (ou "fim" para encerrar): '.
    | input |
    input := stdin nextLine.
    (input = 'fim') ifTrue: [ 
        Transcript show: 'Encerrando programa...' ; cr.
        break 
    ].
    | nota |
    nota := input asNumber.
    (nota isNil or: [nota > 10 or: [nota < 0]]) ifTrue: [
        Transcript show: 'Nota inválida. Tente novamente.' ; cr.
        continue
    ].

    notas add: nota.
    soma_media := soma_media + nota.
    quantidade_media := quantidade_media + 1.
].

Transcript show: 'Resultados:' ; cr.
Transcript show: 'Notas: ', notas asString ; cr.
Transcript show: 'Quantidade de notas: ', quantidade_media asString ; cr.
Transcript show: 'Média: ', (soma_media / quantidade_media) asString ; cr.
```

Neste código, utilizamos loops whileTrue e condicionais ifTrue para controlar a entrada das notas pelo usuário e validar se as notas são válidas (no intervalo de 0 a 10). O programa armazena todas as notas em uma coleção ordenada (OrderedCollection) e realiza a soma das notas e quantidade de notas para calcular a média.

No final, os resultados são exibidos no Transcript, mostrando as notas digitadas, a quantidade de notas e a média calculada.

Esse é apenas um exemplo simples de código em Smalltalk, mas mostra como é possível criar programas interativos e realizar cálculos básicos em um ambiente de programação orientado a objetos.