Claro! Aqui está um código em Smalltalk que gera um padrão de texto diferenciado:

```smalltalk
| frase1 frase2 resultado |
frase1 := 'Olá, como você está? '.
frase2 := 'Espero que esteja tendo um ótimo dia! '.

resultado := String new.
1 to: frase1 size do: [:i |
    resultado := resultado, (frase1 at: i).
    resultado := resultado, (frase2 at: i).
].

Transcript show: resultado
```

Nesse código, temos duas variáveis `frase1` e `frase2`, que contêm duas frases diferentes. A variável `resultado` é inicializada como uma nova string vazia.

Em seguida, usamos um loop `1 to: frase1 size` para iterar através de cada caractere da frase1. Dentro do loop, concatenamos o caractere atual de `frase1` com o caractere correspondente de `frase2` e adicionamos ao `resultado`.

Finalmente, usamos `Transcript show: resultado` para exibir o resultado no console.

Este código gera um padrão de texto onde cada caractere de `frase1` é seguido pelo caractere correspondente de `frase2`. Por exemplo, se `frase1` for "Olá" e `frase2` for "Mundo", o resultado será "OMlánud". Essa abordagem pode ser modificada para criar outros padrões de texto interessantes!
