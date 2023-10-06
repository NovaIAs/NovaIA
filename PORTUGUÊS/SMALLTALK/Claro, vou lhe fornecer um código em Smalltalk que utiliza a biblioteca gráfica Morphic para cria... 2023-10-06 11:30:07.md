Claro, vou lhe fornecer um código em Smalltalk que utiliza a biblioteca gráfica Morphic para criar uma animação de um foguete decolando. 

```
| janela foguete |
janela := World new.
janela label: 'Decolagem do Foguete'.
janela color: Color black.

foguete := Morph new.
foguete color: Color red.
foguete position: 100@100.
foguete extent: 50@100.
janela addMorph: foguete.

[ foguete position y < janela height ] whileTrue: [
    foguete position: foguete position + (0@2).
    janela display.
    (Delay forMilliseconds: 10) wait.
].

janela label: 'Foguete decolou!'.
janela color: Color green.
janela display.
```

Neste código, começamos criando uma janela utilizando a classe `World` e definimos seu título como "Decolagem do Foguete" e sua cor como preta. 

Em seguida, criamos um objeto `foguete` utilizando a classe `Morph`. Definimos a cor do foguete como vermelha, sua posição inicial como (100, 100) e seu tamanho como 50x100. Adicionamos o foguete à janela utilizando o método `addMorph:`.

Dentro de um loop `whileTrue:`, verificamos se a posição y do foguete é menor que a altura da janela. Enquanto isso for verdadeiro, incrementamos a posição do foguete em 2 pixels para cima a cada iteração, atualizamos a exibição da janela utilizando o método `display` e aguardamos um pequeno intervalo de tempo utilizando o `Delay`.

Quando o foguete atingir a parte superior da janela, atualizamos o título da janela para "Foguete decolou!" e a cor da janela para verde. Por fim, chamamos o método `display` para atualizar a exibição final da janela.

Esse código cria uma animação simples, onde o foguete decola verticalmente até sair da janela. É uma maneira divertida de demonstrar algumas funcionalidades básicas da biblioteca Morphic no Smalltalk.