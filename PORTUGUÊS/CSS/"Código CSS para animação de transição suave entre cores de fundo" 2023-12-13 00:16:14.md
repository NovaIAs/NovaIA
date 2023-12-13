Claro, vou criar um código complexo em CSS para você. Aqui está um exemplo de código que cria uma animação de transição suave entre cores de fundo em um elemento:

```css
@keyframes transicao-cores {
  0% {
    background-color: red;
  }
  25% {
    background-color: blue;
  }
  50% {
    background-color: green;
  }
  75% {
    background-color: yellow;
  }
  100% {
    background-color: purple;
  }
}

.elemento {
  width: 200px;
  height: 200px;
  background-color: red;
  animation: transicao-cores 5s infinite;
}
```

Neste exemplo, utilizamos a regra `@keyframes` para definir uma animação chamada `transicao-cores`. Essa animação especifica diferentes estágios de cores de fundo para o elemento ao longo do tempo.

Dentro do `@keyframes`, definimos os estágios de animação usando porcentagens, de 0% a 100%. Em cada estágio, especificamos a cor de fundo desejada para o elemento.

Em seguida, aplicamos a animação ao elemento específico usando a propriedade `animation`. No exemplo acima, aplicamos a animação `transicao-cores` com uma duração de 5 segundos (`5s`) e repetição infinita (`infinite`).

Dessa forma, o elemento terá uma transição suave entre as cores de fundo especificadas em cada estágio da animação, criando um efeito visual interessante.

Note que esse é apenas um exemplo de código complexo em CSS. Existem inúmeras possibilidades de criação e customização utilizando CSS, de acordo com as necessidades de cada projeto.