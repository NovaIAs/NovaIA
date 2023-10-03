Claro! Aqui está um código CSS complexo que cria um efeito de gradiente animado em um elemento de texto:

```css
@keyframes gradient-animation {
  0% {
    background-position: 0% 50%;
  }
  50% {
    background-position: 100% 50%;
  }
  100% {
    background-position: 0% 50%;
  }
}

.texto-gradiente {
  background: linear-gradient(to right, #ffafbd, #ffc3a0, #ffecb8);
  background-size: 200% 200%;
  animation: gradient-animation 10s ease infinite;
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
}
```

Neste código, usamos a propriedade `@keyframes` para definir uma animação chamada "gradient-animation". Essa animação possui três etapas: 0%, 50% e 100%. 

A propriedade `background-position` é utilizada para definir a posição do gradiente de fundo do elemento de texto em cada etapa da animação. 

Em seguida, aplicamos a animação ao elemento de texto com a classe ".texto-gradiente" usando a propriedade `animation`. Definimos a duração da animação como 10 segundos, o modo de transição como "ease" e o número de repetições como "infinite" (infinito).

Além disso, definimos o gradiente de fundo com a função `linear-gradient`, utilizando três cores diferentes: `#ffafbd`, `#ffc3a0` e `#ffecb8`. 

Também especificamos o tamanho do gradiente de fundo com `background-size: 200% 200%` para garantir que ele cubra todo o elemento de texto.

Para tornar o texto transparente e permitir que o gradiente de fundo seja exibido através dele, usamos as propriedades `-webkit-background-clip: text` e `-webkit-text-fill-color: transparent`, que são específicas para navegadores baseados em WebKit, como o Google Chrome e o Safari.

Esse código cria um efeito visual interessante, onde o gradiente de fundo se move suavemente através do texto, criando uma aparência de animação.