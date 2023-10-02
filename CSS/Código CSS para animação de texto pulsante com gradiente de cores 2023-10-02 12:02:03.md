Claro! Aqui está um código complexo em CSS que cria uma animação de texto pulsante com um gradiente de cores:

```css
@keyframes pulsar {
  0% {
    transform: scale(1);
    color: #ff0000;
  }
  50% {
    transform: scale(1.3);
    color: #00ff00;
  }
  100% {
    transform: scale(1);
    color: #0000ff;
  }
}

body {
  background-color: #000;
  display: flex;
  justify-content: center;
  align-items: center;
  height: 100vh;
}

h1 {
  font-family: Arial, sans-serif;
  font-size: 4rem;
  background: -webkit-linear-gradient(#ff0000, #00ff00, #0000ff);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  animation: pulsar 2s infinite;
}
```

Neste código, é definida uma animação chamada `pulsar` que altera a escala e a cor do texto ao longo do tempo. Em seguida, o corpo da página é estilizado com um fundo preto e é centralizado verticalmente e horizontalmente. O título `<h1>` é estilizado com uma fonte Arial, um tamanho de 4rem e um gradiente de cores que é aplicado ao texto por meio das propriedades `-webkit-background-clip` e `-webkit-text-fill-color`. A animação `pulsar` é aplicada ao título com uma duração de 2 segundos e é repetida infinitamente.

Espero que esse código atenda às suas expectativas!