Claro! Vou criar um código complexo em CSS para você. Este código vai gerar uma animação de carregamento personalizada. 

```css
@keyframes loading-animation {
  0% {
    width: 0;
    background-color: #000;
  }
  25% {
    width: 25%;
    background-color: #ff0000;
  }
  50% {
    width: 50%;
    background-color: #00ff00;
  }
  75% {
    width: 75%;
    background-color: #0000ff;
  }
  100% {
    width: 100%;
    background-color: #000;
  }
}

.loading-container {
  width: 200px;
  height: 20px;
  border: 1px solid #000;
  position: relative;
  animation: loading-animation 1s infinite;
  margin: 50px auto;
}

.loading-bar {
  width: 0;
  height: 100%;
  position: absolute;
}
```

Este código começa criando uma animação chamada `loading-animation` usando a regra `@keyframes`. A animação possui cinco etapas (`0%`, `25%`, `50%`, `75%`, `100%`) que definem diferentes larguras e cores de fundo para a barra de carregamento.

Em seguida, criamos uma classe `.loading-container` que representa o contêiner da barra de carregamento. Definimos a largura, altura, borda e posição relativa. Também aplicamos a animação `loading-animation` com uma duração de 1 segundo e uma repetição infinita.

Dentro do contêiner, temos um elemento com a classe `.loading-bar` que representa a barra de carregamento real. Definimos a largura inicial como zero e a altura como 100%. Usando a propriedade `position: absolute`, posicionamos a barra de carregamento dentro do contêiner.

Ao aplicar esse código em um elemento HTML, você verá uma barra de carregamento que preenche gradualmente com diferentes cores, simulando um processo de carregamento.