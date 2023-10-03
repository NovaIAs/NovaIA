Claro! Aqui está um código complexo em CSS que cria um efeito de animação de texto em camadas:

```css
<!DOCTYPE html>
<html>
<head>
  <style>
    .container {
      position: relative;
      width: 300px;
      height: 200px;
      margin: 0 auto;
      overflow: hidden;
    }
    
    .layer {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      overflow: hidden;
    }
    
    .layer span {
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      font-size: 24px;
      font-weight: bold;
      color: white;
      text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.8);
      opacity: 0;
      animation: animateText 4s infinite;
    }
    
    .layer span:nth-child(1) {
      animation-delay: 0s;
    }
    
    .layer span:nth-child(2) {
      animation-delay: 1s;
    }
    
    .layer span:nth-child(3) {
      animation-delay: 2s;
    }
    
    .layer span:nth-child(4) {
      animation-delay: 3s;
    }
    
    @keyframes animateText {
      0% {
        opacity: 0;
        transform: translate(-50%, -50%) rotate(0deg);
      }
      25% {
        opacity: 1;
        transform: translate(-50%, -50%) rotate(0deg);
      }
      50% {
        opacity: 1;
        transform: translate(-50%, -50%) rotate(180deg);
      }
      75% {
        opacity: 1;
        transform: translate(-50%, -50%) rotate(180deg);
      }
      100% {
        opacity: 0;
        transform: translate(-50%, -50%) rotate(360deg);
      }
    }
    
    .layer:nth-child(1) {
      background-color: #ff5858;
      z-index: 4;
    }
    
    .layer:nth-child(2) {
      background-color: #ffbd2e;
      z-index: 3;
    }
    
    .layer:nth-child(3) {
      background-color: #32c8de;
      z-index: 2;
    }
    
    .layer:nth-child(4) {
      background-color: #673AB7;
      z-index: 1;
    }
  </style>
</head>
<body>
  <div class="container">
    <div class="layer">
      <span>L</span>
      <span>a</span>
      <span>y</span>
      <span>e</span>
      <span>r</span>
      <span>s</span>
      <span> </span>
      <span>E</span>
      <span>f</span>
      <span>f</span>
      <span>e</span>
      <span>c</span>
      <span>t</span>
    </div>
    <div class="layer">
      <span>L</span>
      <span>a</span>
      <span>y</span>
      <span>e</span>
      <span>r</span>
      <span>s</span>
      <span> </span>
      <span>E</span>
      <span>f</span>
      <span>f</span>
      <span>e</span>
      <span>c</span>
      <span>t</span>
    </div>
    <div class="layer">
      <span>L</span>
      <span>a</span>
      <span>y</span>
      <span>e</span>
      <span>r</span>
      <span>s</span>
      <span> </span>
      <span>E</span>
      <span>f</span>
      <span>f</span>
      <span>e</span>
      <span>c</span>
      <span>t</span>
    </div>
    <div class="layer">
      <span>L</span>
      <span>a</span>
      <span>y</span>
      <span>e</span>
      <span>r</span>
      <span>s</span>
      <span> </span>
      <span>E</span>
      <span>f</span>
      <span>f</span>
      <span>e</span>
      <span>c</span>
      <span>t</span>
    </div>
  </div>
</body>
</html>
```

Este código cria um efeito de animação de texto em camadas, onde cada camada exibe uma letra específica e a animação é aplicada individualmente em cada letra. O resultado é um texto que parece girar em um efeito tridimensional.

O código utiliza a propriedade `position: absolute` para posicionar as camadas uma em cima da outra, a propriedade `transform` para centralizar o texto verticalmente e horizontalmente, e a propriedade `animation` para criar a animação de rotação das letras.

Cada camada tem uma cor de fundo diferente, criando um efeito visual interessante. O texto é composto pela palavra "Layers Effect" repetida em cada camada. Cada letra tem um atraso na animação para criar um efeito de movimento sequencial.

Espero que este código atenda às suas expectativas!