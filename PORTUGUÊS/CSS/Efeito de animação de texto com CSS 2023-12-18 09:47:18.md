Claro! Aqui está um código complexo em CSS que cria um efeito de animação de texto em uma página da web:

```css
/* Definindo o estilo do container */
.container {
  display: flex;
  justify-content: center;
  align-items: center;
  height: 100vh;
}

/* Definindo o estilo do texto */
.texto {
  font-family: Arial, sans-serif;
  font-size: 48px;
  font-weight: bold;
  color: #333;
  text-transform: uppercase;
  position: relative;
}

/* Criando o efeito de animação */
@keyframes animacao-texto {
  0% {
    opacity: 0;
    transform: translateY(100px);
  }
  100% {
    opacity: 1;
    transform: translateY(0);
  }
}

/* Aplicando o efeito de animação ao texto */
.texto::before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background: linear-gradient(to right, #f00, #00f);
  z-index: -1;
  opacity: 0.5;
  transform: scaleX(0);
  transform-origin: left;
  animation: animacao-texto 3s ease-in-out infinite alternate;
}
```

Neste código, começamos definindo o estilo do container, que será responsável por centralizar o texto verticalmente e horizontalmente na página. Em seguida, definimos o estilo do texto em si, incluindo a fonte, cor, tamanho e transformação para letras maiúsculas.

Em seguida, criamos um efeito de animação usando a regra `@keyframes`. Esse efeito de animação faz com que o texto apareça gradualmente na página, deslizando de baixo para cima.

Finalmente, aplicamos o efeito de animação ao texto usando a pseudo-classe `::before`. Essa pseudo-classe cria um elemento pseudo antes do texto principal e é usado para aplicar uma cor de fundo em gradiente e uma animação de escala horizontal.

Espero que este código atenda às suas expectativas!