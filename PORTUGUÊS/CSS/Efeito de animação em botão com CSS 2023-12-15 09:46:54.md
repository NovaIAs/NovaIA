Claro! Aqui está um código complexo em CSS, que utiliza várias propriedades e seletores para criar um efeito diferenciado em um elemento HTML. O código abaixo cria um efeito de animação em um botão quando o cursor do mouse passa sobre ele:

```css
/* Definindo o estilo base do botão */
.botao-animado {
  display: inline-block;
  padding: 10px 20px;
  font-size: 18px;
  font-weight: bold;
  color: #fff;
  background-color: #3498db;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  position: relative;
  overflow: hidden;
}

/* Definindo o estilo do efeito de fundo */
.botao-animado::before {
  content: '';
  position: absolute;
  top: 0;
  left: -100%;
  width: 100%;
  height: 100%;
  background-color: rgba(255, 255, 255, 0.3);
  transition: transform 0.3s cubic-bezier(0.5, 1, 0.89, 1);
}

/* Definindo o estilo do efeito de movimento do fundo */
.botao-animado:hover::before {
  transform: translateX(100%);
}

/* Definindo o estilo do texto do botão */
.botao-animado span {
  position: relative;
  z-index: 1;
}

/* Definindo o estilo do efeito de sombra */
.botao-animado::after {
  content: '';
  position: absolute;
  bottom: 0;
  left: 0;
  width: 100%;
  height: 4px;
  background-color: #fff;
  transform: scaleX(0);
  transform-origin: left;
  transition: transform 0.3s cubic-bezier(0.5, 1, 0.89, 1);
}

/* Definindo o estilo do efeito de crescimento da sombra */
.botao-animado:hover::after {
  transform: scaleX(1);
}
```

Explicação do código:

1. Começamos definindo o estilo base do botão com a classe `.botao-animado`. Aqui, definimos várias propriedades como `display`, `padding`, `font-size`, `color`, `background-color`, `border`, `border-radius`, `cursor`, `position` e `overflow`.
2. Em seguida, utilizamos o seletor `::before` para adicionar um pseudo-elemento antes do conteúdo do botão. Este pseudo-elemento será responsável por criar o efeito de fundo animado.
3. Definimos as propriedades do pseudo-elemento `::before`, como `content`, `position`, `top`, `left`, `width`, `height`, `background-color` e `transition`.
4. No seletor `:hover` do botão, alteramos a propriedade `transform` do pseudo-elemento `::before` para criar o efeito de movimento do fundo.
5. Utilizamos o seletor `span` para estilizar o texto do botão.
6. Adicionamos outro pseudo-elemento `::after` para criar o efeito de sombra na parte inferior do botão.
7. Definimos as propriedades do pseudo-elemento `::after`, como `content`, `position`, `bottom`, `left`, `width`, `height`, `background-color`, `transform`, `transform-origin` e `transition`.
8. No seletor `:hover` do botão, alteramos a propriedade `transform` do pseudo-elemento `::after` para criar o efeito de crescimento da sombra.

Espero que esse código atenda às suas expectativas!