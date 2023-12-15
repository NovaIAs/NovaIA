Claro! Aqui está um exemplo de um código complexo em CSS que foi criado especialmente para você. Este código cria um efeito de animação em um elemento de botão quando o mouse passa sobre ele:

```css
/* Definindo as cores */
:root {
  --cor-primaria: #ff0000;
  --cor-secundaria: #0000ff;
}

/* Estilizando o botão */
.botao {
  display: inline-block;
  padding: 10px 20px;
  font-size: 18px;
  color: white;
  background-color: var(--cor-primaria);
  border: none;
  border-radius: 5px;
  cursor: pointer;
  position: relative;
  overflow: hidden;
}

/* Estilizando o efeito de fundo */
.botao::before {
  content: "";
  position: absolute;
  top: -50%;
  left: -50%;
  width: 200%;
  height: 200%;
  background-image: radial-gradient(var(--cor-primaria), var(--cor-secundaria));
  transform: scale(0);
  opacity: 0;
  transition: transform 0.5s, opacity 0.5s;
}

/* Definindo a animação ao passar o mouse */
.botao:hover::before {
  transform: scale(1);
  opacity: 1;
}

/* Definindo a animação ao clicar */
.botao:active::before {
  transform: scale(0);
  opacity: 0;
  transition-duration: 0s;
}
```

Neste código, começamos definindo duas variáveis de cor utilizando a declaração `:root`. Essas variáveis são utilizadas posteriormente para a cor do botão e a cor do efeito de fundo.

Em seguida, estilizamos a aparência do botão utilizando a classe `.botao`. Definimos suas dimensões, tamanho da fonte, cor do texto e do fundo, além de outros estilos como borda arredondada e cursor.

Utilizamos a pseudo-classe `::before` para adicionar um elemento antes do conteúdo do botão, que será responsável pelo efeito de fundo. Definimos sua posição como absoluta e ocupando 200% tanto na largura quanto na altura, criando assim um efeito de raio. A cor de fundo é uma gradiente radial entre a cor primária e a cor secundária, definidas anteriormente.

Para a animação, utilizamos a propriedade `transition` para suavizar a transformação e a opacidade do elemento `::before`. Quando o mouse passa sobre o botão, utilizando a pseudo-classe `:hover`, aumentamos a escala do elemento `::before` e sua opacidade gradualmente. Quando o botão é clicado, utilizando a pseudo-classe `:active`, a escala e a opacidade do elemento `::before` retornam ao seu estado inicial instantaneamente, através da propriedade `transition-duration`.

Espero que este código atenda às suas expectativas!