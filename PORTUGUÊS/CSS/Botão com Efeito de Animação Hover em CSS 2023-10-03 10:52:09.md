Certamente! Aqui está um código complexo em CSS que cria um efeito de animação hover em um botão:

```css
/* Definindo as cores utilizadas */
:root {
  --cor-primaria: #ff5f6d;
  --cor-secundaria: #00bbf9;
}

/* Estilizando o botão */
.button {
  position: relative;
  display: inline-block;
  padding: 10px 20px;
  color: white;
  text-transform: uppercase;
  font-weight: bold;
  background: var(--cor-primaria);
  border: none;
  cursor: pointer;
  overflow: hidden;
  transition: background 0.3s;
}

/* Efeito hover */
.button::before {
  content: "";
  position: absolute;
  top: 50%;
  left: 50%;
  width: 0;
  height: 0;
  background: var(--cor-secundaria);
  transform: translate(-50%, -50%);
  opacity: 0;
  border-radius: 50%;
  transition: width 0.3s ease-out, height 0.3s ease-out, opacity 0.3s;
}

.button:hover::before {
  width: 300px;
  height: 300px;
  opacity: 1;
}

/* Efeito de texto cortado */
.button span {
  position: relative;
  z-index: 1;
}

.button span::before {
  content: attr(data-text);
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  color: var(--cor-secundaria);
  clip-path: polygon(0 0, 100% 0, 100% 100%, 0% 100%);
  transition: clip-path 0.3s;
}

.button:hover span::before {
  clip-path: polygon(0 0, 100% 0, 100% 100%, 0 100%);
}
```

Este código cria um botão com uma animação de hover. O botão tem uma cor de fundo primária definida pela variável `--cor-primaria` e, quando o cursor passa por cima do botão, um efeito de expansão circular é ativado.

O efeito de expansão circular é criado utilizando um pseudo-elemento `::before` posicionado no centro do botão com `position: absolute`. O `width`, `height` e `opacity` são animados para criar a ilusão de expansão. A cor de fundo do pseudo-elemento é definida pela variável `--cor-secundaria`.

Além disso, o texto do botão tem um efeito de texto cortado. Isso é feito utilizando outro pseudo-elemento `::before` que cobre todo o texto e aplica um `clip-path` para cortar o texto diagonalmente. Quando o cursor passa por cima do botão, o `clip-path` é animado para revelar o texto completo.

Espero que isso atenda às suas expectativas! Se você tiver mais alguma solicitação, por favor, me avise.