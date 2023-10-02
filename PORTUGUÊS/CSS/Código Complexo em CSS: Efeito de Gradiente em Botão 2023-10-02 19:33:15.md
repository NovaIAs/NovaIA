Claro! Aqui está um código complexo em CSS que cria um efeito de animação de gradiente em um botão:

HTML:

```html
<button class="animated-button">
  <span>Botão Animado</span>
</button>
```

CSS:

```css
.animated-button {
  position: relative;
  display: inline-block;
  padding: 10px 20px;
  border: none;
  outline: none;
  background: linear-gradient(to right, #ff6b6b, #3f51b5);
  color: #fff;
  font-size: 16px;
  font-weight: bold;
  cursor: pointer;
  overflow: hidden;
  transition: all 0.3s cubic-bezier(0.68, -0.55, 0.27, 1.55);
}

.animated-button:before {
  content: "";
  position: absolute;
  top: -100%;
  left: 50%;
  width: 100%;
  height: 100%;
  background: linear-gradient(to right, #ff6b6b, #3f51b5);
  transform: translateX(-50%) rotate(45deg);
  z-index: -1;
  transition: all 0.3s cubic-bezier(0.68, -0.55, 0.27, 1.55);
}

.animated-button:hover {
  background: transparent;
}

.animated-button:hover:before {
  top: 0;
}
```

Neste código, criamos um botão com uma classe "animated-button". O botão possui um fundo gradiente que vai do vermelho (#ff6b6b) ao azul (#3f51b5). Também definimos algumas propriedades de estilo, como cores, fonte, tamanho e cursor. 

Usando pseudoelemento `:before`, criamos um efeito de gradiente que se move de cima para baixo para criar uma animação quando o botão é hover. 

Ao passar o mouse sobre o botão, o fundo gradiente desaparece e o gradiente do pseudoelemento é animado de cima para baixo, criando um efeito visual interessante.

Espero que goste deste código complexo e diferenciado em CSS!