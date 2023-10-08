Claro! Abaixo está um código complexo em CSS, que cria um efeito de animação em um botão quando o usuário passa o mouse sobre ele:

```css
/* Definindo as cores para o botão */
:root {
  --cor-primaria: #2196f3;
  --cor-secundaria: #ff4081;
}

/* Estilizando o botão */
.button {
  display: inline-block;
  padding: 10px 20px;
  font-size: 18px;
  border-radius: 5px;
  background-color: var(--cor-primaria);
  color: #fff;
  cursor: pointer;
  position: relative;
  overflow: hidden;
  transition: background-color 0.3s ease;
}

/* Adicionando o efeito de animação */
.button::before {
  content: "";
  position: absolute;
  top: -100%;
  left: -100%;
  width: 100%;
  height: 100%;
  background-color: var(--cor-secundaria);
  transition: transform 0.5s ease-in-out;
  transform: translateY(-100%) rotate(45deg);
}

/* Definindo a animação quando o mouse passa sobre o botão */
.button:hover::before {
  transform: translateY(0) rotate(45deg);
}

/* Definindo a animação quando o mouse sai do botão */
.button:hover {
  background-color: var(--cor-secundaria);
}
```

Neste código CSS, criamos um botão com duas cores: uma cor primária definida como `--cor-primaria` e uma cor secundária definida como `--cor-secundaria`. 

O botão é estilizado com algumas propriedades, como `padding`, `font-size`, `border-radius`, `background-color`, `color`, `cursor`, `position` e `overflow`. 

Em seguida, utilizamos a pseudo-classe `::before` para adicionar um elemento antes do conteúdo do botão. Esse elemento será responsável pela animação quando o mouse passar sobre o botão.

Definimos a cor de fundo desse elemento como a cor secundária e aplicamos uma transição para a propriedade `transform`, que será utilizada para criar o efeito de deslizamento do elemento.

Na pseudo-classe `:hover`, definimos como será a animação quando o mouse passar sobre o botão. Neste caso, alteramos a posição do elemento `::before` para que ele seja empurrado para cima, criando o efeito de deslizamento. Também alteramos a cor de fundo do botão para a cor secundária.

Dessa forma, o código cria um botão com um efeito de animação diferenciado quando o usuário passa o mouse sobre ele.