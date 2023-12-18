Claro! Aqui está um código complexo em CSS que utiliza animações, pseudo-elementos e diversos seletores para criar um efeito único. Vou explicar cada parte do código para facilitar o entendimento:

```css
/* Definindo as cores */
:root {
  --cor-primaria: #ff6600;
  --cor-secundaria: #00ccff;
}

/* Definindo o estilo do corpo */
body {
  font-family: Arial, sans-serif;
  background-color: #f2f2f2;
  margin: 0;
  padding: 0;
}

/* Estilo do cabeçalho */
header {
  background-color: var(--cor-primaria);
  color: white;
  padding: 20px;
}

header h1 {
  font-size: 24px;
  margin: 0;
}

/* Estilo do conteúdo principal */
main {
  display: flex;
  justify-content: center;
  align-items: center;
  height: 400px;
}

/* Estilo do botão */
button {
  padding: 10px 20px;
  background-color: var(--cor-secundaria);
  color: white;
  border: none;
  border-radius: 5px;
  font-size: 18px;
  cursor: pointer;
  transition: background-color 0.3s;
}

button:hover {
  background-color: #0066cc;
}

/* Estilo do efeito de animação do botão */
button::before {
  content: "";
  position: absolute;
  width: 0;
  height: 100%;
  background-color: var(--cor-primaria);
  transition: width 0.3s;
}

button:hover::before {
  width: 100%;
}

/* Estilo do rodapé */
footer {
  background-color: var(--cor-primaria);
  color: white;
  padding: 10px;
  text-align: center;
}

/* Estilo do link no rodapé */
footer a {
  color: white;
  text-decoration: none;
}

footer a:hover {
  text-decoration: underline;
}
```

Neste código, começamos definindo algumas variáveis de cor no `:root` para serem utilizadas posteriormente. O estilo do `body` é configurado para ter uma fonte sans-serif, uma cor de fundo suave e margens e preenchimento zerados.

O elemento `header` é estilizado com uma cor de fundo primária e texto branco, além de um espaçamento interno de 20 pixels. O título dentro do `header` possui um tamanho de fonte de 24 pixels e margem zero.

O conteúdo principal dentro do elemento `main` é estilizado para ficar centralizado tanto vertical quanto horizontalmente. A altura é definida como 400 pixels.

O botão é estilizado com um espaçamento interno de 10 pixels no eixo vertical e 20 pixels no eixo horizontal, uma cor de fundo secundária, texto branco, borda nula, borda arredondada de 5 pixels, tamanho de fonte de 18 pixels, cursor do mouse como ponteiro e uma transição de cor de fundo de 0.3 segundos ao passar o mouse por cima. O pseudo-elemento `::before` é utilizado para criar um efeito de animação, onde uma barra de cor primária cresce horizontalmente a partir da esquerda ao passar o mouse por cima do botão.

O `footer` é estilizado com uma cor de fundo primária, texto branco, espaçamento interno de 10 pixels e alinhamento centralizado. Os links dentro do rodapé possuem cor branca e sem decoração, e ao passar o mouse por cima, eles receberão um sublinhado.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais ajuda, é só me perguntar.