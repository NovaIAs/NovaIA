Claro! Aqui está um exemplo de um código complexo em CSS:

```css
/* Definição de variáveis */
:root {
  --cor-primaria: #ff0000;
  --cor-secundaria: #00ff00;
  --cor-terciaria: #0000ff;
}

/* Estilo do corpo da página */
body {
  background-color: var(--cor-primaria);
  font-family: Arial, sans-serif;
  color: var(--cor-secundaria);
}

/* Estilo do cabeçalho */
header {
  background-color: var(--cor-secundaria);
  padding: 20px;
  color: var(--cor-primaria);
}

header h1 {
  font-size: 36px;
  margin: 0;
}

/* Estilo do menu de navegação */
nav {
  background-color: var(--cor-terciaria);
  padding: 10px;
}

nav ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
}

nav ul li {
  display: inline;
}

nav ul li a {
  color: var(--cor-primaria);
  text-decoration: none;
  padding: 10px;
}

nav ul li a:hover {
  background-color: var(--cor-secundaria);
  color: var(--cor-terciaria);
}

/* Estilo do conteúdo principal */
main {
  padding: 20px;
}

main h2 {
  font-size: 24px;
}

main p {
  line-height: 1.5;
}

/* Estilo do rodapé */
footer {
  background-color: var(--cor-secundaria);
  padding: 20px;
  color: var(--cor-primaria);
  text-align: center;
}

/* Estilo do botão */
.button {
  display: inline-block;
  background-color: var(--cor-primaria);
  color: var(--cor-secundaria);
  border: none;
  padding: 10px 20px;
  text-align: center;
  text-decoration: none;
  font-size: 16px;
  cursor: pointer;
  margin: 10px;
}

.button:hover {
  background-color: var(--cor-secundaria);
  color: var(--cor-terciaria);
}
```

Neste código, temos a definição de variáveis CSS no início do arquivo, onde são definidas as cores primária, secundária e terciária. Em seguida, temos o estilo do corpo da página, definindo a cor de fundo, a fonte e a cor do texto.

O cabeçalho é estilizado com um fundo, padding e cor de texto específicos. O mesmo acontece com o menu de navegação, onde os itens de lista são exibidos em linha e os links são estilizados com cores diferentes ao passar o mouse.

O conteúdo principal é estilizado com um padding e tamanhos de fonte diferentes para os títulos e parágrafos. O rodapé segue um estilo semelhante ao cabeçalho.

Por fim, temos um estilo para um botão, que é exibido como um bloco em linha, com cores de fundo e texto específicas, e muda de cor ao passar o mouse sobre ele.

Este código complexo em CSS é apenas um exemplo de como você pode estilizar uma página da web de forma detalhada, permitindo a customização de cores e estilos de elementos específicos.