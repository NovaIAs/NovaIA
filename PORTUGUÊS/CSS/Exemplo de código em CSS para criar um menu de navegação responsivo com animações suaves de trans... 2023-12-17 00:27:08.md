Claro! Aqui está um exemplo de código em CSS que cria um menu de navegação responsivo com animações suaves de transição:

```css
/* Define as cores */
:root {
  --cor-primaria: #ff8c00;
  --cor-secundaria: #f0f0f0;
}

/* Estiliza o corpo da página */
body {
  margin: 0;
  padding: 0;
  font-family: Arial, sans-serif;
  background-color: var(--cor-secundaria);
}

/* Estiliza o cabeçalho */
header {
  background-color: var(--cor-primaria);
  color: white;
  padding: 20px;
}

/* Estiliza o logo */
header .logo {
  font-size: 24px;
  font-weight: bold;
}

/* Estiliza o menu de navegação */
nav {
  background-color: var(--cor-primaria);
}

nav ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
  overflow: hidden;
}

nav li {
  float: left;
}

nav li a {
  display: block;
  color: white;
  text-align: center;
  padding: 14px 16px;
  text-decoration: none;
  transition: background-color 0.3s ease;
}

nav li a:hover {
  background-color: #ff6600;
}

/* Estiliza o conteúdo da página */
main {
  padding: 20px;
}

/* Estiliza o rodapé */
footer {
  background-color: var(--cor-primaria);
  color: white;
  padding: 20px;
  text-align: center;
}

/* Estilos para dispositivos móveis */
@media only screen and (max-width: 600px) {
  nav ul {
    display: none;
  }

  nav li {
    width: 100%;
  }

  nav li a {
    padding: 10px;
  }

  nav li.icon {
    float: right;
    display: block;
  }

  nav li.icon a {
    display: block;
    text-align: center;
  }

  nav.responsive {
    position: relative;
  }

  nav.responsive li.icon {
    position: absolute;
    right: 0;
    top: 0;
  }

  nav.responsive li {
    float: none;
    display: inline;
  }

  nav.responsive li a {
    display: block;
    text-align: left;
  }
}
```

Este código cria um menu de navegação responsivo, onde os itens do menu são exibidos na horizontal. Quando a tela é redimensionada para dispositivos móveis com uma largura máxima de 600 pixels, o menu se transforma em um ícone de menu responsivo, que, quando clicado, exibe os itens do menu verticalmente.

O código começa definindo as cores através de variáveis CSS, `--cor-primaria` e `--cor-secundaria`. Em seguida, estiliza o corpo da página, o cabeçalho, o logo, o menu de navegação, o conteúdo da página e o rodapé.

A parte mais interessante é a seção de estilos para dispositivos móveis, onde é utilizado o media query `@media only screen and (max-width: 600px)` para aplicar estilos específicos quando a largura da tela é menor ou igual a 600 pixels. Nessa parte, o menu é escondido e substituído por um ícone de menu responsivo (representado por `nav li.icon`). Quando o ícone é clicado, o menu é exibido verticalmente.

Espero que este código atenda às suas expectativas!