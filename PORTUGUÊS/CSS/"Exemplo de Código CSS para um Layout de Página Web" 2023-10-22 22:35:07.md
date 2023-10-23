Claro, aqui está um código complexo em CSS:

```css
/* Estilo global */
body {
  font-family: Arial, sans-serif;
  margin: 0;
  padding: 0;
  background-color: #f2f2f2;
}

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}

/* Estilo do cabeçalho */
.header {
  background-color: #333;
  color: #fff;
  padding: 10px;
}

.header h1 {
  font-size: 24px;
  margin: 0;
}

/* Estilo do menu de navegação */
.nav {
  background-color: #555;
  padding: 10px;
}

.nav ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
}

.nav li {
  display: inline-block;
  margin-right: 10px;
}

.nav a {
  color: #fff;
  text-decoration: none;
  padding: 5px;
}

/* Estilo do conteúdo principal */
.main {
  background-color: #fff;
  padding: 20px;
}

.main h2 {
  font-size: 20px;
}

.main p {
  font-size: 16px;
}

/* Estilo do rodapé */
.footer {
  background-color: #333;
  color: #fff;
  padding: 10px;
  text-align: center;
}

/* Estilo responsivo para dispositivos móveis */
@media screen and (max-width: 768px) {
  .container {
    padding: 10px;
  }
  
  .header h1 {
    font-size: 20px;
  }
  
  .nav {
    padding: 5px;
  }
  
  .nav li {
    margin-right: 5px;
  }
  
  .main {
    padding: 10px;
  }
  
  .main h2 {
    font-size: 18px;
  }
  
  .main p {
    font-size: 14px;
  }
  
  .footer {
    padding: 5px;
  }
}
```

Neste código CSS, temos diferentes estilos aplicados a um layout básico de página web. Vou explicar cada parte do código:

- A primeira seção é o estilo global, onde definimos o estilo do corpo da página, a fonte utilizada, a margem, o preenchimento e a cor de fundo.

- Em seguida, temos o estilo do cabeçalho, com uma cor de fundo escura, cor do texto clara, preenchimento e tamanho da fonte.

- Depois, temos o estilo do menu de navegação, que possui uma cor de fundo mais escura, lista sem marcadores, os itens de menu são exibidos em linha, com margem entre eles, e os links têm cor clara e preenchimento.

- Na sequência, temos o estilo do conteúdo principal, com um fundo branco, preenchimento e tamanhos de fonte diferentes para títulos e parágrafos.

- Por fim, temos o estilo do rodapé, com uma cor de fundo escura, cor do texto clara, preenchimento e alinhamento central.

- Além disso, o código também inclui uma seção de estilo responsivo, usando a media query `@media`, que ajusta os estilos para dispositivos móveis com uma largura máxima de tela de 768px.

Esse é um exemplo de código complexo em CSS que pode ser adaptado e estilizado de acordo com as necessidades de um projeto.