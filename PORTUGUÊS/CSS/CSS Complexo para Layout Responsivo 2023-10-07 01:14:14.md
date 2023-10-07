Claro! Aqui está um exemplo de um código complexo em CSS:

```css
/* Estilo global */
body {
  font-family: Arial, sans-serif;
  background-color: #f2f2f2;
  margin: 0;
  padding: 0;
}

/* Cabeçalho */
header {
  background-color: #333;
  color: #fff;
  padding: 20px;
}

header h1 {
  font-size: 24px;
  margin: 0;
}

/* Navegação */
nav {
  background-color: #999;
  padding: 10px;
}

nav ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
}

nav ul li {
  display: inline;
  margin-right: 10px;
}

nav ul li a {
  color: #fff;
  text-decoration: none;
}

/* Conteúdo principal */
main {
  padding: 20px;
}

/* Colunas */
.column {
  float: left;
  width: 33.33%;
}

/* Rodapé */
footer {
  background-color: #333;
  color: #fff;
  padding: 20px;
  text-align: center;
}

/* Responsividade */
@media (max-width: 768px) {
  .column {
    width: 100%;
    margin-bottom: 20px;
  }
}
```

Explicando o código:

1. Definimos o estilo global: definimos a fonte, a cor de fundo, a margem e o preenchimento do corpo do documento.
2. Estilizamos o cabeçalho: definimos a cor de fundo, a cor do texto e o preenchimento do cabeçalho. Também definimos o tamanho da fonte e a margem do título dentro do cabeçalho.
3. Estilizamos a navegação: definimos a cor de fundo e o preenchimento da navegação. Além disso, removemos a lista padrão e o espaçamento dos itens da lista. Definimos que os itens da lista devem ser exibidos em linha e adicionamos margem direita aos itens. Definimos a cor e a remoção do sublinhado para os links da navegação.
4. Estilizamos o conteúdo principal: adicionamos um preenchimento ao conteúdo principal.
5. Definimos as colunas: definimos que as colunas devem ser exibidas lado a lado e ocupar 33,33% da largura disponível.
6. Estilizamos o rodapé: definimos a cor de fundo, a cor do texto e o preenchimento do rodapé. Também definimos o alinhamento centralizado do texto no rodapé.
7. Adicionamos uma regra de mídia para tornar o layout responsivo: quando a largura da tela for menor ou igual a 768 pixels, as colunas devem ocupar 100% da largura disponível e adicionar uma margem inferior.

Este é um exemplo de código complexo em CSS que pode ser usado como base para a criação de um layout personalizado.