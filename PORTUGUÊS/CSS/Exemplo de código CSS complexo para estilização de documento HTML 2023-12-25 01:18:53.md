Claro! Aqui está um exemplo complexo de código em CSS que utiliza as diferentes propriedades e seletores da linguagem:

```css
/* Estilo para o corpo do documento */
body {
  font-family: Arial, sans-serif;
  background-color: #f2f2f2;
  margin: 0;
  padding: 0;
}

/* Estilo para o cabeçalho */
header {
  background-color: #333;
  color: #fff;
  padding: 20px;
}

/* Estilo para o título do cabeçalho */
header h1 {
  font-size: 24px;
}

/* Estilo para a navegação */
nav {
  background-color: #666;
  padding: 10px;
}

/* Estilo para os links na navegação */
nav a {
  color: #fff;
  text-decoration: none;
  padding: 5px;
  margin-right: 10px;
}

/* Estilo para os links na navegação quando estão ativos */
nav a:active,
nav a:hover {
  background-color: #999;
}

/* Estilo para o conteúdo principal */
main {
  padding: 20px;
}

/* Estilo para os títulos das seções */
main h2 {
  color: #333;
  font-size: 20px;
}

/* Estilo para os parágrafos */
main p {
  line-height: 1.5;
  margin-bottom: 10px;
}

/* Estilo para uma classe específica */
.destaque {
  font-weight: bold;
  color: #ff0000;
}

/* Estilo para uma classe específica dentro de um seletor específico */
main article.destaque p {
  font-size: 18px;
}

/* Estilo para uma lista não ordenada */
ul {
  list-style-type: square;
}

/* Estilo para elementos específicos dentro de uma lista */
ul li:nth-child(odd) {
  background-color: #f2f2f2;
}

/* Estilo para uma tabela */
table {
  border-collapse: collapse;
  width: 100%;
}

/* Estilo para as células da tabela */
table td, table th {
  border: 1px solid #ddd;
  padding: 8px;
}

/* Estilo para os cabeçalhos da tabela */
table th {
  background-color: #f2f2f2;
  text-align: left;
}

/* Estilo para um formulário */
form {
  margin-bottom: 20px;
}

/* Estilo para os campos de entrada */
form input[type="text"],
form input[type="email"],
form textarea {
  width: 100%;
  padding: 10px;
  margin-bottom: 10px;
  border: 1px solid #ccc;
  border-radius: 4px;
}

/* Estilo para o botão de envio */
form input[type="submit"] {
  background-color: #333;
  color: #fff;
  border: none;
  padding: 10px 20px;
  cursor: pointer;
}

/* Estilo para uma mensagem de erro */
.error {
  color: #ff0000;
  font-weight: bold;
}
```

Neste exemplo, o código CSS é utilizado para estilizar um documento HTML. Ele define estilos para o corpo do documento, cabeçalho, navegação, conteúdo principal, entre outros elementos.

Cada seção do código é explicada através de comentários em CSS. Por exemplo, o seletor `body` define estilos para o corpo do documento, como a fonte, cor de fundo, margens e preenchimento.

Além disso, o código também utiliza seletores aninhados, como `header h1`, para estilizar elementos específicos dentro de outros elementos. Também são utilizadas classes, como `.destaque`, para aplicar estilos específicos a elementos que possuam essa classe.

O código também inclui exemplos de estilos para listas, tabelas, formulários e mensagens de erro. Esses estilos são aplicados a elementos específicos, como `ul`, `table`, `form` e `.error`.

Esse é apenas um exemplo de código CSS complexo, com diversas propriedades e seletores. A complexidade pode ser aumentada ou adaptada de acordo com as necessidades do projeto.