Claro! Aqui está um código complexo em CSS que utiliza várias propriedades e seletores diferentes:

```css
/* Reset de estilos */
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Estilos para o corpo da página */
body {
  font-family: Arial, sans-serif;
  background-color: #f5f5f5;
  color: #333;
}

/* Estilos para um cabeçalho personalizado */
header {
  background-color: #333;
  padding: 20px;
  color: #fff;
}

header h1 {
  font-size: 24px;
  margin-bottom: 10px;
}

header p {
  font-size: 14px;
}

/* Estilos para uma seção de conteúdo */
section {
  background-color: #fff;
  padding: 20px;
  margin-bottom: 20px;
  box-shadow: 0 0 5px rgba(0, 0, 0, 0.1);
}

section h2 {
  font-size: 20px;
  margin-bottom: 10px;
}

section p {
  font-size: 16px;
  line-height: 1.5;
}

/* Estilos para uma lista não ordenada */
ul {
  list-style: none;
}

ul li:before {
  content: "•";
  margin-right: 10px;
}

/* Estilos para um formulário de contato */
form {
  margin-top: 20px;
}

form label {
  display: block;
  margin-bottom: 10px;
  font-weight: bold;
}

form input[type="text"],
form textarea {
  width: 100%;
  padding: 10px;
  margin-bottom: 10px;
  border: 1px solid #ccc;
}

form input[type="submit"] {
  background-color: #333;
  color: #fff;
  padding: 10px 20px;
  border: none;
  cursor: pointer;
}

form input[type="submit"]:hover {
  background-color: #555;
}

/* Estilos para um rodapé personalizado */
footer {
  background-color: #333;
  padding: 20px;
  color: #fff;
  text-align: center;
}

footer p {
  font-size: 14px;
}

/* Responsividade para dispositivos móveis */
@media only screen and (max-width: 600px) {
  header {
    padding: 10px;
  }
  
  header h1 {
    font-size: 18px;
  }
  
  section {
    padding: 10px;
  }
  
  section h2 {
    font-size: 16px;
  }
  
  section p {
    font-size: 14px;
  }
  
  form input[type="text"],
  form textarea {
    padding: 5px;
  }
  
  form input[type="submit"] {
    padding: 5px 10px;
  }
  
  footer {
    padding: 10px;
  }
  
  footer p {
    font-size: 12px;
  }
}
```

Esse código CSS é um exemplo de uma estrutura completa para uma página web. Ele contém estilos para o cabeçalho, seção de conteúdo, formulário de contato, rodapé e também inclui regras de responsividade para dispositivos móveis.

No início, temos um reset de estilos para zerar as margens e paddings padrão dos elementos. Em seguida, definimos os estilos para o corpo da página, como a fonte, cor de fundo e cor do texto.

Depois, temos os estilos para o cabeçalho, onde definimos a cor de fundo, padding e cor do texto. Dentro do cabeçalho, temos um título (`h1`) e um parágrafo (`p`).

Em seguida, temos os estilos para a seção de conteúdo, onde definimos a cor de fundo, padding, margem inferior e uma sombra. Dentro da seção, temos um título (`h2`) e um parágrafo (`p`).

Logo após, temos os estilos para uma lista não ordenada, onde definimos que a lista não terá marcadores padrão e utilizamos o seletor `:before` para adicionar um marcador personalizado.

Em seguida, temos os estilos para um formulário de contato, onde definimos a margem superior e outros estilos para os rótulos, campos de texto e botão de envio.

Por fim, temos os estilos para o rodapé, onde definimos a cor de fundo, padding, cor do texto e alinhamento central. Dentro do rodapé, temos um parágrafo (`p`).

Além disso, o código também inclui uma regra de media query para dispositivos móveis, onde definimos estilos alternativos para telas com largura máxima de 600 pixels. Nessa regra, diminuímos o tamanho dos elementos para melhorar a experiência em dispositivos menores.

Espero que esse código atenda às suas expectativas! Caso precise de mais alguma coisa, estou à disposição.