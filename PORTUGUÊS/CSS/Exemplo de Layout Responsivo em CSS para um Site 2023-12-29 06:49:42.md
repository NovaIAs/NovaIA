Claro! Aqui está um exemplo de um código complexo em CSS:

```css
/* Estilo do contêiner principal */
.container {
  width: 100%;
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}

/* Estilo do cabeçalho */
.header {
  background-color: #333;
  color: #fff;
  padding: 20px;
  text-align: center;
}

.header h1 {
  font-size: 36px;
  margin: 0;
}

/* Estilo do menu de navegação */
.navbar {
  background-color: #f5f5f5;
  padding: 10px;
}

.navbar ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
}

.navbar li {
  display: inline-block;
  margin-right: 10px;
}

.navbar a {
  text-decoration: none;
  color: #333;
  padding: 5px;
}

/* Estilo do conteúdo principal */
.content {
  margin-top: 20px;
}

.content h2 {
  font-size: 24px;
  margin: 0;
  margin-bottom: 10px;
}

.content p {
  margin: 0;
  margin-bottom: 20px;
}

/* Estilo do rodapé */
.footer {
  background-color: #333;
  color: #fff;
  padding: 20px;
  text-align: center;
}

.footer p {
  margin: 0;
}

/* Estilo responsivo */
@media (max-width: 768px) {
  .container {
    padding: 10px;
  }

  .header h1 {
    font-size: 24px;
  }

  .navbar {
    padding: 5px;
  }

  .navbar li {
    margin-right: 5px;
  }

  .content h2 {
    font-size: 18px;
  }

  .footer {
    padding: 10px;
  }
}
```

Este código é um exemplo de um layout básico de um site responsivo. Ele possui estilos para o contêiner principal, cabeçalho, menu de navegação, conteúdo principal e rodapé. Também inclui um estilo responsivo que ajusta o layout para dispositivos móveis com uma largura máxima de 768px.

Para utilizá-lo, basta copiar esse código para um arquivo CSS e vinculá-lo ao seu documento HTML. Em seguida, você pode usar as classes e IDs fornecidos neste código para estilizar os elementos em seu HTML de acordo com o seu projeto.

Lembrando que este é apenas um exemplo e você pode personalizá-lo de acordo com suas necessidades.