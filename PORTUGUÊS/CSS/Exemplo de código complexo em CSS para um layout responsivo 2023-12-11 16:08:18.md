Claro! Aqui está um exemplo de um código complexo em CSS:

```css
/* ESTILOS GLOBAIS */
* {
  box-sizing: border-box;
}

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

/* ESTILOS DO CABEÇALHO */
header {
  background-color: #333;
  color: #fff;
  padding: 10px;
}

.logo {
  font-size: 24px;
  font-weight: bold;
}

.navbar {
  margin-top: 20px;
}

.navbar ul {
  list-style-type: none;
  padding: 0;
  margin: 0;
}

.navbar li {
  display: inline-block;
  margin-right: 10px;
}

.navbar a {
  color: #fff;
  text-decoration: none;
  padding: 5px;
}

.navbar a:hover {
  background-color: #555;
}

/* ESTILOS DO CONTEÚDO PRINCIPAL */
main {
  background-color: #fff;
  margin-top: 20px;
  padding: 20px;
}

.title {
  font-size: 32px;
  font-weight: bold;
  margin-bottom: 20px;
}

.subtitle {
  font-size: 18px;
  font-weight: bold;
  margin-bottom: 10px;
}

.paragraph {
  margin-bottom: 20px;
}

.button {
  display: inline-block;
  background-color: #555;
  color: #fff;
  padding: 10px 20px;
  text-decoration: none;
  border-radius: 5px;
}

.button:hover {
  background-color: #333;
}

/* ESTILOS DO RODAPÉ */
footer {
  background-color: #333;
  color: #fff;
  padding: 10px;
  text-align: center;
}

.footer-text {
  font-size: 14px;
}

/* ESTILOS RESPONSIVOS */
@media (max-width: 768px) {
  .container {
    padding: 10px;
  }
  
  header {
    padding: 5px;
  }
  
  .navbar {
    margin-top: 10px;
  }
  
  .title {
    font-size: 24px;
    margin-bottom: 10px;
  }
  
  .subtitle {
    font-size: 14px;
  }
  
  .button {
    padding: 5px 10px;
  }
  
  footer {
    padding: 5px;
  }
  
  .footer-text {
    font-size: 12px;
  }
}
```

Este código CSS cria um layout responsivo para um site. Ele inicia definindo estilos globais, como a caixa de modelo, a fonte e a cor de fundo do corpo do documento.

Em seguida, temos estilos para o cabeçalho, onde definimos a cor de fundo, a cor do texto e o espaçamento interno. A classe `.logo` define o estilo da logomarca e a classe `.navbar` contém a estilização da barra de navegação.

Na seção de estilos do conteúdo principal, temos o estilo para o elemento `<main>`, onde definimos a cor de fundo, o espaçamento e os estilos para os títulos, subtítulos, parágrafos e botões.

Por fim, temos os estilos do rodapé, onde definimos a cor de fundo, a cor do texto e o espaçamento interno. A classe `.footer-text` define o estilo do texto no rodapé.

O código também inclui estilos responsivos, que são aplicados quando a largura da tela é igual ou inferior a 768 pixels. Nesse caso, os estilos são ajustados para uma exibição melhor em dispositivos móveis.

Essa é apenas uma amostra básica de um código CSS complexo. Você pode personalizar e expandir esses estilos de acordo com as necessidades do seu projeto.