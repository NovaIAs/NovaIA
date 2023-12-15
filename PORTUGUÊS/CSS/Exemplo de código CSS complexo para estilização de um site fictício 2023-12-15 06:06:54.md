Claro! Aqui está um exemplo de código CSS complexo, que utiliza seletores avançados e propriedades diversas para estilização de um site fictício:

```css
/* Estilo global */
body {
  font-family: Arial, sans-serif;
  background-color: #f2f2f2;
  color: #333333;
}

/* Estilizando a barra de navegação */
.navbar {
  background-color: #333333;
  height: 50px;
  width: 100%;
  position: fixed;
  top: 0;
  left: 0;
  z-index: 999;
}

.navbar ul {
  list-style: none;
  padding: 0;
  margin: 0;
  display: flex;
  justify-content: center;
  align-items: center;
  height: 100%;
}

.navbar li {
  margin: 0 10px;
}

.navbar a {
  color: #ffffff;
  text-decoration: none;
  padding: 10px;
  transition: color 0.3s ease;
}

.navbar a:hover {
  color: #ff0000;
}

/* Criando uma seção de destaque */
.section-highlight {
  background-color: #ffffff;
  padding: 50px;
  margin: 50px auto;
  max-width: 800px;
  box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
}

.section-highlight h2 {
  color: #333333;
  font-size: 24px;
  margin-bottom: 20px;
}

.section-highlight p {
  font-size: 16px;
  line-height: 1.5;
}

/* Estilizando uma tabela */
.table {
  width: 100%;
  margin-bottom: 20px;
}

.table th, .table td {
  padding: 10px;
  text-align: left;
}

.table th {
  background-color: #333333;
  color: #ffffff;
}

/* Criando uma animação */
@keyframes fade-in {
  0% {
    opacity: 0;
  }
  100% {
    opacity: 1;
  }
}

.animated-element {
  animation: fade-in 1s ease-in-out;
}

/* Estilizando um formulário */
.form {
  max-width: 400px;
  margin: 0 auto;
}

.form label {
  font-weight: bold;
}

.form input, .form textarea {
  width: 100%;
  padding: 10px;
  margin-bottom: 10px;
  border: 1px solid #cccccc;
}

/* Estilizando um botão */
.button {
  display: inline-block;
  padding: 10px 20px;
  background-color: #333333;
  color: #ffffff;
  border: none;
  text-decoration: none;
  transition: background-color 0.3s ease;
}

.button:hover {
  background-color: #ff0000;
}
```

Neste exemplo, o código CSS estiliza um site fictício. Ele cria uma barra de navegação fixa no topo da página, uma seção de destaque com título e parágrafo, uma tabela estilizada, uma animação de fade-in em um elemento, um formulário com campos de input e textarea, e um botão estilizado. O código também utiliza seletores como `body`, `.navbar`, `.section-highlight`, `.table`, `.animated-element`, `.form`, e `.button`, aplicando propriedades como `background-color`, `color`, `font-size`, `margin`, entre outras.