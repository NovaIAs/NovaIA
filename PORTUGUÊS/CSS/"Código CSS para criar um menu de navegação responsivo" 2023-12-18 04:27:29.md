Claro! Aqui está um código complexo em CSS para criar um menu de navegação responsivo:

```css
/* Estilos base */
body {
  font-family: Arial, sans-serif;
  margin: 0;
  padding: 0;
}

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}

/* Estilos do menu */
.navbar {
  background-color: #333;
  color: #fff;
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 10px;
}

.navbar-brand {
  font-size: 24px;
}

.navbar-nav {
  display: flex;
  list-style: none;
  margin: 0;
  padding: 0;
}

.nav-item {
  margin-right: 10px;
}

.nav-link {
  color: #fff;
  text-decoration: none;
  padding: 5px;
}

.nav-link:hover {
  background-color: #555;
}

/* Estilos do menu responsivo */
/* Toggle Button */
.navbar-toggle {
  display: none;
}

.navbar-toggle:checked ~ .navbar-nav {
  display: block;
}

/* Estilos do ícone do Toggle Button */
.navbar-toggle-label {
  display: block;
  font-size: 24px;
  cursor: pointer;
}

.navbar-toggle-label:hover {
  background-color: #555;
}

/* Media Query para dispositivos menores */
@media screen and (max-width: 768px) {
  .navbar {
    flex-direction: column;
  }

  .navbar-nav {
    display: none;
    margin-top: 10px;
  }

  .nav-item {
    margin: 5px 0;
  }

  .navbar-toggle {
    display: block;
  }
}
```

Explicação do código:

1. Definimos alguns estilos base para o documento, como a fonte e margens.
2. A classe `.container` é utilizada para centralizar o conteúdo do menu na página.
3. Definimos os estilos para a classe `.navbar`, que é o contêiner principal do menu de navegação.
4. `.navbar-brand` define o estilo do logotipo ou título do menu.
5. `.navbar-nav` é uma lista não ordenada que contém os itens do menu.
6. `.nav-item` é um item individual do menu.
7. `.nav-link` é o estilo dos links de navegação.
8. Adicionamos um estilo de hover para os links quando o mouse estiver sobre eles.
9. A classe `.navbar-toggle` é usada para ocultar o menu em dispositivos menores.
10. A classe `.navbar-toggle:checked ~ .navbar-nav` exibe o menu quando o botão toggle é clicado.
11. Estilizamos o rótulo do botão toggle.
12. No media query `@media screen and (max-width: 768px)`, definimos os estilos para dispositivos menores. O menu passa a ser exibido em colunas e o botão toggle é exibido.
13. Os itens do menu são ocultos e exibidos quando o botão toggle é clicado.

Este código cria um menu de navegação responsivo, que se adapta a diferentes tamanhos de tela. Os estilos são definidos usando CSS puro e estão escritos em português para facilitar a compreensão.