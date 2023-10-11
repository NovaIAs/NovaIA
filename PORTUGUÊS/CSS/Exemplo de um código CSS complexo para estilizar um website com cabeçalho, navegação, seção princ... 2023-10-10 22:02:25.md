Claro! Aqui está um exemplo de um código CSS complexo que utiliza várias propriedades e seletores para criar um estilo único para um website:

```css
/* Reset CSS básico para garantir uma base consistente */
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Estilo global para todo o documento */
body {
  font-family: Arial, sans-serif;
  background-color: #f2f2f2;
  color: #333;
}

/* Estilizando o cabeçalho */
header {
  background-color: #3e3e3e;
  padding: 20px;
  text-align: center;
}

header h1 {
  color: #fff;
  font-size: 32px;
  margin-bottom: 10px;
}

header p {
  color: #ccc;
  font-size: 18px;
}

/* Estilizando a navegação */
nav {
  background-color: #333;
  padding: 10px;
  margin-top: 20px;
}

nav ul {
  list-style-type: none;
  display: flex;
  justify-content: center;
}

nav ul li {
  margin: 0 10px;
}

nav ul li a {
  color: #fff;
  text-decoration: none;
  font-size: 16px;
  padding: 5px 10px;
  border-radius: 5px;
}

nav ul li a:hover {
  background-color: #fff;
  color: #333;
}

/* Estilizando a seção principal */
section {
  padding: 30px;
  background-color: #fff;
  margin-top: 20px;
}

section h2 {
  font-size: 24px;
  margin-bottom: 10px;
}

section p {
  font-size: 16px;
  line-height: 1.5;
}

section img {
  max-width: 100%;
  height: auto;
  margin-bottom: 20px;
}

/* Estilizando o rodapé */
footer {
  background-color: #333;
  padding: 20px;
  text-align: center;
  color: #fff;
  margin-top: 20px;
}

footer p {
  font-size: 14px;
}

/* Media queries para tornar o layout responsivo */
@media screen and (max-width: 768px) {
  header h1 {
    font-size: 28px;
  }

  nav ul li a {
    font-size: 14px;
  }

  section h2 {
    font-size: 20px;
  }

  section p {
    font-size: 14px;
  }

  footer p {
    font-size: 12px;
  }
}
```

Este código CSS cria um estilo para um website com um cabeçalho, navegação, seção principal e rodapé. Ele também inclui media queries para tornar o layout responsivo em dispositivos de tela menor.

O reset CSS básico é aplicado a todos os elementos, removendo as margens e preenchimentos padrão e definindo a propriedade `box-sizing` como `border-box` para garantir que o tamanho dos elementos leve em consideração as bordas e o preenchimento.

O estilo global define a fonte, a cor de fundo e a cor do texto padrão para todo o documento.

O cabeçalho é estilizado com um fundo escuro, espaçamento interno e alinhamento centralizado. O título e parágrafo dentro do cabeçalho são estilizados com cores de texto e tamanhos de fonte específicos.

A navegação tem um fundo escuro e espaçamento interno. A lista de itens de navegação é exibida em uma linha usando a propriedade `display: flex`, e cada item de navegação tem margem para separação. Os links de navegação têm cores de texto e estilos de hover específicos.

A seção principal tem espaçamento interno, um fundo branco e uma margem superior. Os títulos e parágrafos dentro da seção principal têm tamanhos de fonte específicos, e as imagens têm largura máxima de 100% para garantir que não ultrapassem o tamanho da seção.

O rodapé tem um fundo escuro, espaçamento interno e alinhamento centralizado. O texto dentro do rodapé é estilizado com uma cor de texto específica.

Por fim, as media queries são usadas para tornar o layout responsivo em dispositivos de tela menor, ajustando os tamanhos de fonte para uma melhor legibilidade em telas menores.