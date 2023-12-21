Claro! Aqui está um exemplo complexo em CSS que combina diferentes propriedades e seletores para criar um efeito visual interessante:

```css
/* Definindo as cores */
:root {
  --cor-primaria: #ff0000;
  --cor-secundaria: #00ff00;
  --cor-terciaria: #0000ff;
}

/* Estilizando o body */
body {
  font-family: Arial, sans-serif;
  background-color: #f2f2f2;
}

/* Estilizando o cabeçalho */
header {
  background-color: var(--cor-primaria);
  padding: 20px;
  color: white;
  text-align: center;
}

header h1 {
  font-size: 36px;
  margin: 0;
}

/* Estilizando a navegação */
nav {
  background-color: var(--cor-secundaria);
  padding: 10px;
}

nav ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
  display: flex;
  justify-content: center;
}

nav ul li {
  margin: 0 10px;
}

nav ul li a {
  text-decoration: none;
  color: white;
}

/* Estilizando o conteúdo */
main {
  display: flex;
  justify-content: center;
  align-items: center;
  min-height: 400px;
}

main h2 {
  font-size: 24px;
  color: var(--cor-terciaria);
}

/* Estilizando o rodapé */
footer {
  background-color: var(--cor-primaria);
  padding: 20px;
  color: white;
  text-align: center;
}

/* Media queries para responsividade */
@media screen and (max-width: 768px) {
  header h1 {
    font-size: 24px;
  }
  
  nav ul li {
    margin: 0 5px;
  }
  
  main h2 {
    font-size: 18px;
  }
}
```

Neste código CSS complexo, temos uma estrutura básica de um site, com um cabeçalho, navegação, conteúdo e rodapé. O código faz uso de variáveis CSS (definidas usando `:root`) para armazenar as cores principais do site. O cabeçalho possui um título definido com `h1` e o corpo da página é estilizado usando o seletor `body`.

A navegação é estilizada com um fundo verde (`--cor-secundaria`) e os itens de menu são alinhados horizontalmente usando `flexbox`. O conteúdo principal é centralizado vertical e horizontalmente, com um título no centro (`h2`).

O rodapé usa uma cor de fundo vermelha (`--cor-primaria`) e tem um texto centralizado.

Finalmente, temos uma media query para tornar o site responsivo para dispositivos com largura máxima de 768 pixels, ajustando o tamanho das fontes e margens para uma melhor experiência de visualização nessas telas menores.

Espero que este exemplo de código complexo em CSS seja útil para você!