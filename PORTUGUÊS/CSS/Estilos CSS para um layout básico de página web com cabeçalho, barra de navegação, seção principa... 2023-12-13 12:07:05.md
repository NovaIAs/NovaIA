Claro! Aqui está um código complexo em CSS, com comentários explicativos em português:

```css
/* Definindo estilos gerais */
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Definindo as cores da paleta */
:root {
  --cor-primaria: #ff0000;
  --cor-secundaria: #00ff00;
  --cor-terciaria: #0000ff;
}

/* Estilizando o corpo da página */
body {
  font-family: Arial, sans-serif;
  background-color: #f0f0f0;
  color: #333333;
}

/* Estilizando o cabeçalho */
header {
  background-color: var(--cor-primaria);
  padding: 20px;
  text-align: center;
}

header h1 {
  color: #ffffff;
  font-size: 30px;
  font-weight: bold;
}

/* Estilizando a barra de navegação */
nav {
  background-color: var(--cor-secundaria);
  padding: 10px;
}

nav ul {
  list-style: none;
  display: flex;
  justify-content: center;
}

nav ul li {
  margin: 0 10px;
}

nav ul li a {
  color: #ffffff;
  text-decoration: none;
  padding: 5px;
}

nav ul li a:hover {
  background-color: rgba(255, 255, 255, 0.5);
  border-radius: 5px;
}

/* Estilizando a seção principal */
section {
  margin: 20px;
}

section h2 {
  font-size: 24px;
  margin-bottom: 10px;
}

section p {
  font-size: 16px;
  line-height: 1.5;
}

/* Estilizando as caixas de destaque */
.destaque {
  background-color: var(--cor-terciaria);
  color: #ffffff;
  padding: 20px;
  margin-bottom: 20px;
}

.destaque h3 {
  font-size: 18px;
  font-weight: bold;
  margin-bottom: 10px;
}

.destaque p {
  font-size: 14px;
}

/* Estilizando o rodapé */
footer {
  background-color: #333333;
  padding: 20px;
  text-align: center;
}

footer p {
  color: #ffffff;
  font-size: 12px;
}

/* Adicionando uma animação */
@keyframes animacao-destaque {
  0% {
    background-color: var(--cor-primaria);
    transform: rotate(0deg);
  }
  50% {
    background-color: var(--cor-secundaria);
    transform: rotate(180deg);
  }
  100% {
    background-color: var(--cor-terciaria);
    transform: rotate(360deg);
  }
}

/* Aplicando a animação nas caixas de destaque */
.destaque {
  animation-name: animacao-destaque;
  animation-duration: 3s;
  animation-iteration-count: infinite;
  animation-timing-function: linear;
}
```

Esse código CSS apresenta estilos para um layout básico de página web, incluindo um cabeçalho, barra de navegação, seção principal, caixas de destaque, rodapé e uma animação aplicada nas caixas de destaque.

Os estilos gerais definem que todos os elementos devem ter margem e preenchimento zerados, e a caixa de conteúdo deve ser dimensionada de acordo com a propriedade `border-box`.

A paleta de cores é definida usando a pseudo-classe `:root`, onde são definidas três variáveis de cor: `--cor-primaria`, `--cor-secundaria` e `--cor-terciaria`.

O cabeçalho possui um fundo vermelho (`--cor-primaria`) e um título centralizado em branco (`#ffffff`).

A barra de navegação possui um fundo verde (`--cor-secundaria`) e uma lista horizontal de links.

A seção principal contém títulos e parágrafos.

As caixas de destaque possuem um fundo azul (`--cor-terciaria`), com títulos e parágrafos em branco.

O rodapé possui um fundo escuro (`#333333`) e um parágrafo em branco.

Por fim, é adicionada uma animação chamada `animacao-destaque`, que alterna as cores de fundo das caixas de destaque e faz uma rotação de 360 graus. Essa animação dura 3 segundos e se repete infinitamente.

Espero que isso atenda às suas expectativas!