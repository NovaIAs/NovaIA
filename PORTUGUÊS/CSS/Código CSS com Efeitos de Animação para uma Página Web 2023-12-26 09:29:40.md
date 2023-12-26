Claro! Aqui está um código complexo em CSS que utiliza diferentes propriedades e seletores para criar um efeito de animação chamativo:

```css
/* Definindo as cores */
:root {
  --cor-primaria: #ff5a5f;
  --cor-secundaria: #00a699;
}

/* Estilizando o corpo do documento */
body {
  background-color: #f2f2f2;
  font-family: Arial, sans-serif;
  margin: 0;
  padding: 0;
}

/* Estilizando o cabeçalho */
header {
  background-color: var(--cor-primaria);
  color: white;
  padding: 20px;
  text-align: center;
}

h1 {
  font-size: 32px;
  margin: 0;
}

/* Estilizando o menu de navegação */
nav {
  background-color: var(--cor-secundaria);
  padding: 10px;
}

nav ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
  text-align: center;
}

nav ul li {
  display: inline;
}

nav ul li a {
  color: white;
  display: inline-block;
  padding: 10px 20px;
  text-decoration: none;
}

/* Estilizando a seção principal */
section {
  padding: 20px;
}

h2 {
  color: var(--cor-primaria);
  font-size: 24px;
  margin-bottom: 10px;
}

p {
  line-height: 1.5;
  margin-bottom: 20px;
}

/* Estilizando o rodapé */
footer {
  background-color: var(--cor-primaria);
  color: white;
  padding: 20px;
  text-align: center;
}

/* Criando animações */
@keyframes fade-in {
  0% {
    opacity: 0;
  }
  100% {
    opacity: 1;
  }
}

@keyframes slide-up {
  0% {
    transform: translateY(100%);
  }
  100% {
    transform: translateY(0);
  }
}

/* Aplicando animações aos elementos */
header {
  animation: fade-in 1s ease-in-out;
}

nav {
  animation: slide-up 1s ease-in-out;
}

section {
  animation: fade-in 1s ease-in-out;
}

footer {
  animation: slide-up 1s ease-in-out;
}
```

Explicação do código:

1. Definimos variáveis de cores usando a sintaxe `--nome-da-variavel: valor`.
2. Estilizamos o corpo do documento, definindo a cor de fundo, a família de fontes e removendo margens e espaçamentos.
3. Estilizamos o cabeçalho, definindo a cor de fundo, a cor do texto, o preenchimento interno e o alinhamento centralizado.
4. Estilizamos o menu de navegação, definindo a cor de fundo e o preenchimento interno.
5. Criamos uma lista não ordenada para o menu de navegação, removendo margens e espaçamentos e alinhando o texto ao centro.
6. Estilizamos cada item da lista no menu de navegação, exibindo-os em linha e removendo marcadores.
7. Estilizamos os links dentro dos itens do menu de navegação, definindo a cor do texto, o preenchimento interno e removendo a decoração de texto.
8. Estilizamos a seção principal, definindo espaçamento interno.
9. Estilizamos os cabeçalhos da seção principal, definindo a cor do texto, o tamanho da fonte e a margem inferior.
10. Estilizamos os parágrafos da seção principal, definindo o espaçamento entre linhas e a margem inferior.
11. Estilizamos o rodapé, definindo a cor de fundo, a cor do texto, o preenchimento interno e o alinhamento centralizado.
12. Criamos animações usando a regra `@keyframes`. No exemplo, criamos duas animações chamadas `fade-in` e `slide-up`.
13. Aplicamos as animações aos elementos desejados usando a propriedade `animation`, especificando o nome da animação, a duração, a transição e a direção.

Esse código cria uma página com um cabeçalho, um menu de navegação, uma seção principal e um rodapé. Além disso, adiciona animações para tornar a página mais dinâmica.