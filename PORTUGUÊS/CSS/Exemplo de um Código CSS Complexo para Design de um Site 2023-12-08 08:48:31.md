Claro! Aqui está um exemplo de um código CSS complexo que pode ser utilizado em um projeto de design de um site:

```css
/* Definição das variáveis */

:root {
  --cor-primaria: #FF0000;
  --cor-secundaria: #00FF00;
  --tamanho-fonte: 16px;
}

/* Estilos gerais */

body {
  font-family: Arial, sans-serif;
  background-color: #F5F5F5;
  color: #333333;
  margin: 0;
  padding: 0;
}

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}

/* Estilos do cabeçalho */

header {
  background-color: var(--cor-primaria);
  padding: 20px;
}

header h1 {
  color: white;
  font-size: 24px;
  margin: 0;
}

header nav ul {
  list-style-type: none;
  padding: 0;
  margin: 0;
  display: flex;
}

header nav li {
  margin-right: 20px;
}

header nav a {
  color: white;
  text-decoration: none;
  font-size: 18px;
}

/* Estilos do conteúdo */

section {
  margin-bottom: 40px;
}

section h2 {
  color: var(--cor-primaria);
  font-size: 20px;
  margin-bottom: 10px;
}

section p {
  font-size: var(--tamanho-fonte);
  line-height: 1.5;
}

section img {
  max-width: 100%;
  height: auto;
  margin-bottom: 20px;
}

/* Estilos do rodapé */

footer {
  background-color: var(--cor-secundaria);
  color: white;
  padding: 10px;
  text-align: center;
}

footer p {
  font-size: 12px;
  margin: 0;
}

/* Estilos das animações */

@keyframes fade-in {
  from {
    opacity: 0;
  }
  to {
    opacity: 1;
  }
}

.fade-in-element {
  animation-name: fade-in;
  animation-duration: 2s;
}

@keyframes slide-in {
  from {
    transform: translateX(100%);
  }
  to {
    transform: translateX(0);
  }
}

.slide-in-element {
  animation-name: slide-in;
  animation-duration: 1.5s;
}

@keyframes spin {
  from {
    transform: rotate(0deg);
  }
  to {
    transform: rotate(360deg);
  }
}

.spinning-element {
  animation-name: spin;
  animation-duration: 1s;
  animation-timing-function: linear;
  animation-iteration-count: infinite;
}

/* Media queries */

@media (max-width: 768px) {
  header h1 {
    font-size: 20px;
  }

  section h2 {
    font-size: 18px;
  }

  section p {
    font-size: 14px;
  }
}
```

Neste exemplo, são definidas algumas variáveis CSS utilizando a declaração `:root`. Em seguida, temos estilos gerais para o corpo do documento e uma classe `.container` para limitar a largura do conteúdo dentro de 1200 pixels.

A seção de cabeçalho (`header`) possui estilos de fundo, títulos e uma navegação horizontal utilizando a propriedade `display: flex`.

O conteúdo é estruturado em seções (`section`) que possuem títulos, parágrafos e imagens responsivas.

O rodapé (`footer`) possui um estilo de fundo secundário e um texto centralizado.

Além disso, o código contém algumas animações (`fade-in`, `slide-in`, `spin`) utilizando keyframes e classes customizadas.

Por fim, o código também inclui uma `@media query` para aplicar estilos específicos em dispositivos com no máximo 768 pixels de largura.

Esse código é apenas um exemplo, e pode ser adaptado e personalizado de acordo com as necessidades do projeto.