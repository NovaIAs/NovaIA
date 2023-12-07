Claro, aqui está um exemplo de código complexo em CSS:

```css
/* Definindo um seletor com pseudo-classe */
button:hover {
  background-color: #ff0000;
  color: #ffffff;
  border: 2px solid #ff0000;
}

/* Definindo um seletor de classe */
.image-gallery {
  display: flex;
  justify-content: space-between;
  align-items: center;
  flex-wrap: wrap;
  margin-top: 20px;
}

/* Definindo um seletor de ID */
#header {
  background-color: #000000;
  color: #ffffff;
  height: 80px;
  padding: 20px;
  font-size: 24px;
}

/* Definindo um seletor de atributo */
a[target="_blank"] {
  color: #0000ff;
  font-weight: bold;
}

/* Definindo um seletor de descendência */
.container ul li {
  margin-bottom: 10px;
}

/* Definindo uma animação */
@keyframes slide-in {
  0% {
    transform: translateX(-100%);
  }
  100% {
    transform: translateX(0);
  }
}

/* Utilizando a animação */
.slide-image {
  animation: slide-in 1s ease-in-out;
}

/* Utilizando variáveis */
:root {
  --primary-color: #ff00ff;
  --secondary-color: #00ff00;
}

.card {
  background-color: var(--primary-color);
  color: var(--secondary-color);
  padding: 10px;
}
```

Neste exemplo, você encontra uma série de técnicas e propriedades do CSS para criar um código complexo em nível de seletores, animações, variáveis e muito mais.

Aqui está uma breve explicação do código:

- Definimos um seletor com pseudo-classe para alterar a aparência de um botão quando ele está sendo hover (passando o mouse sobre ele).
- Aplicamos um conjunto de estilos a um elemento com uma classe chamada "image-gallery" para criar uma galeria de imagens com determinado layout.
- Estilizamos um elemento com um ID chamado "header", definindo suas cores, altura e espaçamento interno.
- Utilizamos um seletor de atributo para aplicar estilos a links que tenham o atributo "target" definido como "_blank" (abrir em nova aba).
- Definimos estilos para elementos filhos de uma classe chamada "container" que estão aninhados dentro de uma estrutura de elementos ul>li.
- Criamos uma animação chamada "slide-in" que desloca um elemento de -100% para 0 no eixo X com duração de 1 segundo.
- Aplicamos a animação a uma classe chamada "slide-image".
- Utilizamos variáveis CSS para definir cores personalizadas e, em seguida, aplicamos essas variáveis em um elemento com uma classe chamada "card".

Espero que este exemplo complexo de código em CSS tenha sido útil.