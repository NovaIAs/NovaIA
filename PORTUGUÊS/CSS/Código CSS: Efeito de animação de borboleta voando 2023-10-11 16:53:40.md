Claro! Aqui está um código CSS complexo que cria um efeito de animação de borboleta voando:

```css
/* Definindo as cores utilizadas */
:root {
  --cor-primaria: #00A3E0;
  --cor-secundaria: #FF8C00;
  --cor-fundo: #F5F5F5;
}

/* Estilo do corpo do documento */
body {
  background-color: var(--cor-fundo);
  margin: 0;
  padding: 0;
  display: flex;
  justify-content: center;
  align-items: center;
  height: 100vh;
}

/* Estilo do contêiner da borboleta */
.borboleta {
  position: relative;
  width: 300px;
  height: 200px;
}

/* Estilo das asas da borboleta */
.asa {
  position: absolute;
  width: 150px;
  height: 200px;
  background-color: var(--cor-primaria);
  border-radius: 50%;
  transform-origin: 75px 100px;
  animation-name: voar;
  animation-duration: 5s;
  animation-iteration-count: infinite;
  animation-timing-function: linear;
}

/* Estilo da primeira asa */
.asa-1 {
  top: 0;
  left: 0;
  transform: rotate(-30deg);
}

/* Estilo da segunda asa */
.asa-2 {
  top: 0;
  right: 0;
  transform: rotate(30deg);
}

/* Estilo do corpo da borboleta */
.corpo {
  position: absolute;
  top: 80px;
  left: 75px;
  width: 150px;
  height: 40px;
  background-color: var(--cor-secundaria);
  border-radius: 50%;
}

/* Estilo das antenas da borboleta */
.antena {
  position: absolute;
  width: 10px;
  height: 100px;
  background-color: var(--cor-secundaria);
  border-radius: 5px;
}

/* Estilo da primeira antena */
.antena-1 {
  top: 40px;
  left: 35px;
  transform: rotate(-45deg);
}

/* Estilo da segunda antena */
.antena-2 {
  top: 40px;
  right: 35px;
  transform: rotate(45deg);
}

/* Animação de voar */
@keyframes voar {
  0% {
    transform: translateY(-40px) rotate(0deg);
  }
  50% {
    transform: translateY(40px) rotate(360deg);
  }
  100% {
    transform: translateY(-40px) rotate(720deg);
  }
}

```

Neste código CSS, criamos um efeito de animação de uma borboleta voando. A borboleta é composta por duas asas, um corpo e duas antenas. Utilizamos variáveis para definir as cores primária, secundária e de fundo, facilitando a personalização do estilo.

Definimos o estilo do corpo do documento para que se estenda por toda a altura e largura da janela. Em seguida, criamos um contêiner para a borboleta e posicionamos suas asas, corpo e antenas através da propriedade `position: absolute`.

As asas da borboleta são elementos circulares com o uso de `border-radius` e possuem um efeito de rotação utilizando a animação `voar`. Cada asa é posicionada nas coordenadas desejadas utilizando as classes `.asa-1` e `.asa-2`.

O corpo da borboleta é um elemento retangular posicionado no centro do contêiner utilizando `position: absolute`. As antenas são elementos verticais posicionados nas laterais do corpo, com uma leve inclinação utilizando `transform: rotate()`.

Por fim, é adicionada a animação `voar` utilizando `@keyframes`, que define os estados da animação nos percentuais de 0%, 50% e 100%. A borboleta voará para cima, para baixo e fará uma rotação completa durante o período de 5 segundos, repetindo infinitamente.

Espero que esse código atenda às suas expectativas! Se precisar de mais ajuda, estou à disposição.