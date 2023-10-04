Claro! Aqui está um código complexo em CSS que cria um efeito de animação de nuvens em um fundo gradiente. 

```css
/* Criando um fundo gradiente */

body {
  background: linear-gradient(to bottom, #b2d4ff, #c3e6f9);
}

/* Definindo a animação das nuvens */

@keyframes moveClouds {
  0% {
    transform: translateX(-100%);
  }
  100% {
    transform: translateX(100%);
  }
}

/* Estilizando as nuvens */

.cloud {
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  width: 100px;
  height: 60px;
  background-color: #fff;
  border-radius: 50px;
  box-shadow: 0 0 20px rgba(0, 0, 0, 0.1);
  animation: moveClouds 10s linear infinite;
}

.cloud:before,
.cloud:after {
  content: "";
  position: absolute;
  background-color: inherit;
  border-radius: 50px;
}

.cloud:before {
  width: 80px;
  height: 40px;
  top: 10px;
  left: 10px;
}

.cloud:after {
  width: 50px;
  height: 30px;
  top: 20px;
  right: 10px;
}

/* Adicionando as nuvens ao documento HTML */

<div class="cloud"></div>
<div class="cloud"></div>
<div class="cloud"></div>
<div class="cloud"></div>
```

Este código cria um fundo gradiente azul claro e, em seguida, define uma animação de nuvens deslizantes. 

Primeiro, o fundo gradiente é definido usando a propriedade `background` com um gradiente linear de cima para baixo, começando com uma cor azul claro (#b2d4ff) e terminando com uma cor mais clara (#c3e6f9).

Em seguida, é definida a animação das nuvens usando a regra `@keyframes`. A animação `moveClouds` define dois pontos-chave: 0% e 100%. No ponto inicial (0%), as nuvens estão completamente fora da tela, deslocadas para a esquerda em 100% da largura do elemento. No ponto final (100%), as nuvens estão completamente fora da tela, deslocadas para a direita em 100% da largura do elemento. A animação é definida para durar 10 segundos, ser linear e repetir infinitamente.

Depois, o estilo das nuvens é definido na classe `.cloud`. As nuvens são posicionadas no centro da tela usando `position: absolute` e `top: 50%` e `left: 50%`. Em seguida, são deslocadas para a esquerda em 50% da largura do elemento e para cima em 50% da altura do elemento usando `transform: translate(-50%, -50%)`. As nuvens têm uma largura de 100px, altura de 60px, cor de fundo branca (#fff), borda arredondada (`border-radius: 50px`) e uma sombra (`box-shadow: 0 0 20px rgba(0, 0, 0, 0.1)`). A animação `moveClouds` é aplicada às nuvens com `animation: moveClouds 10s linear infinite`.

Para criar o formato das nuvens, são usados os pseudo-elementos `:before` e `:after`. Ambos têm uma cor de fundo igual ao elemento pai (`.cloud`) e borda arredondada (`border-radius: 50px`). O pseudo-elemento `:before` tem uma largura de 80px, altura de 40px, posicionado a 10px da borda superior e a 10px da borda esquerda. O pseudo-elemento `:after` tem uma largura de 50px, altura de 30px, posicionado a 20px da borda superior e a 10px da borda direita.

Por fim, quatro divs com a classe `.cloud` são adicionadas ao documento HTML para exibir as nuvens.