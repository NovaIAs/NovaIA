Claro! Aqui está um exemplo de código em CSS que cria um efeito de animação personalizado para um botão:

```css
/* Definindo as cores principais */
:root {
  --cor-primaria: #FF4500;
  --cor-secundaria: #FFFFFF;
}

/* Estilizando o botão */
.button {
  display: inline-block;
  padding: 10px 20px;
  font-size: 16px;
  border: none;
  border-radius: 5px;
  background-color: var(--cor-primaria);
  color: var(--cor-secundaria);
  cursor: pointer;
  transition: background-color 0.3s ease;
}

/* Adicionando o efeito de animação */
.button:hover {
  animation: pulsar 1s infinite;
}

/* Definindo a animação */
@keyframes pulsar {
  0% {
    transform: scale(1);
    box-shadow: 0 0 0 0 rgba(255, 69, 0, 0.4);
  }
  50% {
    transform: scale(1.05);
    box-shadow: 0 0 0 10px rgba(255, 69, 0, 0);
  }
  100% {
    transform: scale(1);
    box-shadow: 0 0 0 0 rgba(255, 69, 0, 0.4);
  }
}
```

Neste código, primeiro definimos duas variáveis CSS (--cor-primaria e --cor-secundaria) para armazenar as cores principais do botão. Em seguida, estilizamos o botão usando a classe `.button`, definindo o tamanho, fonte, cores, bordas e outros estilos.

O efeito de animação é adicionado quando o botão é hover (passa o mouse por cima). Utilizamos a propriedade `animation` para aplicar a animação chamada `pulsar` por 1 segundo de forma infinita.

A animação `pulsar` é definida utilizando a regra `@keyframes`. Nela, especificamos três estágios de animação: 0%, 50% e 100%. No estágio inicial (0%), o botão fica com a escala normal e uma sombra. No estágio intermediário (50%), a escala aumenta ligeiramente e a sombra desaparece. No estágio final (100%), o botão retorna à escala normal e a sombra reaparece. Esses estágios são animados suavemente através da propriedade `transform` (para alterar a escala) e `box-shadow` (para adicionar a sombra).

Assim, quando o botão é hover, ele terá um efeito de pulsação suave e atraente. Você pode personalizar as cores e ajustar a duração e intensidade da animação de acordo com suas preferências.