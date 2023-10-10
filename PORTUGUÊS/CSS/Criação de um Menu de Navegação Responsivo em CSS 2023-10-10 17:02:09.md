Claro! Abaixo está um exemplo complexo de código em CSS que cria um menu de navegação responsivo:

```CSS
/* Estilos para dispositivos móveis */
@media only screen and (max-width: 768px) {
  /* Estilo para o ícone de menu */
  .menu-icon {
    display: block;
    position: fixed;
    top: 10px;
    right: 10px;
    cursor: pointer;
    font-size: 24px;
    color: #ffffff;
    z-index: 9999;
  }

  /* Estilo para o menu mobile */
  .menu {
    position: fixed;
    top: 0;
    right: 0;
    width: 100%;
    height: 100%;
    background-color: #333333;
    z-index: 9998;
    transform: translateX(100%);
    transition: transform 0.3s ease-in-out;
  }

  /* Estilo para o item do menu */
  .menu-item {
    display: block;
    padding: 20px;
    color: #ffffff;
    font-size: 18px;
    text-align: center;
    border-bottom: 1px solid #555555;
  }

  /* Estilo para o menu mobile quando aberto */
  .menu.open {
    transform: translateX(0);
  }
}

/* Estilos para dispositivos maiores que 768px */
@media only screen and (min-width: 769px) {
  /* Estilo para o menu desktop */
  .menu {
    display: flex;
    justify-content: space-between;
    align-items: center;
    background-color: #333333;
    padding: 10px;
  }

  /* Estilo para o item do menu */
  .menu-item {
    display: inline-block;
    padding: 10px;
    color: #ffffff;
    font-size: 18px;
    text-align: center;
    border-right: 1px solid #555555;
  }

  /* Estilizar o último item do menu */
  .menu-item:last-child {
    border-right: none;
  }
}
```

Este código cria um menu de navegação responsivo que se adapta a diferentes tamanhos de tela. No CSS, utilizamos media queries para aplicar estilos específicos para dispositivos móveis (largura máxima de 768px) e dispositivos maiores que 768px.

No estilo para dispositivos móveis, definimos uma classe `.menu-icon` para o ícone de menu, que é exibido apenas em telas menores. A classe `.menu` é utilizada para o menu mobile, onde definimos sua posição, largura, altura, cor de fundo e um efeito de transição suave. A classe `.menu-item` é utilizada para estilizar cada item do menu, definindo o espaço interno, cor do texto e uma borda inferior.

Quando o menu mobile é aberto, adicionamos a classe `.open` ao elemento `.menu`, que altera a propriedade `transform` para movê-lo para a posição visível.

Para dispositivos maiores, definimos a classe `.menu` como um elemento flex que possui um fundo, espaçamento interno e alinhamento dos itens. A classe `.menu-item` é estilizada de forma semelhante ao menu mobile, mas com uma borda direita para separar os itens.

Esses estilos garantem que o menu de navegação se adapte e seja visualmente agradável em diferentes dispositivos.