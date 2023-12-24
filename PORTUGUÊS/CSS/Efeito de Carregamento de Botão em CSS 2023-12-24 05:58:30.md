Claro! Aqui está um código complexo em CSS que utiliza várias propriedades e seletores para criar um efeito de animação de um botão de carregamento:

```css
/* Definindo as cores */
:root {
  --cor-de-fundo: #f2f2f2;
  --cor-principal: #3498db;
  --cor-secundaria: #e74c3c;
}

/* Estilizando o botão */
.botao-carregamento {
  position: relative;
  display: inline-block;
  width: 150px;
  height: 50px;
  border: none;
  background-color: var(--cor-principal);
  color: #fff;
  font-size: 18px;
  font-weight: bold;
  text-align: center;
  text-transform: uppercase;
  cursor: pointer;
  overflow: hidden;
  z-index: 1;
}

/* Criando o efeito de carregamento */
.botao-carregamento:before {
  content: "";
  position: absolute;
  top: 0;
  left: -150px;
  width: 150px;
  height: 100%;
  background-color: var(--cor-secundaria);
  animation: carregamento 1.5s infinite;
}

/* Definindo a animação de carregamento */
@keyframes carregamento {
  0% {
    left: -150px;
  }
  100% {
    left: 100%;
  }
}

/* Estilizando o texto do botão */
.botao-carregamento span {
  position: relative;
  z-index: 2;
}

/* Estilizando o fundo do botão */
.botao-carregamento:after {
  content: "";
  position: absolute;
  top: 0;
  left: 0;
  width: 150px;
  height: 100%;
  background-color: var(--cor-de-fundo);
  z-index: 0;
}
```

Explicação do código:

1. Começamos definindo três variáveis CSS (`--cor-de-fundo`, `--cor-principal` e `--cor-secundaria`) para armazenar as cores que serão utilizadas no botão de carregamento.

2. Em seguida, estilizamos o botão de carregamento utilizando a classe `.botao-carregamento`. Definimos propriedades como posição, tamanho, cor de fundo, cor do texto, entre outros.

3. Utilizamos a pseudo-classe `:before` para criar um elemento antes do conteúdo do botão, que será responsável pelo efeito de carregamento. Posicionamos esse elemento fora do botão, à esquerda, com a mesma largura e altura do botão.

4. Definimos a animação `carregamento` utilizando `@keyframes`. Essa animação faz o elemento de carregamento se movimentar da esquerda para a direita, saindo completamente da tela.

5. Estilizamos o texto do botão utilizando a classe `.botao-carregamento span`. Essa classe tem um `z-index` maior do que o elemento de carregamento, para que o texto fique acima dele.

6. Por fim, utilizamos a pseudo-classe `:after` para criar um elemento após o conteúdo do botão, que será responsável pelo fundo do botão. Esse elemento possui a mesma largura e altura do botão, e possui uma cor de fundo definida pela variável `--cor-de-fundo`. O `z-index` desse elemento é menor do que o do botão e do elemento de carregamento, para que fique atrás deles.

Espero que este código atenda às suas expectativas!