Claro! Aqui está um código complexo em CSS para criar um menu de navegação responsivo e animado:

```css
/* Estilo base do menu */
.menu {
  display: flex;
  justify-content: space-between;
  align-items: center;
  background-color: #333;
  padding: 20px;
}

/* Estilo do logo */
.logo {
  color: #fff;
  font-size: 24px;
  font-weight: bold;
}

/* Estilo dos itens do menu */
.menu-item {
  list-style: none;
  display: inline-block;
  margin-left: 20px;
}

.menu-item a {
  color: #fff;
  text-decoration: none;
}

/* Estilo do botão de menu para dispositivos móveis */
.menu-icon {
  display: none;
  color: #fff;
  font-size: 24px;
  cursor: pointer;
}

/* Estilo do menu dropdown */
.dropdown {
  display: none;
  position: absolute;
  background-color: #333;
  padding: 20px;
  top: 100%;
  left: 0;
  width: 100%;
  z-index: 1;
}

.dropdown-item {
  margin-bottom: 10px;
}

/* Animação do menu dropdown */
@keyframes slideDown {
  0% {
    transform: translateY(-100%);
  }
  100% {
    transform: translateY(0);
  }
}

/* Estilo do menu dropdown visível */
.dropdown.show {
  display: block;
  animation: slideDown 0.3s;
}

/* Estilo do botão de fechar o menu dropdown */
.close-icon {
  position: absolute;
  top: 10px;
  right: 10px;
  color: #fff;
  font-size: 24px;
  cursor: pointer;
}

/* Estilos de mídia para dispositivos móveis */
@media (max-width: 768px) {
  .menu-item {
    display: none;
  }

  .menu-icon {
    display: block;
  }

  .dropdown {
    padding: 10px;
  }
}
```

Explicação do código:

1. Começamos definindo o estilo base do menu, que utiliza flexbox para alinhar os itens horizontalmente. O fundo é definido como cinza escuro (#333) e possui um espaçamento interno de 20 pixels.

2. Em seguida, estilizamos o logo do menu, definindo a cor como branco (#fff), o tamanho da fonte como 24 pixels e o peso da fonte como negrito.

3. Definimos o estilo dos itens do menu, removendo a lista padrão e exibindo os itens em linha (inline-block). Cada item possui uma margem esquerda de 20 pixels.

4. Estilizamos os links dos itens do menu, definindo a cor como branco (#fff) e removendo a decoração de texto.

5. Em seguida, definimos o estilo do botão de menu para dispositivos móveis, que inicialmente está oculto (display: none). O ícone é exibido como um texto com o tamanho de fonte de 24 pixels, na cor branca (#fff) e com um cursor do tipo ponteiro.

6. Definimos o estilo do menu dropdown, que também está oculto (display: none) e possui uma posição absoluta. O fundo é definido como cinza escuro (#333) e possui um espaçamento interno de 20 pixels. A posição é ajustada para ficar abaixo do menu principal (top: 100%) e ocupar toda a largura (width: 100%). A propriedade z-index é usada para garantir que o dropdown fique acima do resto do conteúdo.

7. Estilizamos cada item do menu dropdown, adicionando uma margem inferior de 10 pixels.

8. Em seguida, definimos uma animação chamada "slideDown" que desce o menu dropdown a partir de uma posição inicial acima do menu principal. A animação dura 0,3 segundos.

9. Definimos o estilo do menu dropdown quando estiver visível (class "show"), exibindo-o (display: block) e aplicando a animação "slideDown" durante 0,3 segundos.

10. Estilizamos o ícone de fechar o menu dropdown, que possui uma posição absoluta, no canto superior direito do dropdown. O ícone é exibido como um texto com o tamanho de fonte de 24 pixels, na cor branca (#fff) e com um cursor do tipo ponteiro.

11. Por fim, definimos estilos de mídia para dispositivos móveis, onde ocultamos os itens do menu principal (display: none) e exibimos o botão de menu (display: block). O dropdown tem seu espaçamento interno reduzido para 10 pixels. Esses estilos são ativados quando a largura da tela é igual ou menor que 768 pixels.

Esse código cria um menu de navegação responsivo com um dropdown animado para dispositivos móveis.