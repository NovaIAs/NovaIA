Claro! Aqui está um código complexo em CSS que utiliza algumas propriedades avançadas e técnicas de animação:

```css
/* Estilos gerais */
body {
  font-family: Arial, sans-serif;
  background-color: #f2f2f2;
  margin: 0;
  padding: 0;
}

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}

h1 {
  color: #333;
  font-size: 36px;
  text-align: center;
  margin-bottom: 30px;
}

/* Estilos do menu */
.menu {
  display: flex;
  justify-content: space-between;
  background-color: #333;
  padding: 10px;
}

.menu-item {
  list-style-type: none;
}

.menu-link {
  text-decoration: none;
  color: #fff;
  font-weight: bold;
  padding: 10px;
  transition: color 0.3s;
}

.menu-link:hover {
  color: #ff7f50;
}

/* Estilos do slider */
.slider {
  position: relative;
  overflow: hidden;
  height: 500px;
}

.slider-item {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  opacity: 0;
  transition: opacity 1s;
}

.slider-item.active {
  opacity: 1;
}

.slider-image {
  width: 100%;
  height: 100%;
  object-fit: cover;
}

.slider-caption {
  position: absolute;
  bottom: 20px;
  left: 50%;
  transform: translateX(-50%);
  background-color: rgba(0, 0, 0, 0.7);
  color: #fff;
  padding: 10px;
  font-size: 24px;
}

.slider-caption::before {
  content: '';
  position: absolute;
  top: -10px;
  left: 50%;
  transform: translateX(-50%);
  border-width: 10px;
  border-style: solid;
  border-color: transparent transparent rgba(0, 0, 0, 0.7) transparent;
}

/* Estilos do formulário de contato */
.contact-form {
  display: flex;
  flex-wrap: wrap;
}

.contact-form input,
.contact-form textarea {
  width: 100%;
  padding: 10px;
  margin-bottom: 10px;
  border-radius: 5px;
  border: 1px solid #ccc;
}

.contact-form textarea {
  height: 150px;
}

.contact-form button {
  background-color: #333;
  color: #fff;
  border: none;
  padding: 10px 20px;
  border-radius: 5px;
  cursor: pointer;
  transition: background-color 0.3s;
}

.contact-form button:hover {
  background-color: #ff7f50;
}
```

Explicação do código:

1. Definimos estilos gerais para o documento, como a fonte, cor de fundo e margens.
2. Criamos uma classe `.container` para centralizar o conteúdo e definir uma largura máxima.
3. Estilizamos um título (`h1`) centralizado com cor, tamanho e margem.
4. Criamos o estilo do menu, utilizando flexbox para alinhar os itens à esquerda e à direita.
5. Estilizamos os links do menu, definindo cores, negrito e adicionando uma transição suave ao passar o mouse.
6. Em seguida, definimos o estilo do slider, que é uma área com uma imagem de fundo e uma legenda.
7. Utilizamos `position: relative` para criar um contexto de posicionamento para os slides.
8. Estilizamos cada item do slider com `position: absolute`, tornando-os sobrepostos e ajustando a opacidade para criar o efeito de transição suave.
9. A classe `.active` é usada para definir o slide visível, alterando sua opacidade para 1.
10. Estilizamos a imagem do slide para preencher todo o espaço disponível com `object-fit: cover`.
11. A legenda do slide é posicionada na parte inferior centralizada e estilizada com um fundo semitransparente.
12. Utilizamos `::before` para adicionar uma seta triangular acima da legenda.
13. Em seguida, definimos o estilo do formulário de contato, utilizando flexbox para alinhar os campos.
14. Estilizamos os campos de entrada de texto com margens, bordas arredondadas e preenchimento.
15. O botão de envio do formulário é estilizado com cores de fundo e texto, além de uma transição suave ao passar o mouse.

Espero que este código seja útil e atenda às suas expectativas!