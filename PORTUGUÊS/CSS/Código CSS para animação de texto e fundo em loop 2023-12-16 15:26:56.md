Claro! Aqui está um código complexo em CSS que irá criar um efeito de animação de texto em loop:

```css
<!DOCTYPE html>
<html>
<head>
<style>
@keyframes animacaoTexto {
  0% { color: red; }
  25% { color: blue; }
  50% { color: green; }
  75% { color: yellow; }
  100% { color: purple; }
}

@keyframes animacaoFundo {
  0% { background-color: white; }
  25% { background-color: red; }
  50% { background-color: blue; }
  75% { background-color: green; }
  100% { background-color: yellow; }
}

.textoAnimado {
  font-size: 30px;
  font-weight: bold;
  animation: animacaoTexto 5s infinite;
}

.fundoAnimado {
  width: 100%;
  height: 100vh;
  position: fixed;
  animation: animacaoFundo 5s infinite;
}
</style>
</head>
<body>
  <div class="fundoAnimado"></div>
  <h1 class="textoAnimado">Olá, mundo!</h1>
</body>
</html>
```

Neste código CSS, definimos duas animações usando a regra `@keyframes`. A primeira animação chamada `animacaoTexto` altera a cor do texto de vermelho para roxo em um loop. A segunda animação chamada `animacaoFundo` altera a cor de fundo da página de branco para amarelo em um loop.

Em seguida, criamos duas classes: `.textoAnimado` e `.fundoAnimado`. A classe `.textoAnimado` aplica a animação `animacaoTexto` ao texto "Olá, mundo!", enquanto a classe `.fundoAnimado` aplica a animação `animacaoFundo` a um elemento `<div>` que ocupa a tela inteira.

Desta forma, o texto e o fundo da página serão animados continuamente, criando um efeito visual interessante. Você pode ajustar a duração das animações alterando o valor `5s` para um valor desejado.