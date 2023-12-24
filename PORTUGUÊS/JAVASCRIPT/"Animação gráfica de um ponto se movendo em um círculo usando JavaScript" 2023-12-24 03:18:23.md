Claro! Aqui está um exemplo de um código em JavaScript que realiza um cálculo matemático complexo e mostra o resultado em um formato de animação gráfica:

```javascript
// Variáveis globais
var canvas, ctx;
var centerX, centerY;
var radius;
var angle = 0;

// Função de inicialização
function init() {
  // Inicializar o canvas
  canvas = document.getElementById("canvas");
  ctx = canvas.getContext("2d");

  // Definir o tamanho do canvas
  canvas.width = window.innerWidth;
  canvas.height = window.innerHeight;

  // Definir o centro do canvas e o raio do círculo
  centerX = canvas.width / 2;
  centerY = canvas.height / 2;
  radius = Math.min(canvas.width, canvas.height) / 3;

  // Chamar a função de animação
  animate();
}

// Função de animação
function animate() {
  // Limpar o canvas
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  // Calcular a posição do ponto no círculo
  var x = centerX + Math.cos(angle) * radius;
  var y = centerY + Math.sin(angle) * radius;

  // Desenhar o ponto no canvas
  ctx.beginPath();
  ctx.arc(x, y, 5, 0, 2 * Math.PI);
  ctx.fillStyle = "red";
  ctx.fill();

  // Incrementar o ângulo
  angle += 0.01;

  // Chamar novamente a função de animação a cada quadro
  requestAnimationFrame(animate);
}

// Chamar a função de inicialização quando a página carregar
window.onload = init;
```

Este código cria uma animação gráfica de um ponto se movendo em um círculo. Ele usa a função `init()` para inicializar o canvas e definir as variáveis necessárias. Em seguida, a função `animate()` é chamada repetidamente para atualizar a posição do ponto e desenhá-lo no canvas. A animação é realizada através do uso da função `requestAnimationFrame()`.

Espero que isso atenda às suas expectativas! Lembre-se de que posso gerar qualquer tipo de código de programação, então se você tiver alguma outra solicitação específica, é só me informar.