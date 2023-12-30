Claro! Aqui está um código em JavaScript que utiliza a biblioteca D3.js para criar um gráfico de barras interativo. O código é bastante extenso e utiliza conceitos avançados, como manipulação de dados e criação de elementos SVG.

```
<!DOCTYPE html>
<html lang="pt-BR">

<head>
  <meta charset="UTF-8">
  <title>Gráfico de Barras Interativo</title>
  <script src="https://d3js.org/d3.v6.js"></script>
  <style>
    .chart {
      width: 500px;
      height: 300px;
    }

    .bar {
      fill: steelblue;
    }

    .bar:hover {
      fill: orange;
    }

    .axis {
      font-size: 12px;
    }

    .axis path,
    .axis line {
      fill: none;
      stroke: black;
    }
  </style>
</head>

<body>
  <div id="chart" class="chart"></div>

  <script>
    // Dados do gráfico
    const data = [
      { label: 'Item 1', value: 10 },
      { label: 'Item 2', value: 20 },
      { label: 'Item 3', value: 15 },
      { label: 'Item 4', value: 25 },
      { label: 'Item 5', value: 12 }
    ];

    // Dimensões do gráfico
    const width = 500;
    const height = 300;
    const margin = { top: 20, right: 20, bottom: 30, left: 40 };
    const innerWidth = width - margin.left - margin.right;
    const innerHeight = height - margin.top - margin.bottom;

    // Escala y
    const y = d3.scaleLinear()
      .domain([0, d3.max(data, d => d.value)])
      .range([innerHeight, 0]);

    // Escala x
    const x = d3.scaleBand()
      .domain(data.map(d => d.label))
      .range([0, innerWidth])
      .padding(0.1);

    // Eixo y
    const yAxis = d3.axisLeft(y);

    // Eixo x
    const xAxis = d3.axisBottom(x);

    // SVG
    const svg = d3.select('#chart')
      .append('svg')
      .attr('width', width)
      .attr('height', height)
      .append('g')
      .attr('transform', `translate(${margin.left},${margin.top})`);

    // Barras
    svg.selectAll('rect')
      .data(data)
      .enter()
      .append('rect')
      .attr('class', 'bar')
      .attr('x', d => x(d.label))
      .attr('y', d => y(d.value))
      .attr('width', x.bandwidth())
      .attr('height', d => innerHeight - y(d.value));

    // Eixo y
    svg.append('g')
      .attr('class', 'axis')
      .call(yAxis);

    // Eixo x
    svg.append('g')
      .attr('class', 'axis')
      .attr('transform', `translate(0,${innerHeight})`)
      .call(xAxis);

  </script>
</body>

</html>
```

Explicação do código:

1. Começamos importando a biblioteca D3.js e definindo um estilo CSS para o nosso gráfico.

2. Em seguida, criamos uma estrutura básica HTML com uma div que vai conter o gráfico.

3. No JavaScript, definimos os dados do gráfico em um array de objetos, onde cada objeto representa um valor com uma etiqueta.

4. Em seguida, definimos as dimensões do gráfico, incluindo as margens.

5. Criamos duas escalas, uma para o eixo y (vertical) e outra para o eixo x (horizontal). A escala y é uma escala linear que mapeia os valores dos dados para uma faixa de altura. A escala x é uma escala de banda que mapeia as etiquetas dos dados para uma faixa de largura.

6. Criamos os eixos y e x usando as escalas definidas anteriormente.

7. Criamos um elemento SVG e o anexamos à div do gráfico. Em seguida, movemos o grupo (`<g>`) para as margens esquerda e superior.

8. Criamos as barras do gráfico usando a função `selectAll()` para selecionar todos os retângulos e vinculamos os dados a eles usando a função `data()`. Em seguida, usamos a função `enter()` para criar elementos retângulos para cada novo dado.

9. Definimos os atributos dos retângulos, como posição x e y, largura e altura, usando as escalas definidas anteriormente.

10. Adicionamos os eixos y e x ao gráfico, usando a função `append()` e a função `call()` para aplicar os eixos ao grupo SVG.

Esse código cria um gráfico de barras interativo com base nos dados fornecidos. Cada barra representa um item e seu valor correspondente. Ao passar o mouse sobre as barras, elas mudam de cor para laranja.