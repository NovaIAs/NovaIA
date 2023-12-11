Claro! Aqui está um código complexo em JavaScript que utiliza a biblioteca D3.js para criar um gráfico de barras interativo. Ele exibe dados de vendas mensais de uma loja fictícia:

```javascript
// Definindo os dados de vendas mensais
const vendasMensais = [
  { mes: 'Janeiro', valor: 1500 },
  { mes: 'Fevereiro', valor: 2200 },
  { mes: 'Março', valor: 1800 },
  { mes: 'Abril', valor: 2400 },
  { mes: 'Maio', valor: 2800 },
  { mes: 'Junho', valor: 3200 }
];

// Definindo as dimensões do gráfico
const largura = 600;
const altura = 400;
const margem = { topo: 20, direita: 20, inferior: 30, esquerda: 40 };
const larguraGrafico = largura - margem.esquerda - margem.direita;
const alturaGrafico = altura - margem.topo - margem.inferior;

// Criando o elemento SVG para o gráfico de barras
const svg = d3.select('body')
  .append('svg')
  .attr('width', largura)
  .attr('height', altura);

// Criando o grupo principal para o gráfico
const grupoGrafico = svg.append('g')
  .attr('transform', `translate(${margem.esquerda}, ${margem.topo})`);

// Escalas de x e y
const escalaX = d3.scaleBand()
  .range([0, larguraGrafico])
  .padding(0.1)
  .domain(vendasMensais.map(d => d.mes));

const escalaY = d3.scaleLinear()
  .range([alturaGrafico, 0])
  .domain([0, d3.max(vendasMensais, d => d.valor)]);

// Criando as barras do gráfico
grupoGrafico.selectAll('rect')
  .data(vendasMensais)
  .enter()
  .append('rect')
  .attr('x', d => escalaX(d.mes))
  .attr('y', d => escalaY(d.valor))
  .attr('width', escalaX.bandwidth())
  .attr('height', d => alturaGrafico - escalaY(d.valor))
  .attr('fill', 'steelblue')
  .on('mouseover', function() {
    d3.select(this)
      .attr('fill', 'orange');
  })
  .on('mouseout', function() {
    d3.select(this)
      .attr('fill', 'steelblue');
  });

// Adicionando os eixos x e y
const eixoX = d3.axisBottom(escalaX);
const eixoY = d3.axisLeft(escalaY);

grupoGrafico.append('g')
  .attr('transform', `translate(0, ${alturaGrafico})`)
  .call(eixoX);

grupoGrafico.append('g')
  .call(eixoY);

// Adicionando rótulos para cada barra
grupoGrafico.selectAll('text')
  .data(vendasMensais)
  .enter()
  .append('text')
  .text(d => d.valor)
  .attr('x', d => escalaX(d.mes) + escalaX.bandwidth() / 2)
  .attr('y', d => escalaY(d.valor) - 5)
  .attr('text-anchor', 'middle')
  .attr('fill', 'white');
```

Neste código, estamos utilizando a biblioteca D3.js para criar um gráfico de barras interativo. Primeiro, definimos os dados de vendas mensais em um array de objetos. Em seguida, estabelecemos as dimensões do gráfico e criamos o elemento SVG para conter o gráfico.

Utilizamos as escalas `escalaX` e `escalaY` para mapear os valores de vendas e os meses para as posições x e y do gráfico. Em seguida, criamos as barras do gráfico utilizando o método `selectAll` para associar cada retângulo aos dados de vendas mensais.

Além disso, adicionamos interações de mouse para destacar as barras quando o cursor passa sobre elas. Utilizamos os eixos `eixoX` e `eixoY` para adicionar os rótulos dos meses e os valores de vendas nos eixos x e y.

Por fim, adicionamos rótulos para cada barra e os posicionamos no centro de cada barra usando a escala `escalaX`. Esses rótulos exibem os valores de vendas correspondentes.

Espero que este exemplo atenda às suas expectativas!