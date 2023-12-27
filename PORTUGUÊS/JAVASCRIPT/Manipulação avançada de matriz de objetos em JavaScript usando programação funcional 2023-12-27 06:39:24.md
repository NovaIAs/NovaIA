Claro! Aqui está um código complexo em JavaScript que usa uma abordagem de programação funcional para manipular uma matriz de objetos:

```javascript
// Definindo a matriz de objetos
const usuarios = [
  { nome: 'João', idade: 25, cidade: 'São Paulo' },
  { nome: 'Maria', idade: 30, cidade: 'Rio de Janeiro' },
  { nome: 'Pedro', idade: 20, cidade: 'Belo Horizonte' },
  { nome: 'Ana', idade: 35, cidade: 'São Paulo' }
];

// Função para filtrar usuários com idade superior a 25 anos
const filtrarPorIdade = (usuarios) => {
  return usuarios.filter(usuario => usuario.idade > 25);
};

// Função para mapear nomes de usuários para letras maiúsculas
const converterParaMaiusculas = (usuarios) => {
  return usuarios.map(usuario => {
    return { ...usuario, nome: usuario.nome.toUpperCase() };
  });
};

// Função para calcular a média de idade dos usuários
const calcularMediaIdade = (usuarios) => {
  const somaIdades = usuarios.reduce((soma, usuario) => soma + usuario.idade, 0);
  return somaIdades / usuarios.length;
};

// Função para agrupar usuários por cidade
const agruparPorCidade = (usuarios) => {
  return usuarios.reduce((agrupamento, usuario) => {
    const cidade = usuario.cidade;
    if (!agrupamento[cidade]) {
      agrupamento[cidade] = [];
    }
    agrupamento[cidade].push(usuario);
    return agrupamento;
  }, {});
};

// Chamando as funções em sequência
const usuariosFiltrados = filtrarPorIdade(usuarios);
const usuariosMaiusculas = converterParaMaiusculas(usuariosFiltrados);
const mediaIdade = calcularMediaIdade(usuariosMaiusculas);
const usuariosAgrupados = agruparPorCidade(usuariosMaiusculas);

console.log('Usuários filtrados:', usuariosFiltrados);
console.log('Usuários em maiúsculas:', usuariosMaiusculas);
console.log('Média de idade:', mediaIdade);
console.log('Usuários agrupados por cidade:', usuariosAgrupados);
```

Neste código, temos uma matriz de objetos chamada `usuarios`. Em seguida, temos uma série de funções que são aplicadas a essa matriz para filtrar, modificar e analisar os dados.

A função `filtrarPorIdade` usa o método `filter` para retornar apenas os usuários com idade superior a 25 anos.

A função `converterParaMaiusculas` usa o método `map` para criar uma nova matriz de usuários, onde o nome de cada usuário é convertido para letras maiúsculas.

A função `calcularMediaIdade` usa o método `reduce` para somar as idades de todos os usuários e, em seguida, divide essa soma pelo número total de usuários para obter a média de idade.

A função `agruparPorCidade` usa o método `reduce` para criar um objeto onde as chaves são as cidades dos usuários e os valores são matrizes contendo todos os usuários dessa cidade.

Por fim, as funções são chamadas em sequência e os resultados são exibidos no console.

Espero que esse código complexo em JavaScript atenda às suas expectativas!