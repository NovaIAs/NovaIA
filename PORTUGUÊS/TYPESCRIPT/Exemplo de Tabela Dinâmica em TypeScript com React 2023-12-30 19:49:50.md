Claro! Aqui está um exemplo de um código complexo em TypeScript, que utiliza a biblioteca React para criar um componente de tabela dinâmica:

```typescript
import React, { useState } from 'react';

interface Dados {
  nome: string;
  idade: number;
  cidade: string;
}

const TabelaDinamica: React.FC = () => {
  const [dados, setDados] = useState<Dados[]>([]);

  const adicionarDados = () => {
    const nome = prompt('Digite o nome:');
    const idade = parseInt(prompt('Digite a idade:'), 10);
    const cidade = prompt('Digite a cidade:');

    if (nome && idade && cidade) {
      const novoDado = { nome, idade, cidade };
      setDados([...dados, novoDado]);
    }
  };

  return (
    <div>
      <button onClick={adicionarDados}>Adicionar Dados</button>
      <table>
        <thead>
          <tr>
            <th>Nome</th>
            <th>Idade</th>
            <th>Cidade</th>
          </tr>
        </thead>
        <tbody>
          {dados.map((dado, index) => (
            <tr key={index}>
              <td>{dado.nome}</td>
              <td>{dado.idade}</td>
              <td>{dado.cidade}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

export default TabelaDinamica;
```

Neste código, utilizamos a sintaxe do TypeScript para definir a interface `Dados`, que representa a estrutura dos dados que serão exibidos na tabela. Em seguida, criamos um componente funcional chamado `TabelaDinamica`, que utiliza o hook `useState` para armazenar os dados inseridos pelo usuário.

O método `adicionarDados` é responsável por capturar os valores digitados pelo usuário através de prompts e adicioná-los ao estado `dados`. Em seguida, a tabela é renderizada utilizando a função `map` para percorrer o array `dados` e exibir os valores correspondentes em cada célula.

Por fim, exportamos o componente `TabelaDinamica` como padrão para que possa ser utilizado em outros lugares do projeto.

Espero que este exemplo atenda às suas expectativas!