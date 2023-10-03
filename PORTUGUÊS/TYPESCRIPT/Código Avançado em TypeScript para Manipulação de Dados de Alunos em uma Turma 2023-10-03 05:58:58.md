Claro! Aqui está um código complexo em TypeScript que envolve a manipulação de arrays e objetos:

```typescript
interface Aluno {
  nome: string;
  idade: number;
  notas: number[];
}

interface Turma {
  nome: string;
  alunos: Aluno[];
}

function calcularMediaTurma(turma: Turma): number {
  let somaNotas = 0;
  let totalAlunos = 0;

  for (const aluno of turma.alunos) {
    totalAlunos++;
    for (const nota of aluno.notas) {
      somaNotas += nota;
    }
  }

  return somaNotas / (totalAlunos * turma.alunos[0].notas.length);
}

function encontrarMelhorAluno(turma: Turma): Aluno {
  let melhorAluno: Aluno | undefined;
  let melhorMedia = 0;

  for (const aluno of turma.alunos) {
    const media = aluno.notas.reduce((acc, nota) => acc + nota) / aluno.notas.length;

    if (media > melhorMedia) {
      melhorMedia = media;
      melhorAluno = aluno;
    }
  }

  return melhorAluno!;
}

// Exemplo de uso
const turma: Turma = {
  nome: "Turma A",
  alunos: [
    {
      nome: "João",
      idade: 18,
      notas: [8, 7, 9]
    },
    {
      nome: "Maria",
      idade: 19,
      notas: [9, 9, 10]
    },
    {
      nome: "Pedro",
      idade: 17,
      notas: [7, 6, 8]
    }
  ]
};

const mediaTurma = calcularMediaTurma(turma);
const melhorAluno = encontrarMelhorAluno(turma);

console.log(`A média da turma ${turma.nome} é ${mediaTurma.toFixed(2)}`);
console.log(`O melhor aluno da turma ${turma.nome} é ${melhorAluno!.nome}`);
```

Neste código, temos a definição de duas interfaces: `Aluno` e `Turma`. A interface `Aluno` representa os dados de um aluno, contendo nome, idade e um array de notas. A interface `Turma` representa uma turma de alunos, contendo um nome e um array de alunos.

Em seguida, temos duas funções principais: `calcularMediaTurma` e `encontrarMelhorAluno`. A função `calcularMediaTurma` recebe uma turma como parâmetro e calcula a média das notas de todos os alunos da turma. Ela percorre o array de alunos e o array de notas de cada aluno, somando todas as notas. Em seguida, divide a soma pelo total de alunos multiplicado pelo número de notas de um aluno. O resultado é retornado como a média da turma.

A função `encontrarMelhorAluno` recebe uma turma como parâmetro e retorna o aluno com a maior média de notas. Ela percorre o array de alunos, calcula a média de notas de cada aluno usando o método `reduce` do array, e compara com a média do melhor aluno encontrado até o momento. Se a média atual for maior, atualiza o melhor aluno. No final, retorna o melhor aluno encontrado.

No exemplo de uso, criamos uma turma com três alunos e chamamos as duas funções para obter a média da turma e o melhor aluno. Em seguida, imprimimos os resultados no console.

Espero que este código complexo e diferenciado em TypeScript seja útil para você!