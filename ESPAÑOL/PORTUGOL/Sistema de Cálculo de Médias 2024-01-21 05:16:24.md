```portuguol

// Declaração de variáveis globais
double notas[50], media, soma = 0;
int qtd_alunos = 0, opcao;

// Função principal
programa
{
  // Apresentar o menu de opções
  enquanto (opcao != 3)
  {
    escrever("1 - Cadastrar nota");
    escrever("2 - Exibir notas");
    escrever("3 - Sair");
    escrever("Digite a opção desejada: ");
    ler(opcao);

    // Processamento da opção selecionada
    se (opcao == 1)
    {
      // Cadastrar nota
      escrever("Digite a nota do aluno: ");
      ler(notas[qtd_alunos]);

      // Incrementar o contador de alunos
      qtd_alunos++;
    }
    senao se (opcao == 2)
    {
      // Exibir notas
      escrever("Notas dos alunos:");
      para (int i = 0; i < qtd_alunos; i++)
      {
        escrever(notas[i]);
      }
    }
  }

  // Calculando a média das notas
  para (int i = 0; i < qtd_alunos; i++)
  {
    soma += notas[i];
  }
  media = soma / qtd_alunos;

  // Exibir a média das notas
  escrever("Média das notas: ", media);
}

```