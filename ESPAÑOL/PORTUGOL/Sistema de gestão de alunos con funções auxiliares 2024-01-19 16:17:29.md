```portuguol
ALGORITMO "Sistema de Gestão de Alunos"

DECLARAÇÕES

   TIPO Aluno = REGISTRO
      nome: LIT[50];
      matricula: INTEIRO;
      curso: LIT[30];
      semestre: INTEIRO;
      notas: NOTA[3];
   FIM REGISTRO;

   Aluno alunos[100];
   INTEIRO opcao, num_alunos; // variáveis inteiras

INÍCIO

   // inicialização do sistema
   num_alunos = 0;

   // menu de opções
   ENQUANTO opcao != 0 FAÇA

      // exibe o menu
      ESCREVA("1 - Cadastrar Aluno");
      ESCREVA("2 - Consultar Aluno");
      ESCREVA("3 - Alterar Aluno");
      ESCREVA("4 - Excluir Aluno");
      ESCREVA("5 - Relatório de Alunos");
      ESCREVA("0 - Sair");
      ESCREVA("Opção: ");
      LEIA(opcao);

      // executa a opção selecionada
      SE opcao = 1 ENTAO
         // cadastra um novo aluno
         SE num_alunos < 100 ENTAO
            alunos[num_alunos].nome = LEIA_NOME();
            alunos[num_alunos].matricula = LEIA_MATRICULA();
            alunos[num_alunos].curso = LEIA_CURSO();
            alunos[num_alunos].semestre = LEIA_SEMESTRE();
            alunos[num_alunos].notas = LEIA_NOTAS();
            num_alunos++;
         FIM SE
         SENAO
            ESCREVA("Número máximo de alunos atingido!");
         FIM SE

      FIM SE

      SE opcao = 2 ENTAO
         // consulta um aluno
         matricula = LEIA_MATRICULA();
         i = BUSCA_ALUNO(matricula);
         SE i >= 0 ENTAO
            EXIBE_ALUNO(i);
         FIM SE
         SENAO
            ESCREVA("Aluno não encontrado!");
         FIM SE

      FIM SE

      SE opcao = 3 ENTAO
         // altera os dados de um aluno
         matricula = LEIA_MATRICULA();
         i = BUSCA_ALUNO(matricula);
         SE i >= 0 ENTAO
            alunos[i].nome = LEIA_NOME();
            alunos[i].curso = LEIA_CURSO();
            alunos[i].semestre = LEIA_SEMESTRE();
            alunos[i].notas = LEIA_NOTAS();
         FIM SE
         SENAO
            ESCREVA("Aluno não encontrado!");
         FIM SE

      FIM SE

      SE opcao = 4 ENTAO
         // exclui um aluno
         matricula = LEIA_MATRICULA();
         i = BUSCA_ALUNO(matricula);
         SE i >= 0 ENTAO
            EXCLUI_ALUNO(i);
            num_alunos--;
         FIM SE
         SENAO
            ESCREVA("Aluno não encontrado!");
         FIM SE

      FIM SE

      SE opcao = 5 ENTAO
         // gera um relatório dos alunos
         GERA_RELATORIO();

      FIM SE

   FIM ENQUANTO

FIM ALGORITMO

// Funções auxiliares

FUNCAO LEIA_NOME() RETORNA LIT[50]
   // lê o nome do aluno
   CLRLIN();
   ESCREVA("Nome: ");
   LEIA(nome); // nome do aluno

   RETORNA nome;
FIM FUNCAO

FUNCAO LEIA_MATRICULA() RETORNA INTEIRO
   // lê a matrícula do aluno
   CLRLIN();
   ESCREVA("Matrícula: ");
   LEIA(matricula); // matrícula do aluno

   RETORNA matricula;
FIM FUNCAO

FUNCAO LEIA_CURSO() RETORNA LIT[30]
   // lê o curso do aluno
   CLRLIN();
   ESCREVA("Curso: ");
   LEIA(curso); // curso do aluno

   RETORNA curso;
FIM FUNCAO

FUNCAO LEIA_SEMESTRE() RETORNA INTEIRO
   // lê o semestre do aluno