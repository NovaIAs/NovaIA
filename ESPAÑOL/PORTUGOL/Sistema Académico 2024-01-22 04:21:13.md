```portuguol
programa SistemaAcademico {

    // Constantes
    const inteiro MAX_ALUNOS = 100;

    // Tipos
    tipo Aluno {
        nome: string;
        matricula: inteiro;
        curso: string;
        notas: vetor de real[4];
        media: real;
    }

    tipo Disciplina {
        nome: string;
        codigo: inteiro;
        cargaHoraria: inteiro;
        professores: vetor de Aluno[2];
        alunos: vetor de Aluno[MAX_ALUNOS];
    }

    tipo Curso {
        nome: string;
        codigo: inteiro;
        disciplinas: vetor de Disciplina[10];
        alunos: vetor de Aluno[MAX_ALUNOS];
    }

    // Variáveis
    vetor de Curso cursos[10];
    vetor de Aluno alunos[MAX_ALUNOS];
    vetor de Disciplina disciplinas[100];

    // Funções
    funcao void cadastrarAluno(Aluno aluno) {
        alunos[aluno.matricula] = aluno;
    }

    funcao void cadastrarDisciplina(Disciplina disciplina) {
        disciplinas[disciplina.codigo] = disciplina;
    }

    funcao void cadastrarCurso(Curso curso) {
        cursos[curso.codigo] = curso;
    }

    funcao real calcularMedia(Aluno aluno, Disciplina disciplina) {
        real media = 0;
        para (inteiro i = 0; i < 4; i++) {
            media += aluno.notas[i] * disciplina.cargaHoraria;
        }
        media /= disciplina.cargaHoraria;
        aluno.media = media;
        retorna media;
    }

    funcao void imprimirBoletim(Aluno aluno) {
        escreva("Boletim do Aluno:", aluno.nome, "\n");
        escreva("Matricula:", aluno.matricula, "\n");
        escreva("Curso:", aluno.curso, "\n");
        escreva("Notas:\n");
        para (inteiro i = 0; i < 4; i++) {
            escreva("Disciplina:", disciplinas[aluno.notas[i]].nome, "\n");
            escreva("Nota:", aluno.notas[i], "\n");
        }
        escreva("Media:", aluno.media, "\n");
    }

    // Código principal
    inteiro opcao;
    repita {
        escreva("Escolha uma opção:\n");
        escreva("1. Cadastrar aluno\n");
        escreva("2. Cadastrar disciplina\n");
        escreva("3. Cadastrar curso\n");
        escreva("4. Calcular média de um aluno\n");
        escreva("5. Imprimir boletim de um aluno\n");
        escreva("0. Sair\n");
        leia(opcao);

        se (opcao == 1) {
            Aluno aluno;
            escreva("Nome do aluno:", "\n");
            leia(aluno.nome);
            escreva("Matricula do aluno:", "\n");
            leia(aluno.matricula);
            escreva("Curso do aluno:", "\n");
            leia(aluno.curso);
            para (inteiro i = 0; i < 4; i++) {
                escreva("Nota", i + 1, ":", "\n");
                leia(aluno.notas[i]);
            }
            aluno.media = calcularMedia(aluno, disciplinas[0]);
            cadastrarAluno(aluno);
        }
        se (opcao == 2) {
            Disciplina disciplina;
            escreva("Nome da disciplina:", "\n");
            leia(disciplina.nome);
            escreva("Código da disciplina:", "\n");
            leia(disciplina.codigo);
            escreva("Carga horária da disciplina:", "\n");
            leia(disciplina.cargaHoraria);
            para (inteiro i = 0; i < 2; i++) {
                escreva("Professor", i + 1, ":", "\n");
                leia(disciplina.professores[i]);
            }
            cadastrarDisciplina(disciplina);