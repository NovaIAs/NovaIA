```portugol
#include <stdio.h>

typedef struct {
    int dia;
    int mes;
    int ano;
} Data;

typedef struct {
    char nome[50];
    Data data_nascimento;
    float salario;
} Funcionario;

int main() {
    Funcionario funcionarios[100];
    int num_funcionarios = 0;

    while (true) {
        printf("Digite o nome do funcionário: ");
        scanf("%s", funcionarios[num_funcionarios].nome);

        printf("Digite a data de nascimento do funcionário (dd/mm/aaaa): ");
        scanf("%d/%d/%d", &funcionarios[num_funcionarios].data_nascimento.dia,
              &funcionarios[num_funcionarios].data_nascimento.mes,
              &funcionarios[num_funcionarios].data_nascimento.ano);

        printf("Digite o salário do funcionário: ");
        scanf("%f", &funcionarios[num_funcionarios].salario);

        num_funcionarios++;

        printf("Deseja cadastrar mais um funcionário? (s/n) ");
        char resposta;
        scanf(" %c", &resposta);

        if (resposta == 'n') {
            break;
        }
    }

    for (int i = 0; i < num_funcionarios; i++) {
        printf("\nFuncionário %d:\n", i + 1);
        printf("Nome: %s\n", funcionarios[i].nome);
        printf("Data de nascimento: %d/%d/%d\n", funcionarios[i].data_nascimento.dia,
               funcionarios[i].data_nascimento.mes, funcionarios[i].data_nascimento.ano);
        printf("Salário: %.2f\n", funcionarios[i].salario);
    }

    return 0;
}
```

Explicación:

Este código cria um programa em PORTUGOL que permite cadastrar funcionários e armazená-los em um array de structs. O programa solicita ao usuário que insira o nome, a data de nascimento e o salário de cada funcionário. Depois de cadastrar todos os funcionários desejados, o programa exibe uma lista com as informações de todos os funcionários cadastrados.

Aqui estão algumas explicações dos comandos e declarações usados no código:

* **`typedef struct ...`**: Esta declaração define uma estrutura de dados chamada `Funcionario`, que contém três campos: `nome`, `data_nascimento` e `salario`.
* **`Funcionario funcionarios[100];`**: Esta declaração cria um array de estruturas `Funcionario` chamado `funcionarios`, que pode armazenar até 100 funcionários.
* **`int num_funcionarios = 0;`**: Esta declaração cria uma variável inteira chamada `num_funcionarios`, que é usada para armazenar o número de funcionários cadastrados.
* **`while (true) { ... }`**: Este é um loop infinito que continua sendo executado até que o usuário digite 'n' para parar de cadastrar funcionários.
* **`printf()`** e **`scanf()`**: Estas funções são usadas para imprimir mensagens na tela e ler a entrada do usuário, respectivamente.
* **`for (int i = 0; i < num_funcionarios; i++) { ... }`**: Este é um loop que itera sobre o array `funcionarios` e exibe as informações de cada funcionário cadastrado.

Este código é complexo porque usa várias estruturas de dados e comandos diferentes, como structs, arrays, loops e funções. Também é complexo porque requer que o usuário insira dados de forma precisa e consistente.