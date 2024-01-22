```portuguol
programa CalcularSalario {

    constantes tiposalario = (fixo, porhora, comissao);

    real horas, valorhora, vendas, comissao, salariobase, salariototal;

    leia(tiposalario, horas, valorhora, vendas, comissao, salariobase);

    caso tiposalario = 1 {
        salariototal = salariobase;
    }
    caso tiposalario = 2 {
        salariototal = horas * valorhora;
    }
    caso tiposalario = 3 {
        salariototal = vendas * comissao;
    }
    caso contrario {
        escreva("Tipo de salário inválido.");
    }

    escreva("Salário total: ", salariototal);

}
```

Este código calcula o salário total de um funcionário com base no tipo de salário (fixo, por hora ou comissão), o número de horas trabalhadas, o valor da hora trabalhada, o valor das vendas e a porcentagem de comissão.

O programa começa declarando as variáveis e constantes que serão usadas no código. As variáveis são usadas para armazenar os valores inseridos pelo usuário, enquanto as constantes são usadas para armazenar valores imutáveis.

Em seguida, o programa lê os valores das variáveis do usuário.

Depois, o programa usa a variável `tiposalario` para determinar o tipo de salário do funcionário.

Se o tipo de salário for fixo, o salário total é igual ao salário base.

Se o tipo de salário for por hora, o salário total é igual ao número de horas trabalhadas vezes o valor da hora trabalhada.

Se o tipo de salário for comissão, o salário total é igual ao valor das vendas vezes a porcentagem de comissão.

Se o tipo de salário for inválido, o programa exibe uma mensagem de erro.

Finalmente, o programa exibe o salário total do funcionário.