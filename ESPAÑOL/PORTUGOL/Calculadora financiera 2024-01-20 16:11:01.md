```portuguol
programa "Calculadora financeira"

constantes
    taxa_juros = 0.10                 // taxa de juros mensal
    periodos = 12                     // número de períodos (meses)
    valor_inicial = 1000.00           // valor inicial

variáveis
    valor_final, pagamento_mensal

procedimento "Calcular valor final"
    valor_final = valor_inicial * (1 + taxa_juros) ^ períodos

procedimento "Calcular pagamento mensal"
    pagamento_mensal = valor_final / períodos

início
    Calcular valor final
    Calcular pagamento mensal
    Escrever "Valor final:", valor_final, "Pagamento mensal:", pagamento_mensal

fim

```

Este código em PORTUGOL calcula o valor final e o pagamento mensal de um empréstimo, dados a taxa de juros mensal, o número de períodos (meses) e o valor inicial.

O código define as constantes `taxa_juros`, `periodos` e `valor_inicial`.

Em seguida, define as variáveis `valor_final` e `pagamento_mensal`.

O procedimento `Calcular valor final` calcula o valor final do empréstimo usando a fórmula `valor_final = valor_inicial * (1 + taxa_juros) ^ períodos`.

O procedimento `Calcular pagamento mensal` calcula o pagamento mensal do empréstimo usando a fórmula `pagamento_mensal = valor_final / períodos`.

O procedimento `início` chama os procedimentos `Calcular valor final` e `Calcular pagamento mensal` e escreve os resultados na tela.