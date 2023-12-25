Claro! Aqui está um exemplo de código em Smalltalk que cria uma calculadora simples com operações de soma, subtração, multiplicação e divisão:

```
Object subclass: Calculadora [
    | valor |
    
    Calculadora class >> new [
        ^ super new initialize
    ]
    
    initialize [
        valor := 0
    ]
    
    somar: umNumero [
        valor := valor + umNumero
    ]
    
    subtrair: umNumero [
        valor := valor - umNumero
    ]
    
    multiplicar: umNumero [
        valor := valor * umNumero
    ]
    
    dividir: umNumero [
        valor := valor / umNumero
    ]
    
    resultado [
        ^ valor
    ]
]

| calculadora |
calculadora := Calculadora new.
calculadora somar: 10.
calculadora subtrair: 5.
calculadora multiplicar: 2.
calculadora dividir: 3.
Transcript show: 'O resultado é: ', calculadora resultado asString
```

Neste código, criamos uma classe chamada `Calculadora` que possui um atributo `valor` para armazenar o resultado das operações. A classe tem os métodos `somar:`, `subtrair:`, `multiplicar:` e `dividir:` para realizar as operações matemáticas. O método `resultado` retorna o valor atual da calculadora.

No exemplo final, criamos uma instância da classe `Calculadora`, realizamos algumas operações (somar 10, subtrair 5, multiplicar por 2 e dividir por 3) e mostramos o resultado na janela Transcript.

Espero que este exemplo em Smalltalk seja útil para você!