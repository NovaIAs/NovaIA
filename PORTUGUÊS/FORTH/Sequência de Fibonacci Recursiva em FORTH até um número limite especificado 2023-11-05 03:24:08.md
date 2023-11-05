Aqui está um exemplo de código complexo em FORTH, escrito em português:

\ Código para gerar uma sequência de Fibonacci recursivamente até um número limite especificado

\ Definir função Fibonacci que recebe um número limite
: Fibonacci ( limite -- )
  0 1 \ Definir os dois primeiros termos da sequência
  swap \ Trocar o limite com o segundo termo (1)
  swap \ Trocar o limite com o primeiro termo (0)
  begin \ Iniciar o loop
    dup \ Duplicar o limite
    over \ Copiar o segundo termo
    + \ Somar o primeiro e o segundo termo
    swap \ Trocar a soma com o segundo termo
    swap \ Trocar a soma com o limite
    swap \ Trocar o segundo termo com o limite
    dup \ Duplicar o limite
    . \ Exibir o termo atual da sequência
    over \ Copiar o segundo termo
    <= \ Verificar se o termo atual é menor ou igual ao limite
  until \ Repetir até que o termo atual seja maior que o limite
  drop \ Remover o limite da pilha
;

\ Exemplo de uso
10 Fibonacci

\ O código acima irá gerar a sequência de Fibonacci recursivamente até o número limite 10 e exibirá os termos da sequência no console.