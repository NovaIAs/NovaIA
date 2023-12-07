Claro! Aqui está um código complexo em Ada que encontra a soma de todos os números primos menores ou iguais a um determinado limite:

```
with Ada.Text_IO;

procedure Soma_Primeiros_N_Primos is
    
    -- Função para verificar se um número é primo ou não
    function Is_Primo(N : Integer) return Boolean is
        Primo : Boolean := True;
    begin
        if N <= 1 then
            -- 0 e 1 não são primos
            Primo := False;
        else
            for I in 2..Integer(Sqrt(Real(N))) loop
                if N mod I = 0 then
                    Primo := False;
                    exit;
                end if;
            end loop;
        end if;
        
        return Primo;
    end Is_Primo;
    
    Limite : Integer;
    Soma : Integer := 0;
    
begin
    -- Solicita o limite ao usuário
    Ada.Text_IO.Put("Digite o limite: ");
    Ada.Text_IO.Get(Limite);
    
    -- Encontra a soma dos N números primos
    for N in 2..Limite loop
        if Is_Primo(N) then
            Soma := Soma + N;
        end if;
    end loop;
    
    -- Imprime a soma dos números primos encontrados
    Ada.Text_IO.Put("A soma dos primeiros " & Integer'Image(Limite) & " números primos é: ");
    Ada.Text_IO.Put(Soma);
    Ada.Text_IO.New_Line;
    
end Soma_Primeiros_N_Primos;
```

Neste código, começamos definindo uma função chamada `Is_Primo`, que recebe um número como parâmetro e retorna `True` se ele for primo e `False` caso contrário. A função percorre todos os números de 2 até a raiz quadrada do número e verifica se ele é divisível por algum desses números.

Em seguida, temos as variáveis `Limite` e `Soma`. A variável `Limite` irá armazenar o valor máximo até o qual queremos encontrar os números primos e a variável `Soma` irá acumular a soma dos números primos encontrados.

Depois, solicitamos ao usuário que digite o limite através da função `Ada.Text_IO.Get`. Em seguida, utilizamos um loop `for` para percorrer todos os números de 2 até o limite informado. Se o número atual for primo, incrementamos a variável `Soma`.

Por fim, imprimimos a mensagem com a soma dos números primos utilizando a função `Ada.Text_IO.Put`. 

Agora você tem um código complexo em Ada que encontra a soma dos números primos menores ou iguais a um determinado limite.