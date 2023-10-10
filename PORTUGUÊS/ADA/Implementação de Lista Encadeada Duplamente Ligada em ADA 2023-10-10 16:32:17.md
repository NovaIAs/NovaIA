Claro! Aqui está um código complexo em ADA que cria uma estrutura de dados de lista encadeada duplamente ligada e implementa algumas operações básicas nessa estrutura. Vou explicar o código em detalhes.

```
with Ada.Text_IO; use Ada.Text_IO;

procedure Lista_Encadeada is
   type Elemento is record
      Dado : Integer;
      Anterior : access Elemento;
      Proximo : access Elemento;
   end record;

   type Lista is record
      Primeiro : access Elemento;
      Ultimo : access Elemento;
   end record;

   function Criar_Elemento (D : Integer) return access Elemento is
      Novo : access Elemento := new Elemento;
   begin
      Novo.Dado := D;
      Novo.Anterior := null;
      Novo.Proximo := null;
      return Novo;
   end Criar_Elemento;

   procedure Inserir_Elemento (L : in out Lista; D : Integer) is
      Novo : access Elemento := Criar_Elemento(D);
   begin
      if L.Primeiro = null then
         L.Primeiro := Novo;
         L.Ultimo := Novo;
      else
         L.Ultimo.Proximo := Novo;
         Novo.Anterior := L.Ultimo;
         L.Ultimo := Novo;
      end if;
   end Inserir_Elemento;

   procedure Remover_Elemento (L : in out Lista; D : Integer) is
      Atual : access Elemento := L.Primeiro;
   begin
      while Atual /= null loop
         if Atual.Dado = D then
            if Atual.Anterior /= null then
               Atual.Anterior.Proximo := Atual.Proximo;
            else
               L.Primeiro := Atual.Proximo;
            end if;

            if Atual.Proximo /= null then
               Atual.Proximo.Anterior := Atual.Anterior;
            else
               L.Ultimo := Atual.Anterior;
            end if;

            return;
         end if;

         Atual := Atual.Proximo;
      end loop;
   end Remover_Elemento;

   procedure Imprimir_Lista (L : Lista) is
      Atual : access Elemento := L.Primeiro;
   begin
      Put("A lista contém os seguintes elementos: ");

      while Atual /= null loop
         Put(Integer'Image(Atual.Dado) & " ");
         Atual := Atual.Proximo;
      end loop;

      New_Line;
   end Imprimir_Lista;

   L : Lista;

begin
   -- Teste da lista encadeada
   Inserir_Elemento(L, 10);
   Inserir_Elemento(L, 20);
   Inserir_Elemento(L, 30);
   Inserir_Elemento(L, 40);
   Imprimir_Lista(L);

   Remover_Elemento(L, 20);
   Imprimir_Lista(L);

   Remover_Elemento(L, 30);
   Imprimir_Lista(L);
end Lista_Encadeada;
```

Aqui, implementamos uma lista encadeada duplamente ligada em ADA. A lista é representada por uma estrutura de dados `Lista` que contém os ponteiros `Primeiro` e `Ultimo`. Cada elemento da lista é representado por uma estrutura `Elemento`, que possui um campo `Dado` (um número inteiro), e os ponteiros `Anterior` e `Proximo`, que apontam para o elemento anterior e o próximo elemento na lista, respectivamente.

A função `Criar_Elemento` cria um novo elemento da lista com o dado fornecido e retorna um ponteiro para esse elemento.

A procedure `Inserir_Elemento` insere um novo elemento no final da lista. Se a lista estiver vazia, o novo elemento se torna o primeiro e o último elemento. Caso contrário, o novo elemento é adicionado após o último elemento existente, os ponteiros são atualizados e o novo elemento se torna o novo último elemento.

A procedure `Remover_Elemento` remove um elemento com o dado fornecido da lista. Percorremos a lista até encontrar o elemento desejado e, em seguida, atualizamos os ponteiros do elemento anterior e do próximo elemento para remover o elemento da lista.

A procedure `Imprimir_Lista` imprime os elementos da lista na saída padrão.

No programa principal, testamos as operações da lista encadeada, inserindo alguns elementos e removendo outros. Em seguida, imprimimos a lista para verificar o resultado.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, sinta-se à vontade para perguntar.