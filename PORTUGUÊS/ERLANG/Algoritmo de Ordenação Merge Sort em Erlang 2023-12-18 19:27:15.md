Claro! Aqui está um código em Erlang que implementa um algoritmo de ordenação chamado Merge Sort, completamente em português.

```erlang
-module(merge_sort).
-export([merge_sort/1]).

merge_sort(Lista) ->
    case length(Lista) of
        0 -> Lista;
        1 -> Lista;
        _ -> 
            {Esquerda, Direita} = dividir(Lista),
            merge(merge_sort(Esquerda), merge_sort(Direita))
    end.

dividir(Lista) ->
    dividir(Lista, [], Lista).

dividir([], Esquerda, Direita) ->
    {Esquerda, Direita};
dividir([_], Esquerda, Direita) ->
    {Esquerda, Direita};
dividir([_, _ | Resto], Esquerda, Direita) ->
    dividir(Resto, [hd(Resto) | Esquerda], [hd(tl(Resto)) | Direita]).

merge([], Lista) ->
    Lista;
merge(Lista, []) ->
    Lista;
merge([Cabeca1 | Resto1], [Cabeca2 | Resto2]) ->
    if
        Cabeca1 =< Cabeca2 ->
            [Cabeca1 | merge(Resto1, [Cabeca2 | Resto2])];
        true ->
            [Cabeca2 | merge([Cabeca1 | Resto1], Resto2)]
    end.
```

O código acima implementa o algoritmo Merge Sort em Erlang. O Merge Sort é um algoritmo de ordenação eficiente que divide a lista em pequenos pedaços, ordena cada pedaço individualmente e, em seguida, junta-os em uma única lista ordenada.

A função `merge_sort` é a função principal que recebe a lista a ser ordenada como entrada. Ela verifica o tamanho da lista e, se for igual a 0 ou 1, a lista já está ordenada e é retornada como está. Caso contrário, a função chama a função auxiliar `dividir` para dividir a lista em duas partes, e então chama recursivamente o `merge_sort` para ordenar cada parte separadamente. Por fim, as partes ordenadas são mescladas usando a função `merge`.

A função `dividir` é responsável por dividir a lista em duas partes aproximadamente iguais. Ela usa o conceito de duas listas, `Esquerda` e `Direita`, para armazenar os elementos de forma intercalada. A função percorre a lista original removendo elementos de cada vez e os adicionando nas listas `Esquerda` e `Direita` alternadamente.

A função `merge` é responsável por mesclar duas listas ordenadas. Ela compara os elementos das duas listas, começando pelas cabeças, e os adiciona à lista resultante na ordem correta. O processo é repetido até que uma das listas esteja vazia, e em seguida, a lista não vazia é concatenada com a lista resultante.

Este código é uma implementação simples do Merge Sort em Erlang e pode ser utilizado para ordenar qualquer tipo de lista.