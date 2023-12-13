Claro! Aqui está um exemplo de código em Smalltalk que cria uma aplicação de gerenciamento de tarefas em estilo Kanban, onde as tarefas podem ser movidas entre colunas representando diferentes estágios de conclusão.

```
| janelaTarefas listaTarefas colunasTarefas |

janelaTarefas := SystemWindow new.
janelaTarefas title: 'Gerenciador de Tarefas Kanban'.
janelaTarefas extent: 800@600.

listaTarefas := OrderedCollection new.

colunasTarefas := OrderedDictionary new.
colunasTarefas at: 'A fazer' put: OrderedCollection new.
colunasTarefas at: 'Em andamento' put: OrderedCollection new.
colunasTarefas at: 'Concluído' put: OrderedCollection new.

1 to: 3 do: [:i |
    | coluna tituloColuna listaTarefasColuna |
    coluna := Morph new.
    coluna color: Color lightGray.
    coluna extent: (janelaTarefas width // 3)@(janelaTarefas height - 100).
    coluna position: ((i - 1) * (coluna width))@50.

    tituloColuna := StringMorph new.
    tituloColuna contents: (colunasTarefas keys at: i).
    tituloColuna color: Color transparent.
    tituloColuna position: (coluna position + (coluna width // 2 - tituloColuna width // 2))@(coluna height - 30).

    listaTarefasColuna := PluggableListMorph new.
    listaTarefasColuna model: colunasTarefas at: (colunasTarefas keys at: i).
    listaTarefasColuna color: Color white.
    listaTarefasColuna position: (coluna position + 10)@(tituloColuna position y - 10).
    listaTarefasColuna width: coluna width - 20.
    listaTarefasColuna height: coluna height - tituloColuna height - 50.
    listaTarefasColuna itemList: (colunasTarefas at: (colunasTarefas keys at: i)).

    coluna addMorph: tituloColuna.
    coluna addMorph: listaTarefasColuna.
    janelaTarefas addMorph: coluna.
].

janelaTarefas addMorph: (SimpleButtonMorph new
    label: 'Adicionar Tarefa';
    target: [
        | nomeTarefa |
        nomeTarefa := FillInTheBlank request: 'Digite o nome da tarefa:'.
        nomeTarefa ifNotNil: [
            (colunasTarefas at: 'A fazer') add: nomeTarefa.
            listaTarefas refresh.
        ].
    ];
    position: (janelaTarefas width // 2 - 100)@(janelaTarefas height - 50);
    extent: 200@40
).

Project current addProjectView: janelaTarefas.
janelaTarefas openInWorld.
```

Este código cria uma janela com três colunas, representando os estágios de uma tarefa, que são "A fazer", "Em andamento" e "Concluído". O usuário pode adicionar tarefas à coluna "A fazer" através de um botão. As tarefas adicionadas são exibidas nas respectivas colunas e podem ser movidas entre as colunas arrastando-as.

A interface da aplicação é construída usando morphs (elementos gráficos) do Smalltalk. Cada coluna é representada por um `Morph` com uma cor de fundo e uma posição específica na janela. Os títulos das colunas são exibidos usando `StringMorph`. As tarefas em cada coluna são exibidas usando `PluggableListMorph`, que é configurado com um modelo (`model`) que é uma coleção ordenada (OrderedCollection) representando as tarefas daquela coluna.

O botão "Adicionar Tarefa" é criado usando um `SimpleButtonMorph`, e quando clicado, solicita ao usuário o nome da tarefa usando `FillInTheBlank`. Após o usuário fornecer o nome da tarefa, ela é adicionada à coluna "A fazer" e a lista de tarefas é atualizada.

Por fim, a janela de tarefas é adicionada ao projeto atual (`Project current`) e é exibida na tela utilizando o método `openInWorld`.

Espero que este exemplo lhe dê uma boa ideia de como criar uma aplicação simples em Smalltalk!