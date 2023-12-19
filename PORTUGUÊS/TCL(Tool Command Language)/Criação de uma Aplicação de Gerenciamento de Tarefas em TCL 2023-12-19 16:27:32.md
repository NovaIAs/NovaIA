Aqui está um código complexo em TCL que envolve a criação de uma aplicação de gerenciamento de tarefas. Este código utiliza uma interface gráfica para que o usuário possa adicionar, remover e listar tarefas.

```tcl
# Definindo listas vazias para armazenar as tarefas
set tasks {}
set completedTasks {}

# Função para adicionar tarefas
proc addTask {} {
    global tasks
    set taskName [tk_dialog .addTaskDialog "Adicionar Tarefa" "Digite o nome da tarefa:" ""]
    if {$taskName ne ""} {
        lappend tasks $taskName
        updateTaskList
    }
}

# Função para remover tarefas
proc removeTask {} {
    global tasks completedTasks
    set selectedTask [listbox .taskListbox get [listbox .taskListbox curselection]]
    if {$selectedTask ne ""} {
        if {[tk_messageBox -type yesno -title "Remover Tarefa" -message "Tem certeza de que deseja remover a tarefa '$selectedTask'?"] eq "yes"} {
            lappend completedTasks $selectedTask
            set tasks [lreplace $tasks [lsearch $tasks $selectedTask] [lsearch $tasks $selectedTask]]
            updateTaskList
        }
    }
}

# Função para atualizar a lista de tarefas
proc updateTaskList {} {
    global tasks
    .taskListbox delete 0 end
    foreach task $tasks {
        .taskListbox insert end $task
    }
}

# Criação da janela principal
wm title . "Gerenciador de Tarefas"
wm geometry . 400x300

# Criação do botão "Adicionar Tarefa"
button .addTaskButton -text "Adicionar Tarefa" -command addTask
pack .addTaskButton -side left -padx 10 -pady 10

# Criação do botão "Remover Tarefa"
button .removeTaskButton -text "Remover Tarefa" -command removeTask
pack .removeTaskButton -side left -padx 10 -pady 10

# Criação da lista de tarefas
listbox .taskListbox -width 40 -height 15
pack .taskListbox -side left -fill both -padx 10 -pady 10

# Loop principal da aplicação
updateTaskList
tkwait window .
```

Este código começa definindo duas variáveis globais, `tasks` e `completedTasks`, que serão usadas para armazenar as tarefas adicionadas pelo usuário e as tarefas completadas, respectivamente.

Em seguida, temos a função `addTask`, que abre um diálogo de entrada de texto para que o usuário possa adicionar uma nova tarefa à lista `tasks`.

A função `removeTask` permite que o usuário remova uma tarefa selecionada da lista `tasks`. Antes de remover a tarefa, exibe um diálogo de confirmação para garantir que o usuário realmente deseja remover a tarefa selecionada. Se confirmado, a tarefa é removida da lista `tasks` e adicionada à lista `completedTasks`.

A função `updateTaskList` é responsável por atualizar a lista de tarefas exibida na interface gráfica. Esta função remove todos os itens da lista de tarefas existente e, em seguida, insere novamente todas as tarefas presentes na lista `tasks`.

Depois dessas definições de função, o código cria a janela principal da aplicação e configura sua geometria e título.

Em seguida, cria dois botões, "Adicionar Tarefa" e "Remover Tarefa", que são associados às funções `addTask` e `removeTask`, respectivamente. Esses botões são organizados na interface usando o gerenciador de layout `pack`.

Logo após, é criada uma lista de tarefas usando o widget `listbox`, que permitirá ao usuário visualizar e selecionar as tarefas. A lista é configurada com uma largura de 40 caracteres e altura de 15 linhas, e também é organizada na interface usando o `pack`.

Por fim, o código chama a função `updateTaskList` para preencher a lista de tarefas inicialmente, e em seguida, chama `tkwait` para iniciar o loop principal da aplicação, aguardando que a janela principal seja fechada pelo usuário.

Este código cria uma interface gráfica básica que permite ao usuário adicionar e remover tarefas, além de exibir a lista de tarefas atualizada em tempo real.