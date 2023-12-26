Claro! Aqui está um código complexo em Rust que envolve múltiplas funcionalidades e utiliza alguns conceitos avançados da linguagem. O código abaixo é um exemplo de um sistema de gerenciamento de tarefas, onde é possível adicionar, excluir e exibir tarefas por meio de uma interface de linha de comando.

```rust
use std::io;
use std::collections::HashMap;

struct Tarefa {
    descricao: String,
    concluida: bool,
}

struct GerenciadorTarefas {
    tarefas: HashMap<u32, Tarefa>,
    proximo_id: u32,
}

impl GerenciadorTarefas {
    fn nova() -> GerenciadorTarefas {
        GerenciadorTarefas {
            tarefas: HashMap::new(),
            proximo_id: 1,
        }
    }
    
    fn adicionar_tarefa(&mut self, descricao: String) {
        let tarefa = Tarefa {
            descricao,
            concluida: false,
        };
        self.tarefas.insert(self.proximo_id, tarefa);
        self.proximo_id += 1;
    }
    
    fn remover_tarefa(&mut self, id: u32) {
        self.tarefas.remove(&id);
    }
    
    fn exibir_tarefas(&self) {
        for (id, tarefa) in &self.tarefas {
            let status = if tarefa.concluida { "Concluída" } else { "Pendente" };
            println!("ID: {}, Descrição: {}, Status: {}", id, tarefa.descricao, status);
        }
    }
    
    fn marcar_tarefa_concluida(&mut self, id: u32) {
        if let Some(tarefa) = self.tarefas.get_mut(&id) {
            tarefa.concluida = true;
        }
    }
}

fn main() {
    let mut gerenciador = GerenciadorTarefas::nova();
    
    loop {
        println!("O que você deseja fazer? (adicionar, remover, exibir, marcar_concluida, sair)");
        
        let mut comando = String::new();
        io::stdin().read_line(&mut comando).expect("Falha ao ler o comando");
        let comando = comando.trim();
        
        match comando {
            "adicionar" => {
                println!("Digite a descrição da tarefa:");
                let mut descricao = String::new();
                io::stdin().read_line(&mut descricao).expect("Falha ao ler a descrição");
                gerenciador.adicionar_tarefa(descricao.trim().to_string());
                println!("Tarefa adicionada com sucesso!");
            },
            "remover" => {
                println!("Digite o ID da tarefa a ser removida:");
                let mut id_str = String::new();
                io::stdin().read_line(&mut id_str).expect("Falha ao ler o ID");
                let id: u32 = id_str.trim().parse().expect("ID inválido");
                gerenciador.remover_tarefa(id);
                println!("Tarefa removida com sucesso!");
            },
            "exibir" => {
                println!("Tarefas:");
                gerenciador.exibir_tarefas();
            },
            "marcar_concluida" => {
                println!("Digite o ID da tarefa a ser marcada como concluída:");
                let mut id_str = String::new();
                io::stdin().read_line(&mut id_str).expect("Falha ao ler o ID");
                let id: u32 = id_str.trim().parse().expect("ID inválido");
                gerenciador.marcar_tarefa_concluida(id);
                println!("Tarefa marcada como concluída com sucesso!");
            },
            "sair" => break,
            _ => println!("Comando inválido"),
        }
    }
}
```

Neste código, foi criada uma estrutura `Tarefa` que representa uma tarefa com uma descrição e um status de conclusão, e uma estrutura `GerenciadorTarefas` que armazena todas as tarefas e fornece métodos para adicionar, remover, exibir e marcar tarefas como concluídas.

A função `main` é onde ocorre a interação com o usuário por meio de um loop `loop` e um sistema de linha de comando simples. O usuário pode executar comandos como "adicionar", "remover", "exibir", "marcar_concluida" e "sair". Dependendo do comando escolhido, o código chama os métodos correspondentes na estrutura `GerenciadorTarefas`.

Espero que este código atenda às suas expectativas!