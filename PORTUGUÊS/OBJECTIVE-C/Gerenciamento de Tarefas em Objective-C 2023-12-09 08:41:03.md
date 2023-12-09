Claro! Aqui está um exemplo de um código complexo em Objective-C que utiliza classes, métodos e estruturas de controle para criar um sistema de gerenciamento de tarefas:

```objective-c
#import <Foundation/Foundation.h>

@interface Tarefa : NSObject

@property (nonatomic, strong) NSString *nome;
@property (nonatomic, strong) NSString *data;
@property (nonatomic, assign) BOOL completa;

@end

@implementation Tarefa

@end

@interface GerenciadorTarefas : NSObject

@property (nonatomic, strong) NSMutableArray *tarefas;

- (void)adicionarTarefa:(Tarefa *)tarefa;
- (void)removerTarefa:(Tarefa *)tarefa;
- (void)listarTarefas;
- (void)completarTarefa:(Tarefa *)tarefa;

@end

@implementation GerenciadorTarefas

- (instancetype)init {
    self = [super init];
    if (self) {
        _tarefas = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)adicionarTarefa:(Tarefa *)tarefa {
    [_tarefas addObject:tarefa];
    NSLog(@"Tarefa %@ adicionada com sucesso!", tarefa.nome);
}

- (void)removerTarefa:(Tarefa *)tarefa {
    [_tarefas removeObject:tarefa];
    NSLog(@"Tarefa %@ removida com sucesso!", tarefa.nome);
}

- (void)listarTarefas {
    if (_tarefas.count == 0) {
        NSLog(@"Nenhuma tarefa encontrada.");
    } else {
        NSLog(@"Lista de tarefas:");
        for (Tarefa *tarefa in _tarefas) {
            NSString *status = tarefa.completa ? @"Completa" : @"Incompleta";
            NSLog(@"- %@ - %@ - %@", tarefa.nome, tarefa.data, status);
        }
    }
}

- (void)completarTarefa:(Tarefa *)tarefa {
    tarefa.completa = YES;
    NSLog(@"Tarefa %@ marcada como completa!", tarefa.nome);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        GerenciadorTarefas *gerenciador = [[GerenciadorTarefas alloc] init];
        
        // Criando algumas tarefas
        Tarefa *tarefa1 = [[Tarefa alloc] init];
        tarefa1.nome = @"Comprar leite";
        tarefa1.data = @"01/01/2022";
        tarefa1.completa = NO;
        
        Tarefa *tarefa2 = [[Tarefa alloc] init];
        tarefa2.nome = @"Estudar programação";
        tarefa2.data = @"02/01/2022";
        tarefa2.completa = NO;
        
        // Adicionando as tarefas ao gerenciador
        [gerenciador adicionarTarefa:tarefa1];
        [gerenciador adicionarTarefa:tarefa2];
        
        // Listando as tarefas
        [gerenciador listarTarefas];
        
        // Marcando a primeira tarefa como completa
        [gerenciador completarTarefa:tarefa1];
        
        // Listando as tarefas novamente
        [gerenciador listarTarefas];
        
        // Removendo a segunda tarefa
        [gerenciador removerTarefa:tarefa2];
        
        // Listando as tarefas novamente
        [gerenciador listarTarefas];
    }
    return 0;
}
```

Neste código, temos duas classes: `Tarefa` e `GerenciadorTarefas`. A classe `Tarefa` representa uma tarefa individual, com propriedades como nome, data e um indicador de completude. A classe `GerenciadorTarefas` é responsável por gerenciar uma lista de tarefas, permitindo adicionar, remover, listar e marcar tarefas como completas.

No método `main`, criamos um objeto `GerenciadorTarefas` e algumas tarefas de exemplo. Em seguida, chamamos os métodos do gerenciador para adicionar as tarefas, listá-las, marcar uma tarefa como completa e remover uma tarefa. Os resultados são exibidos no console através das mensagens de log.

Espero que esse código complexo em Objective-C atenda às suas expectativas!