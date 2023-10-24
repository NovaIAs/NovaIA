Claro! Abaixo está um exemplo de um código complexo em Objective-C que usa classes, métodos e manipulação de dados para implementar um sistema de gerenciamento de funcionários.

```objective-c
#import <Foundation/Foundation.h>

// Declaração da classe Employee
@interface Employee : NSObject

@property (nonatomic, strong) NSString *name;
@property (nonatomic, strong) NSString *position;
@property (nonatomic) float salary;

- (instancetype)initWithName:(NSString *)name position:(NSString *)position salary:(float)salary;
- (void)displayInformation;

@end

@implementation Employee

- (instancetype)initWithName:(NSString *)name position:(NSString *)position salary:(float)salary {
    self = [super init];
    if (self) {
        _name = name;
        _position = position;
        _salary = salary;
    }
    return self;
}

- (void)displayInformation {
    NSLog(@"Name: %@", self.name);
    NSLog(@"Position: %@", self.position);
    NSLog(@"Salary: %.2f", self.salary);
}

@end

// Declaração da classe EmployeeManager
@interface EmployeeManager : NSObject

@property (nonatomic, strong) NSMutableArray *employees;

- (instancetype)init;
- (void)addEmployee:(Employee *)employee;
- (void)removeEmployee:(Employee *)employee;
- (Employee *)getEmployeeWithName:(NSString *)name;

@end

@implementation EmployeeManager

- (instancetype)init {
    self = [super init];
    if (self) {
        _employees = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)addEmployee:(Employee *)employee {
    [self.employees addObject:employee];
}

- (void)removeEmployee:(Employee *)employee {
    [self.employees removeObject:employee];
}

- (Employee *)getEmployeeWithName:(NSString *)name {
    for (Employee *employee in self.employees) {
        if ([employee.name isEqualToString:name]) {
            return employee;
        }
    }
    return nil;
}

@end

// Exemplo de uso do código
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Criando instâncias de funcionários
        Employee *employee1 = [[Employee alloc] initWithName:@"João" position:@"Desenvolvedor" salary:5000.0];
        Employee *employee2 = [[Employee alloc] initWithName:@"Maria" position:@"Gerente de Projetos" salary:8000.0];
        
        // Criando instância do gerenciador de funcionários
        EmployeeManager *employeeManager = [[EmployeeManager alloc] init];
        
        // Adicionando funcionários ao gerenciador
        [employeeManager addEmployee:employee1];
        [employeeManager addEmployee:employee2];
        
        // Exibindo informações dos funcionários
        [employee1 displayInformation];
        [employee2 displayInformation];
        
        // Removendo funcionário
        [employeeManager removeEmployee:employee2];
        
        // Buscando funcionário pelo nome
        Employee *foundEmployee = [employeeManager getEmployeeWithName:@"João"];
        if (foundEmployee != nil) {
            [foundEmployee displayInformation];
        } else {
            NSLog(@"Funcionário não encontrado.");
        }
    }
    return 0;
}
```

Neste exemplo, temos duas classes: `Employee` e `EmployeeManager`. A classe `Employee` representa um funcionário com propriedades como nome, cargo e salário. Ela também possui um método para exibir as informações do funcionário.

A classe `EmployeeManager` é responsável por gerenciar uma lista de funcionários. Ela possui métodos para adicionar, remover e buscar funcionários.

No exemplo de uso do código, criamos duas instâncias de funcionários, adicionamos ao `EmployeeManager`, exibimos as informações dos funcionários, removemos um funcionário e buscamos um funcionário pelo nome.

Este código demonstra alguns conceitos importantes de Objective-C, como criação de classes, métodos, propriedades e manipulação de objetos. É um exemplo completo de um sistema simples de gerenciamento de funcionários.