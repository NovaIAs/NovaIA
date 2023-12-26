Claro! Aqui está um código complexo em Objective-C que envolve a criação de uma calculadora com interface gráfica. Este código utiliza o framework Cocoa para criar a interface de usuário.

Primeiro, precisamos importar os frameworks necessários:

```objective-c
#import <Cocoa/Cocoa.h>
```

Em seguida, criaremos uma classe `Calculator` onde iremos definir as propriedades e métodos necessários para a calculadora:

```objective-c
@interface Calculator : NSObject

@property (nonatomic, strong) NSWindow *window;
@property (nonatomic, strong) NSTextField *displayTextField;
@property (nonatomic, strong) NSButton *buttonAdd;
@property (nonatomic, strong) NSButton *buttonSubtract;
@property (nonatomic, strong) NSButton *buttonMultiply;
@property (nonatomic, strong) NSButton *buttonDivide;
@property (nonatomic, strong) NSButton *buttonEquals;
@property (nonatomic, strong) NSButton *buttonClear;
@property (nonatomic, assign) double operand1;
@property (nonatomic, assign) double operand2;
@property (nonatomic, assign) double result;
@property (nonatomic, assign) BOOL isTypingNumber;
@property (nonatomic, assign) BOOL isDecimalPointPressed;
@property (nonatomic, assign) BOOL isOperationPerformed;

- (void)setupUI;
- (void)numberButtonPressed:(NSButton *)sender;
- (void)decimalPointButtonPressed:(NSButton *)sender;
- (void)operationButtonPressed:(NSButton *)sender;
- (void)equalsButtonPressed:(NSButton *)sender;
- (void)clearButtonPressed:(NSButton *)sender;
- (void)updateDisplay;
- (void)calculateResult;

@end
```

Em seguida, implementaremos os métodos da classe `Calculator`:

```objective-c
@implementation Calculator

- (void)setupUI {
    // Configuração da janela
    self.window = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 300, 400)
                                              styleMask:NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskMiniaturizable
                                                backing:NSBackingStoreBuffered
                                                  defer:NO];
    [self.window setTitle:@"Calculadora"];
    [self.window makeKeyAndOrderFront:nil];
    
    // Configuração do campo de exibição
    self.displayTextField = [[NSTextField alloc] initWithFrame:NSMakeRect(10, 340, 280, 50)];
    [self.displayTextField setEditable:NO];
    [self.displayTextField setAlignment:NSTextAlignmentRight];
    [self.displayTextField setFont:[NSFont systemFontOfSize:24]];
    [self.displayTextField setStringValue:@"0"];
    [self.window.contentView addSubview:self.displayTextField];
    
    // Configuração dos botões numéricos
    NSArray *numberButtonTitles = @[@"0", @"1", @"2", @"3", @"4", @"5", @"6", @"7", @"8", @"9"];
    for (int i = 0; i < 10; i++) {
        NSButton *numberButton = [[NSButton alloc] initWithFrame:NSMakeRect(10 + (i % 3) * 90, 250 - (i / 3) * 70, 80, 60)];
        [numberButton setTitle:numberButtonTitles[i]];
        [numberButton setTarget:self];
        [numberButton setAction:@selector(numberButtonPressed:)];
        [self.window.contentView addSubview:numberButton];
    }
    
    // Configuração do botão de ponto decimal
    NSButton *decimalPointButton = [[NSButton alloc] initWithFrame:NSMakeRect(100, 60, 80, 60)];
    [decimalPointButton setTitle:@"."];
    [decimalPointButton setTarget:self];
    [decimalPointButton setAction:@selector(decimalPointButtonPressed:)];
    [self.window.contentView addSubview:decimalPointButton];
    
    // Configuração dos botões de operação
    NSArray *operationButtonTitles = @[@"+", "-", "*", "/"];
    for (int i = 0; i < 4; i++) {
        NSButton *operationButton = [[NSButton alloc] initWithFrame:NSMakeRect(200, 250 - i * 70, 80, 60)];
        [operationButton setTitle:operationButtonTitles[i]];
        [operationButton setTarget:self];
        [operationButton setAction:@selector(operationButtonPressed:)];
        [self.window.contentView addSubview:operationButton];
    }
    
    // Configuração do botão de igual
    self.buttonEquals = [[NSButton alloc] initWithFrame:NSMakeRect(200, 60, 80, 120)];
    [self.buttonEquals setTitle:@"="];
    [self.buttonEquals setTarget:self];
    [self.buttonEquals setAction:@selector(equalsButtonPressed:)];
    [self.window.contentView addSubview:self.buttonEquals];
    
    // Configuração do botão de limpar
    self.buttonClear = [[NSButton alloc] initWithFrame:NSMakeRect(10, 60, 80, 120)];
    [self.buttonClear setTitle:@"C"];
    [self.buttonClear setTarget:self];
    [self.buttonClear setAction:@selector(clearButtonPressed:)];
    [self.window.contentView addSubview:self.buttonClear];
}

- (void)numberButtonPressed:(NSButton *)sender {
    if (!self.isTypingNumber) {
        [self.displayTextField setStringValue:@""];
        self.isTypingNumber = YES;
    }
    
    NSString *numberString = [sender title];
    [self.displayTextField setStringValue:[[self.displayTextField stringValue] stringByAppendingString:numberString]];
}

- (void)decimalPointButtonPressed:(NSButton *)sender {
    if (!self.isTypingNumber) {
        [self.displayTextField setStringValue:@"0"];
        self.isTypingNumber = YES;
    }
    
    if (!self.isDecimalPointPressed) {
        [self.displayTextField setStringValue:[[self.displayTextField stringValue] stringByAppendingString:@"."]];
        self.isDecimalPointPressed = YES;
    }
}

- (void)operationButtonPressed:(NSButton *)sender {
    if (self.isTypingNumber) {
        self.operand1 = [self.displayTextField doubleValue];
        self.isTypingNumber = NO;
        self.isDecimalPointPressed = NO;
    }
    
    NSString *operation = [sender title];
    if ([operation isEqualToString:@"+"]) {
        self.isOperationPerformed = YES;
    } else if ([operation isEqualToString:@"-"]) {
        self.isOperationPerformed = YES;
    } else if ([operation isEqualToString:@"*"]) {
        self.isOperationPerformed = YES;
    } else if ([operation isEqualToString:@"/"]) {
        self.isOperationPerformed = YES;
    }
}

- (void)equalsButtonPressed:(NSButton *)sender {
    if (self.isTypingNumber) {
        self.operand2 = [self.displayTextField doubleValue];
        self.isTypingNumber = NO;
        self.isDecimalPointPressed = NO;
    }
    
    [self calculateResult];
    [self updateDisplay];
}

- (void)clearButtonPressed:(NSButton *)sender {
    self.operand1 = 0;
    self.operand2 = 0;
    self.result = 0;
    self.isTypingNumber = NO;
    self.isDecimalPointPressed = NO;
    self.isOperationPerformed = NO;
    [self.displayTextField setStringValue:@"0"];
}

- (void)updateDisplay {
    [self.displayTextField setStringValue:[NSString stringWithFormat:@"%g", self.result]];
}

- (void)calculateResult {
    if (self.isOperationPerformed) {
        NSString *operation = [self.window.contentView viewWithTag:self.currentOperationTag];
        if ([operation isEqualToString:@"+"]) {
            self.result = self.operand1 + self.operand2;
        } else if ([operation isEqualToString:@"-"]) {
            self.result = self.operand1 - self.operand2;
        } else if ([operation isEqualToString:@"*"]) {
            self.result = self.operand1 * self.operand2;
        } else if ([operation isEqualToString:@"/"]) {
            self.result = self.operand1 / self.operand2;
        }
    }
}

@end
```

Por fim, na função `main` do seu programa, você pode criar uma instância da classe `Calculator` e chamar o método `setupUI` para iniciar a calculadora:

```objective-c
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Calculator *calculator = [[Calculator alloc] init];
        [calculator setupUI];
        [[NSApplication sharedApplication] run];
    }
    return 0;
}
```

Este código cria uma calculadora básica com interface gráfica em Objective-C. A interface consiste em botões numéricos, botões de operação (adição, subtração, multiplicação e divisão), um botão de igual e um botão de limpar. A medida que você digita os números e realiza as operações, o resultado é exibido no campo de exibição.

Espero que este código atenda às suas expectativas! Se você tiver mais alguma dúvida ou precisar de ajuda adicional, fique à vontade para perguntar.