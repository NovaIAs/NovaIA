Claro! Aqui está um exemplo de código complexo em Swift, que consiste em criar uma calculadora com interface gráfica:

```swift
import UIKit

class ViewController: UIViewController {
    
    // Declaração dos elementos da interface gráfica
    var displayLabel: UILabel!
    var clearButton: UIButton!
    var numberButtons: [UIButton] = []
    var operatorButtons: [UIButton] = []
    var equalButton: UIButton!
    
    // Variáveis para armazenar os valores e a operação atual
    var firstNumber: Double = 0
    var secondNumber: Double = 0
    var currentOperator: String = ""
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        // Configuração da tela
        view.backgroundColor = .white
        
        // Criação do label de exibição
        displayLabel = UILabel(frame: CGRect(x: 20, y: 100, width: view.frame.width - 40, height: 80))
        displayLabel.textAlignment = .right
        displayLabel.font = UIFont.systemFont(ofSize: 40)
        view.addSubview(displayLabel)
        
        // Criação dos botões numéricos
        for i in 0...9 {
            let button = UIButton(type: .system)
            button.frame = CGRect(x: 20 + (i % 3) * 80, y: 200 + (i / 3) * 80, width: 60, height: 60)
            button.setTitle("\(i)", for: .normal)
            button.titleLabel?.font = UIFont.systemFont(ofSize: 30)
            button.addTarget(self, action: #selector(numberButtonTapped(_:)), for: .touchUpInside)
            numberButtons.append(button)
            view.addSubview(button)
        }
        
        // Criação dos botões de operação
        let operators: [String] = ["+", "-", "*", "/"]
        for i in 0..<operators.count {
            let button = UIButton(type: .system)
            button.frame = CGRect(x: view.frame.width - 80, y: 200 + i * 80, width: 60, height: 60)
            button.setTitle(operators[i], for: .normal)
            button.titleLabel?.font = UIFont.systemFont(ofSize: 30)
            button.addTarget(self, action: #selector(operatorButtonTapped(_:)), for: .touchUpInside)
            operatorButtons.append(button)
            view.addSubview(button)
        }
        
        // Criação do botão de igual
        equalButton = UIButton(type: .system)
        equalButton.frame = CGRect(x: 20, y: 440, width: view.frame.width - 40, height: 60)
        equalButton.setTitle("=", for: .normal)
        equalButton.titleLabel?.font = UIFont.systemFont(ofSize: 30)
        equalButton.addTarget(self, action: #selector(equalButtonTapped(_:)), for: .touchUpInside)
        view.addSubview(equalButton)
        
        // Criação do botão de limpar
        clearButton = UIButton(type: .system)
        clearButton.frame = CGRect(x: 20, y: 520, width: view.frame.width - 40, height: 60)
        clearButton.setTitle("C", for: .normal)
        clearButton.titleLabel?.font = UIFont.systemFont(ofSize: 30)
        clearButton.addTarget(self, action: #selector(clearButtonTapped(_:)), for: .touchUpInside)
        view.addSubview(clearButton)
    }
    
    // Função chamada quando um botão numérico é pressionado
    @objc func numberButtonTapped(_ sender: UIButton) {
        guard let numberString = sender.titleLabel?.text else { return }
        guard let number = Double(numberString) else { return }
        
        if currentOperator == "" {
            firstNumber = number
        } else {
            secondNumber = number
        }
        
        displayLabel.text = numberString
    }
    
    // Função chamada quando um botão de operação é pressionado
    @objc func operatorButtonTapped(_ sender: UIButton) {
        guard let operatorString = sender.titleLabel?.text else { return }
        
        currentOperator = operatorString
    }
    
    // Função chamada quando o botão de igual é pressionado
    @objc func equalButtonTapped(_ sender: UIButton) {
        var result: Double = 0
        
        switch currentOperator {
        case "+":
            result = firstNumber + secondNumber
        case "-":
            result = firstNumber - secondNumber
        case "*":
            result = firstNumber * secondNumber
        case "/":
            result = firstNumber / secondNumber
        default:
            break
        }
        
        displayLabel.text = String(result)
        
        // Reseta as variáveis
        firstNumber = 0
        secondNumber = 0
        currentOperator = ""
    }
    
    // Função chamada quando o botão de limpar é pressionado
    @objc func clearButtonTapped(_ sender: UIButton) {
        displayLabel.text = ""
        
        // Reseta as variáveis
        firstNumber = 0
        secondNumber = 0
        currentOperator = ""
    }
}
```

Esse código cria uma calculadora com interface gráfica em Swift. A interface consiste em um label de exibição, botões numéricos de 0 a 9, botões de operação (+, -, *, /), um botão de igual (=) e um botão de limpar (C).

A lógica do código é a seguinte:
1. Criação dos elementos da interface gráfica e configuração de suas posições.
2. Declaração das variáveis para armazenar os valores e a operação atual.
3. Implementação das funções para tratar os eventos de toque nos botões numéricos, de operação, igual e limpar.
4. Nas funções dos botões numéricos, o número correspondente ao botão é armazenado na variável apropriada (primeiro ou segundo número) e exibido no label de exibição.
5. Na função do botão de operação, o operador correspondente é armazenado na variável de operação atual.
6. Na função do botão de igual, o cálculo correspondente é realizado com base nos valores e operação atual, o resultado é exibido no label de exibição e as variáveis são resetadas.
7. Na função do botão de limpar, o label de exibição é limpo e as variáveis são resetadas.

Isso é apenas um exemplo de código complexo em Swift, que mostra um caso de uso comum. É importante ressaltar que existem muitas outras possibilidades de implementação de uma calculadora, assim como outras funcionalidades que podem ser adicionadas.