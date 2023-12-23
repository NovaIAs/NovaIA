Claro! Aqui está um exemplo de código em Java que cria uma calculadora com uma interface gráfica simples:

```java
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class Calculadora extends JFrame implements ActionListener {
    private JTextField textField;
    private JButton button0, button1, button2, button3, button4, button5, button6, button7, button8, button9;
    private JButton buttonSoma, buttonSubtracao, buttonMultiplicacao, buttonDivisao, buttonIgual, buttonLimpar;

    private double numero1, numero2;
    private String operador;

    public Calculadora() {
        setTitle("Calculadora");
        setSize(300, 400);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(new BorderLayout());

        textField = new JTextField();
        add(textField, BorderLayout.NORTH);

        JPanel panel = new JPanel();
        panel.setLayout(new GridLayout(4, 4));

        button7 = new JButton("7");
        panel.add(button7);
        button7.addActionListener(this);

        button8 = new JButton("8");
        panel.add(button8);
        button8.addActionListener(this);

        button9 = new JButton("9");
        panel.add(button9);
        button9.addActionListener(this);

        buttonDivisao = new JButton("/");
        panel.add(buttonDivisao);
        buttonDivisao.addActionListener(this);

        button4 = new JButton("4");
        panel.add(button4);
        button4.addActionListener(this);

        button5 = new JButton("5");
        panel.add(button5);
        button5.addActionListener(this);

        button6 = new JButton("6");
        panel.add(button6);
        button6.addActionListener(this);

        buttonMultiplicacao = new JButton("*");
        panel.add(buttonMultiplicacao);
        buttonMultiplicacao.addActionListener(this);

        button1 = new JButton("1");
        panel.add(button1);
        button1.addActionListener(this);

        button2 = new JButton("2");
        panel.add(button2);
        button2.addActionListener(this);

        button3 = new JButton("3");
        panel.add(button3);
        button3.addActionListener(this);

        buttonSubtracao = new JButton("-");
        panel.add(buttonSubtracao);
        buttonSubtracao.addActionListener(this);

        buttonLimpar = new JButton("C");
        panel.add(buttonLimpar);
        buttonLimpar.addActionListener(this);

        button0 = new JButton("0");
        panel.add(button0);
        button0.addActionListener(this);

        buttonIgual = new JButton("=");
        panel.add(buttonIgual);
        buttonIgual.addActionListener(this);

        buttonSoma = new JButton("+");
        panel.add(buttonSoma);
        buttonSoma.addActionListener(this);

        add(panel, BorderLayout.CENTER);

        setVisible(true);
    }

    public void actionPerformed(ActionEvent e) {
        String valor = ((JButton) e.getSource()).getText();

        if (valor.equals("+") || valor.equals("-") || valor.equals("*") || valor.equals("/")) {
            operador = valor;
            numero1 = Double.parseDouble(textField.getText());
            textField.setText("");
        } else if (valor.equals("=")) {
            numero2 = Double.parseDouble(textField.getText());
            double resultado = 0;

            switch (operador) {
                case "+":
                    resultado = numero1 + numero2;
                    break;
                case "-":
                    resultado = numero1 - numero2;
                    break;
                case "*":
                    resultado = numero1 * numero2;
                    break;
                case "/":
                    resultado = numero1 / numero2;
                    break;
            }

            textField.setText(String.valueOf(resultado));
        } else if (valor.equals("C")) {
            textField.setText("");
        } else {
            textField.setText(textField.getText() + valor);
        }
    }

    public static void main(String[] args) {
        new Calculadora();
    }
}
```

Este código cria uma interface gráfica de uma calculadora básica utilizando a biblioteca Swing do Java. A calculadora possui os botões numéricos de 0 a 9, os operadores matemáticos (+, -, *, /) e os botões de igual (=) e limpar (C).

Quando um número é pressionado, ele é exibido no campo de texto da calculadora. Quando um operador é pressionado, o número atual é salvo e o campo de texto é limpo para o próximo número. Quando o botão de igual é pressionado, a operação matemática é realizada e o resultado é exibido no campo de texto. Já o botão de limpar, limpa o campo de texto.

O código utiliza o conceito de eventos para capturar as ações do usuário nos botões e realizar as operações correspondentes. A interface gráfica é construída utilizando layouts e componentes do Swing, como JFrame, JTextField e JButton.

Espero que isso te ajude a entender um pouco mais sobre programação em Java! Se tiver mais alguma dúvida, estou aqui para ajudar.