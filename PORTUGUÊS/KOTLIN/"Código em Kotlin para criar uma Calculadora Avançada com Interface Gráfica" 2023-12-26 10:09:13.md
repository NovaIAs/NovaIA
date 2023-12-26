Com certeza! Aqui está um código complexo em Kotlin que utiliza vários recursos da linguagem para criar uma calculadora avançada com uma interface gráfica:

```kotlin
import javax.swing.*
import java.awt.*
import java.awt.event.*

fun main() {
    val frame = JFrame("Calculadora")
    frame.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
    frame.layout = BorderLayout()

    val display = JTextField()
    display.horizontalAlignment = SwingConstants.RIGHT

    val panel = JPanel(GridBagLayout())
    val constraints = GridBagConstraints()

    constraints.fill = GridBagConstraints.BOTH
    constraints.weightx = 1.0
    constraints.weighty = 1.0

    val buttons = arrayOf(
        arrayOf("7", "8", "9", "/"),
        arrayOf("4", "5", "6", "*"),
        arrayOf("1", "2", "3", "-"),
        arrayOf("0", ".", "=", "+")
    )

    for (i in buttons.indices) {
        constraints.gridy = i

        for (j in buttons[i].indices) {
            constraints.gridx = j

            val button = JButton(buttons[i][j])
            button.margin = Insets(0, 0, 0, 0)

            button.addActionListener(object : ActionListener {
                override fun actionPerformed(e: ActionEvent) {
                    val text = button.text
                    if (text == "=") {
                        val result = evaluateExpression(display.text)
                        display.text = result.toString()
                    } else {
                        display.text += text
                    }
                }
            })

            panel.add(button, constraints)
        }
    }

    frame.add(display, BorderLayout.NORTH)
    frame.add(panel, BorderLayout.CENTER)

    frame.pack()
    frame.isVisible = true
}

fun evaluateExpression(expression: String): Double {
    val engine = ScriptEngineManager().getEngineByName("JavaScript")
    
    try {
        val result = engine.eval(expression)
        return result.toString().toDouble()
    } catch (e: ScriptException) {
        e.printStackTrace()
    }
    
    return 0.0
}
```

Este código cria uma interface gráfica de uma calculadora usando o pacote `javax.swing`. Ele possui um `JTextField` para exibir a expressão e o resultado, e um `JPanel` para os botões das operações aritméticas.

Os botões são criados usando um loop e adicionados ao painel usando o `GridBagLayout`. Cada botão é associado a um `ActionListener` que executa a lógica de adicionar os números e operadores na expressão e calcular o resultado quando o botão de igual é pressionado.

A função `evaluateExpression` usa o mecanismo de avaliação de expressões JavaScript fornecido pelo Kotlin para calcular o resultado da expressão. Ela retorna o resultado como um `Double`.

Espero que este código atenda às suas expectativas!