Claro! Aqui está um código complexo em Python que utiliza a biblioteca Tkinter para criar uma interface gráfica. O código cria uma calculadora com uma interface intuitiva e funcional.

```python
from tkinter import *

# Função para realizar o cálculo
def calcular():
    try:
        # Obtém os valores digitados nos campos de entrada
        num1 = float(entrada1.get())
        num2 = float(entrada2.get())

        # Realiza o cálculo da operação selecionada
        if operacao.get() == "+":
            resultado = num1 + num2
        elif operacao.get() == "-":
            resultado = num1 - num2
        elif operacao.get() == "*":
            resultado = num1 * num2
        elif operacao.get() == "/":
            resultado = num1 / num2

        # Exibe o resultado na label correspondente
        label_resultado["text"] = f"Resultado: {resultado}"

    except ValueError:
        # Exibe uma mensagem de erro caso ocorra um erro de valor inválido
        label_resultado["text"] = "Erro: Valores inválidos!"

# Cria a janela principal
janela = Tk()
janela.title("Calculadora")
janela.geometry("300x200")

# Cria os widgets da interface
label_num1 = Label(janela, text="Número 1:")
label_num1.pack()

entrada1 = Entry(janela)
entrada1.pack()

label_num2 = Label(janela, text="Número 2:")
label_num2.pack()

entrada2 = Entry(janela)
entrada2.pack()

label_operacao = Label(janela, text="Operação:")
label_operacao.pack()

operacao = Entry(janela)
operacao.pack()

botao_calcular = Button(janela, text="Calcular", command=calcular)
botao_calcular.pack()

label_resultado = Label(janela, text="")
label_resultado.pack()

# Inicia o loop da interface gráfica
janela.mainloop()
```

Explicação do código:
1. Importamos a biblioteca Tkinter para criar a interface gráfica.
2. Definimos uma função chamada `calcular()` que é chamada quando o botão "Calcular" é clicado. Essa função realiza o cálculo com base nos valores digitados nos campos de entrada e exibe o resultado em uma label.
3. Criamos a janela principal utilizando a classe `Tk()` e definimos seu título e tamanho.
4. Criamos os widgets da interface, como labels, campos de entrada e um botão "Calcular".
5. Ligamos o botão "Calcular" à função `calcular()` usando o parâmetro `command`.
6. Criamos uma label vazia para exibir o resultado do cálculo.
7. Iniciamos o loop da interface gráfica utilizando o método `mainloop()`, que fica aguardando eventos do usuário.

Esse código é uma implementação básica de uma calculadora em Python utilizando uma interface gráfica. Você pode expandir e personalizar esse código adicionando mais operações matemáticas, melhorando a validação dos valores de entrada ou adicionando novos recursos à calculadora.