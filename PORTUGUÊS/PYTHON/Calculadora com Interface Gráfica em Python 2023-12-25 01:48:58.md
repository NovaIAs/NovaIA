Claro! Aqui está um código complexo em Python que envolve manipulação de dados, cálculos matemáticos e uma interface gráfica utilizando a biblioteca Tkinter. O código é dividido em várias partes para facilitar a compreensão. Vamos começar!

```python
# Importando as bibliotecas necessárias
from tkinter import *
from tkinter import messagebox

# Definindo a classe principal do programa
class Calculadora:
    def __init__(self, master):
        self.master = master
        self.master.title("Calculadora")

        # Criando a entrada de texto
        self.entry = Entry(master, font=("Arial", 20), justify=RIGHT)
        self.entry.grid(row=0, column=0, columnspan=4, padx=10, pady=10)

        # Criando os botões numéricos
        numeros = ["7", "8", "9", "4", "5", "6", "1", "2", "3", "0", ".", "C"]
        self.botoes = []
        for i in range(len(numeros)):
            self.botoes.append(Button(master, text=numeros[i], font=("Arial", 15), width=5, height=2))
            self.botoes[i].grid(row=(i//3)+1, column=i%3)

        # Criando os botões de operação
        operacoes = ["+", "-", "*", "/"]
        self.botoes_operacao = []
        for i in range(len(operacoes)):
            self.botoes_operacao.append(Button(master, text=operacoes[i], font=("Arial", 15), width=5, height=2))
            self.botoes_operacao[i].grid(row=i+1, column=3)

        # Criando o botão de igual
        self.botao_igual = Button(master, text="=", font=("Arial", 15), width=5, height=2)
        self.botao_igual.grid(row=4, column=2)

        # Definindo as ações dos botões
        for i in range(len(numeros)):
            self.botoes[i].configure(command=lambda button=self.botoes[i]: self.adicionar_numero(button))
        for i in range(len(operacoes)):
            self.botoes_operacao[i].configure(command=lambda button=self.botoes_operacao[i]: self.adicionar_operacao(button))
        self.botao_igual.configure(command=self.calcular_resultado)

        # Variáveis de controle
        self.primeiro_numero = ""
        self.operacao = ""
        self.resultado = ""

    # Função para adicionar números na entrada de texto
    def adicionar_numero(self, button):
        if button["text"] == ".":
            if "." not in self.entry.get():
                self.entry.insert(END, button["text"])
        else:
            self.entry.insert(END, button["text"])

    # Função para adicionar operações na entrada de texto
    def adicionar_operacao(self, button):
        self.primeiro_numero = self.entry.get()
        self.operacao = button["text"]
        self.entry.delete(0, END)

    # Função para calcular o resultado
    def calcular_resultado(self):
        try:
            segundo_numero = self.entry.get()
            self.resultado = eval(self.primeiro_numero + self.operacao + segundo_numero)
            self.entry.delete(0, END)
            self.entry.insert(END, str(self.resultado))
        except ZeroDivisionError:
            messagebox.showerror("Erro", "Divisão por zero não é permitida.")
        except Exception as e:
            messagebox.showerror("Erro", str(e))

# Função principal para executar o programa
if __name__ == "__main__":
    root = Tk()
    calculadora = Calculadora(root)
    root.mainloop()
```

Este código cria uma calculadora básica utilizando uma interface gráfica. A classe `Calculadora` é a classe principal do programa. No método `__init__`, a janela principal é criada e os elementos da calculadora, como a entrada de texto, os botões numéricos e os botões de operação, são configurados.

A classe possui métodos como `adicionar_numero`, `adicionar_operacao` e `calcular_resultado` para manipular os dados inseridos pelo usuário e realizar as operações matemáticas. A função `eval` é utilizada para calcular o resultado a partir dos números e da operação inseridos.

A parte final do código cria a janela principal do programa e inicia o loop principal de eventos utilizando o método `mainloop()`.

Espero que este código atenda às suas expectativas!