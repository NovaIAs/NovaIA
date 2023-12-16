Claro! Abaixo está um exemplo de um código em TCL que cria uma interface gráfica simples usando o pacote Tk. O código cria uma janela com um botão que, quando clicado, exibe uma caixa de diálogo com uma mensagem.

```tcl
# Importar o pacote Tk
package require Tk

# Criar a janela principal
wm title . "Exemplo de Interface Gráfica"
wm geometry . 400x300

# Criar um botão
button .button -text "Clique Aqui" -command exibir_mensagem
pack .button -side top -padx 10 -pady 10

# Função para exibir a mensagem
proc exibir_mensagem {} {
    set mensagem "Olá, Mundo!"
    tk_messageBox -message $mensagem -title "Mensagem"
}
```

Explicação do código:
- Na primeira linha, estamos importando o pacote Tk usando o comando `package require Tk`.
- Em seguida, definimos o título da janela principal usando o comando `wm title . "Exemplo de Interface Gráfica"`.
- O comando `wm geometry` define a geometria da janela principal, neste caso, 400x300 pixels.
- Em seguida, criamos um botão usando o comando `button .button -text "Clique Aqui" -command exibir_mensagem`. O botão recebe o nome ".button" e tem o texto "Clique Aqui". O parâmetro "-command" define a função a ser executada quando o botão for clicado.
- O comando `pack` é usado para organizar os widgets na janela. Neste caso, estamos empacotando o botão na parte superior da janela com um espaçamento horizontal e vertical de 10 pixels.
- A função `exibir_mensagem` é definida usando o comando `proc`. Dentro da função, definimos a variável `mensagem` como "Olá, Mundo!".
- Por fim, utilizamos o comando `tk_messageBox` para exibir uma caixa de diálogo com a mensagem definida na variável `mensagem`.

Esse é apenas um exemplo simples de código em TCL para criar uma interface gráfica básica. É importante mencionar que a linguagem TCL é conhecida por sua simplicidade e facilidade de uso. Você pode expandir esse exemplo e criar interfaces mais complexas, adicionando mais widgets e funcionalidades.