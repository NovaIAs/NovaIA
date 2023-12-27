Claro! Aqui está um exemplo de um código CSS complexo, utilizando seletores avançados, pseudo-classes e propriedades personalizadas. O código abaixo cria um efeito de animação de transição suave para elementos de uma lista, aplicando diferentes estilos conforme o usuário interage com eles:

```css
/* Definindo uma propriedade personalizada */
:root {
  --cor-destaque: #FFC107;
}

/* Estilizando a lista */
.lista {
  list-style: none;
  padding: 0;
  margin: 0;
}

/* Estilizando cada item da lista */
.lista li {
  position: relative;
  padding: 20px;
  background-color: #f5f5f5;
  border-radius: 10px;
  margin-bottom: 10px;
  cursor: pointer;
  transition: background-color 0.3s ease;
}

/* Mudando o estilo quando o usuário passa o mouse sobre um item */
.lista li:hover {
  background-color: var(--cor-destaque);
  color: white;
}

/* Mudando o estilo quando o usuário clica em um item */
.lista li:active {
  transform: scale(0.95);
}

/* Mudando o estilo quando um item está selecionado */
.lista li.selected {
  background-color: var(--cor-destaque);
  color: white;
}

/* Mudando o estilo do primeiro item */
.lista li:first-child {
  border-top-left-radius: 0;
  border-top-right-radius: 0;
}

/* Mudando o estilo do último item */
.lista li:last-child {
  border-bottom-left-radius: 0;
  border-bottom-right-radius: 0;
}

/* Mudando o estilo de um item quando o usuário passa o mouse sobre o item anterior */
.lista li:hover ~ li.selected {
  background-color: #e0e0e0;
}

/* Mudando o estilo de um item quando o usuário passa o mouse sobre o item posterior */
.lista li.selected ~ li:hover {
  background-color: #e0e0e0;
}

/* Mudando o estilo de um item quando o usuário passa o mouse sobre o item anterior e posterior */
.lista li.selected ~ li:hover ~ li.selected {
  background-color: var(--cor-destaque);
}

/* Mudando o estilo de um item quando o usuário passa o mouse sobre o item posterior e anterior */
.lista li:hover ~ li.selected ~ li:hover {
  background-color: var(--cor-destaque);
}
```

Neste código, temos uma lista com elementos `<li>`. Cada item é estilizado com um background cinza claro e um padding de 20px. Quando o usuário passa o mouse sobre um item, a cor de fundo muda para um amarelo alaranjado (`--cor-destaque`) e a cor do texto é alterada para branco. Ao clicar em um item, ele é levemente reduzido de tamanho.

Além disso, há estilos adicionais quando um item está selecionado ou quando o usuário passa o mouse sobre os itens anteriores ou posteriores ao selecionado. Essas regras adicionais permitem uma transição de cores suave entre os itens.

Espero que este código atenda às suas necessidades!