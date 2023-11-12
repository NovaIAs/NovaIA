Claro! Aqui está um código CSS complexo que utiliza diferentes propriedades para estilizar um menu suspenso personalizado:

```css
/* Definindo a estilização do menu */
.menu {
  position: relative;
  display: inline-block;
  line-height: 40px;
  font-family: Arial, sans-serif;
  font-size: 14px;
  color: #333;
}

.menu:hover .submenu {
  display: block;
}

/* Definindo a estilização do item de menu */
.menu-item {
  position: relative;
  display: inline-block;
  padding: 0 10px;
  cursor: pointer;
}

.menu-item:hover {
  color: #ff6600;
}

/* Definindo a estilização do submenu */
.submenu {
  position: absolute;
  display: none;
  top: 100%;
  left: 0;
  min-width: 200px;
  background-color: #f9f9f9;
  border: 1px solid #ccc;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.submenu-item {
  display: block;
  padding: 10px;
  color: #333;
  transition: background-color 0.3s ease;
}

.submenu-item:hover {
  background-color: #ff6600;
  color: #fff;
}

/* Definindo estilização adicional para o último item do submenu */
.submenu-item:last-child {
  border-bottom: none;
}

/* Definindo estilização para seta do submenu */
.submenu-arrow {
  position: absolute;
  top: 50%;
  right: 10px;
  margin-top: -3px;
  width: 0;
  height: 0;
  border-style: solid;
  border-width: 5px 5px 0 5px;
  border-color: #333 transparent transparent transparent;
  transition: border-color 0.3s ease;
}

.menu-item:hover .submenu-arrow {
  border-color: #ff6600 transparent transparent transparent;
}
```

Neste código, criamos uma estilização para um menu suspenso. Cada item de menu é um elemento ".menu-item". Quando o usuário passa o mouse sobre o item de menu, o submenu correspondente é exibido. A estilização do submenu é definida com as classes ".submenu" e ".submenu-item". A seta do submenu é estilizada utilizando a classe ".submenu-arrow". 

Espero que esse código atenda às suas expectativas! Lembre-se de que você pode personalizá-lo e adaptá-lo de acordo com as suas necessidades.