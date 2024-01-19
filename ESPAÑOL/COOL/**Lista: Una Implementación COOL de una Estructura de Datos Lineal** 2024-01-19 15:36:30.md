```cool
-- Definición de una clase `Lista`
class Lista {
    -- Constructor de la clase `Lista`
    new(tam : Integer) : Lista;

    -- Método `append` para añadir un elemento a la lista
    append(elem : Integer) : Lista;

    -- Método `remove` para eliminar un elemento de la lista
    remove(elem : Integer) : Lista;

    -- Método `length` para obtener la longitud de la lista
    length() : Integer;

    -- Método `get` para obtener un elemento de la lista
    get(index : Integer) : Integer;

    -- Método `set` para establecer un elemento de la lista
    set(index : Integer, elem : Integer) : Lista;

    -- Método `sort` para ordenar la lista
    sort() : Lista;

    -- Método `reverse` para invertir la lista
    reverse() : Lista;

    -- Método `clear` para limpiar la lista
    clear() : Lista;

    -- Método `print` para imprimir la lista
    print() : String;
};

-- Implementación de la clase `Lista`
class Lista {
    -- Constructor de la clase `Lista`
    new(tam : Integer) : Lista {
        var i : Integer <- tam;
        var l : Lista <- this;
        while (i > 0) {
            l <- self.append(0);
            i <- i - 1;
        };
        return l;
    };

    -- Método `append` para añadir un elemento a la lista
    append(elem : Integer) : Lista {
        var l : Lista <- this;
        if (l.length() == 0) {
            l <- new Lista(1);
            l.set(0, elem);
        } else {
            var i : Integer <- l.length();
            while (i > 0) {
                var temp : Integer <- l.get(i - 1);
                l.set(i, temp);
                i <- i - 1;
            };
            l.set(0, elem);
        };
        return l;
    };

    -- Método `remove` para eliminar un elemento de la lista
    remove(elem : Integer) : Lista {
        var l : Lista <- this;
        var i : Integer <- l.length();
        while (i > 0) {
            var temp : Integer <- l.get(i - 1);
            if (temp == elem) {
                l <- self.removeAt(i - 1);
                break;
            };
            i <- i - 1;
        };
        return l;
    };

    -- Método `length` para obtener la longitud de la lista
    length() : Integer {
        var l : Lista <- this;
        var i : Integer <- 0;
        while (l.get(i) != 0) {
            i <- i + 1;
        };
        return i;
    };

    -- Método `get` para obtener un elemento de la lista
    get(index : Integer) : Integer {
        var l : Lista <- this;
        return l.get(index);
    };

    -- Método `set` para establecer un elemento de la lista
    set(index : Integer, elem : Integer) : Lista {
        var l : Lista <- this;
        l.set(index, elem);
        return l;
    };

    -- Método `sort` para ordenar la lista
    sort() : Lista {
        var l : Lista <- this;
        var i : Integer <- l.length();
        while (i > 0) {
            var j : Integer <- i - 1;
            while (j >= 0) {
                if (l.get(j) > l.get(j + 1)) {
                    var temp : Integer <- l.get(j);
                    l.set(j, l.get(j + 1));
                    l.set(j + 1, temp);
                };
                j <- j - 1;
            };
            i <- i - 1;
        };
        return l;
    };

    -- Método `reverse` para invertir la lista
    reverse() : Lista {
        var l : Lista <- this;
        var i : Integer <- 0;
        var j : Integer <- l.length() - 1;
        while (i < j) {
            var temp : Integer <- l.get(i);
            l.set(i, l.get(j));