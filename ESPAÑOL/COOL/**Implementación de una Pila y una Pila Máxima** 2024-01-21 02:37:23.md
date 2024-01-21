```cool

módulo Principal {

    constante máximo = 10;

    clase Pila {

        atributos {
            elementos : lista de entero;
            tamaño : entero;
        }

        métodos {

            inicializar() : Pila {
                tamaño := 0;
                elementos := lista_vacía();
            }

            apilar(elemento : entero) : vacío {
                tamaño := tamaño + 1;
                elementos := elemento || elementos;
            }

            desapilar() : entero {
                si tamaño > 0 entonces
                    tamaño := tamaño - 1;
                    elementos := elementos[2..];
                    devolver elementos[1];
                si_no
                    devolver 0;
                fi;
            }

            tamaño() : entero {
                devolver tamaño;
            }

            es_vacía() : booleano {
                devolver tamaño = 0;
            }

            imprimir() : vacío {
                si tamaño > 0 entonces
                    escribir(elementos[1]);
                    imprimir(elementos[2..]);
                si_no
                    escribir("Pila vacía");
                fi;
            }
        }
    }

    clase PilaMáxima : Pila {

        atributos {
            tope : entero;
        }

        métodos {

            inicializar() : PilaMáxima {
                tope := 0;
                heredar inicializar();
            }

            apilar(elemento : entero) : vacío {
                si elemento > tope entonces
                    tope := elemento;
                fi;
                heredar apilar(elemento);
            }

            tope() : entero {
                devolver tope;
            }
        }
    }

    clase Prueba {

        métodos {

            ejecutar() : vacío {
                pila1 := Pila.inicializar();
                pila2 := PilaMáxima.inicializar();

                para i desde 1 hasta máximo hacer
                    pila1.apilar(i);
                    pila2.apilar(i);
                fin_para;

                escribir("Pila 1:");
                pila1.imprimir();

                escribir("Pila 2:");
                pila2.imprimir();

                escribir("Tope de la Pila 2:", pila2.tope());
            }
        }
    }

    prueba := Prueba.inicializar();
    prueba.ejecutar();
}

```