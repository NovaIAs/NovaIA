```perl
# Programa para simular un sistema de archivos en memoria

use strict;
use warnings;

# Definición de la estructura de un nodo del sistema de archivos
package Nodo {
    sub new {
        my ($class, %args) = @_;
        bless {
            nombre => $args{nombre},
            tipo => $args{tipo},
            contenido => $args{contenido} || "",
            hijos => [],
            padre => $args{padre} || undef,
        }, $class;
    }

    sub nombre {
        my ($self) = @_;
        return $self->{nombre};
    }

    sub tipo {
        my ($self) = @_;
        return $self->{tipo};
    }

    sub contenido {
        my ($self) = @_;
        return $self->{contenido};
    }

    sub hijos {
        my ($self) = @_;
        return $self->{hijos};
    }

    sub padre {
        my ($self) = @_;
        return $self->{padre};
    }

    sub agregar_hijo {
        my ($self, $hijo) = @_;
        push @{$self->{hijos}}, $hijo;
    }

    sub eliminar_hijo {
        my ($self, $hijo) = @_;
        my @hijos = @{$self->{hijos}};
        @hijos = grep { $_ != $hijo } @hijos;
        $self->{hijos} = \@hijos;
    }
};

# Definición de la estructura del sistema de archivos
package SistemaDeArchivos {
    sub new {
        my ($class) = @_;
        bless {
            raiz => Nodo->new(nombre => "/", tipo => "directorio"),
            nodos => {},
        }, $class;
    }

    sub raiz {
        my ($self) = @_;
        return $self->{raiz};
    }

    sub nodos {
        my ($self) = @_;
        return $self->{nodos};
    }

    sub agregar_nodo {
        my ($self, $nodo) = @_;
        $self->{nodos}{$nodo->nombre()} = $nodo;
    }

    sub eliminar_nodo {
        my ($self, $nodo) = @_;
        delete $self->{nodos}{$nodo->nombre()};
    }

    sub buscar_nodo {
        my ($self, $nombre) = @_;
        return $self->{nodos}{$nombre};
    }
};

# Crear un nuevo sistema de archivos
my $sistema_de_archivos = SistemaDeArchivos->new();

# Crear la estructura de directorios
$sistema_de_archivos->agregar_nodo(Nodo->new(nombre => "/bin", tipo => "directorio", padre => $sistema_de_archivos->raiz()));
$sistema_de_archivos->agregar_nodo(Nodo->new(nombre => "/bin/bash", tipo => "archivo", padre => $sistema_de_archivos->buscar_nodo("/bin")));
$sistema_de_archivos->agregar_nodo(Nodo->new(nombre => "/bin/ls", tipo => "archivo", padre => $sistema_de_archivos->buscar_nodo("/bin")));
$sistema_de_archivos->agregar_nodo(Nodo->new(nombre => "/etc", tipo => "directorio", padre => $sistema_de_archivos->raiz()));
$sistema_de_archivos->agregar_nodo(Nodo->new(nombre => "/etc/passwd", tipo => "archivo", padre => $sistema_de_archivos->buscar_nodo("/etc")));
$sistema_de_archivos->agregar_nodo(Nodo->new(nombre => "/home", tipo => "directorio", padre => $sistema_de_archivos->raiz()));
$sistema_de_archivos->agregar_nodo(Nodo->new(nombre => "/home/usuario1", tipo => "directorio", padre => $sistema_de_archivos->buscar_nodo("/home")));
$sistema_de_archivos->agregar_nodo(Nodo->new(nombre => "/home/usuario1/archivo1", tipo => "archivo", padre => $sistema_de_archivos->buscar_nodo("/home/usuario1")));

# Imprimir la estructura del sistema de archivos
print "Estructura del sistema de archivos:\n";
print_nodos($sistema_de_archivos->raiz());

sub print_nodos {
    my ($nodo) = @_;
    print "Nodo: $nodo->{nombre}, tipo: $nodo->{tipo}, contenido: $nodo->{contenido}\n";
    foreach my $hijo (@{$nodo->{hijos}}) {
        print_nodos($hijo);
    }
}
```

Este programa simula un sistema de archivos en memoria. Define una estructura de datos para representar los nodos del sistema de archivos y una estructura para representar el sistema de archivos en su conjunto. El programa permite crear, eliminar y buscar nodos en el sistema de archivos, así como imprimir la estructura del sistema de archivos.

El programa es complejo porque define una estructura de datos personalizada para representar los nodos del sistema de archivos y otra estructura de datos para representar el sistema de archivos en su conjunto. También implementa algoritmos para crear, eliminar y buscar nodos en el sistema de archivos, así como para imprimir la estructura del sistema de archivos.