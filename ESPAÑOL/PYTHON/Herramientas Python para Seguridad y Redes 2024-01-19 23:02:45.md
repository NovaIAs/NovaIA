```python
# Importación de bibliotecas necesarias
import re
import os
import sys
import random
import string
import hashlib
import zipfile
import tarfile
import subprocess
import threading
import time
import socket
import struct
import binascii
import collections
import itertools
import functools
import operator
import math
import statistics
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats

# Definición de constantes
HOST = '127.0.0.1'
PORT = 5000
BUFFER_SIZE = 1024
DELIMITER = b'\n'

# Función para generar una contraseña segura
def generar_contraseña(longitud=16, caracteres=string.ascii_letters + string.digits + string.punctuation):
    return ''.join(random.choice(caracteres) for i in range(longitud))

# Función para cifrar una cadena de texto
def cifrar_cadena(cadena, llave):
    cifrado = hashlib.sha256(llave.encode('utf-8')).digest()
    return binascii.hexlify(bytes([b ^ c for b, c in zip(cadena.encode('utf-8'), itertools.cycle(cifrado))]))

# Función para descomprimir un archivo
def descomprimir_archivo(archivo_comprimido, ruta_destino):
    if archivo_comprimido.endswith('.zip'):
        with zipfile.ZipFile(archivo_comprimido, 'r') as zip_file:
            zip_file.extractall(ruta_destino)
    elif archivo_comprimido.endswith('.tar') or archivo_comprimido.endswith('.tar.gz'):
        with tarfile.open(archivo_comprimido, 'r') as tar_file:
            tar_file.extractall(ruta_destino)

# Función para iniciar un servidor TCP
def iniciar_servidor_tcp():
    # Crear un socket TCP
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Vincular el socket a la dirección IP y al puerto
    sock.bind((HOST, PORT))

    # Escuchar por conexiones entrantes
    sock.listen()

    # Aceptar la primera conexión entrante
    conn, addr = sock.accept()

    # Enviar un mensaje al cliente
    conn.send(b'Hola, mundo!\n')

    # Recibir un mensaje del cliente
    data = conn.recv(BUFFER_SIZE)

    # Cerrar la conexión
    conn.close()

# Función para iniciar un cliente TCP
def iniciar_cliente_tcp():
    # Crear un socket TCP
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Conectarse al servidor TCP
    sock.connect((HOST, PORT))

    # Enviar un mensaje al servidor
    sock.send(b'Hola, mundo!\n')

    # Recibir un mensaje del servidor
    data = sock.recv(BUFFER_SIZE)

    # Cerrar la conexión
    sock.close()

# Función para iniciar un servidor UDP
def iniciar_servidor_udp():
    # Crear un socket UDP
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

    # Vincular el socket a la dirección IP y al puerto
    sock.bind((HOST, PORT))

    while True:
        # Recibir un mensaje del cliente
        data, addr = sock.recvfrom(BUFFER_SIZE)

        # Enviar un mensaje al cliente
        sock.sendto(b'Hola, mundo!', addr)

# Función para iniciar un cliente UDP
def iniciar_cliente_udp():
    # Crear un socket UDP
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

    # Enviar un mensaje al servidor
    sock.sendto(b'Hola, mundo!', (HOST, PORT))

    # Recibir un mensaje del servidor
    data, addr = sock.recvfrom(BUFFER_SIZE)

# Función principal
if __name__ == '__main__':
    # Obtener la opción del usuario
    opcion = int(input('Elige una opción:\n1. Generar contraseña\n2. Cifrar cadena\n3. Descomprimir archivo\n4. Iniciar servidor TCP\n5. Iniciar cliente TCP\n6. Iniciar servidor UDP\n7. Iniciar cliente UDP\n'))

    # Realizar la acción correspondiente a la opción elegida
    if opcion == 1:
        longitud = int(input('Longitud de la contraseña: '))
        contraseña = generar_contraseña(longitud=longitud)
        print(f'Contraseña: {contraseña}')
    elif opcion == 2:
        cadena = input('Cadena a cifrar: ')
        llave = input('Llave: ')
        cifrado = cifrar_cadena(cadena=cadena, llave=llave)
        print(f'Cifrado: {cifrado}')
    elif opcion == 3:
        archivo_comprimido = input('Ruta del archivo comprimido: ')
        ruta_destino = input('Ruta de destino: ')
        descomprimir_archivo(archivo