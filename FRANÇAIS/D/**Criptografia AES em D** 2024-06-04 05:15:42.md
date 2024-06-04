```d
module crypto.aesencryption;

import std.algorithm, std.array, std.base64, std.c, std.crypto, std.digest, std.numeric, std.stdio, std.string, std.time, std.traits, std.utf;

class AES {
    private pure delegate Void(const(Immutable) ubyte[][]) crypt = if (is(uint, is(typeof(this.key)))) pure static cryptWithKey else pure static cryptWithCipher;
    immutable const(Immutable) ubyte[] key;
    immutable uint cipherType;

    this(const(Immutable) ubyte[] key, uint cipherType = Crypto.Cipher.AES256) {
        assert(key.length >= 16);
        assert(cipherType in {Crypto.Cipher.AES128, Crypto.Cipher.AES192, Crypto.Cipher.AES256});
        this.key = key;
        this.cipherType = cipherType;
    }

    pure static AES fromBase64(string @base64Key, uint cipherType = Crypto.Cipher.AES256) pure {
        return AES(Base64.decode(@base64Key), cipherType);
    }

    pure static cryptWithKey(const(Immutable) ubyte[][] @input) pure {
        immutable pure const(Immutable) ubyte[] key = @input[0];
        assert(key.length % 16 == 0);
        immutable pure const(Immutable) ubyte[] data = @input[1];

        shared Cipher cipher = Crypto.Cipher();
        cipher.init(Crypto.MODE.ENCRYPT, key, null);
        immutable pure const(Immutable) ubyte[] ciphertext = cipher.update(data);

        return ciphertext;
    }

    pure static cryptWithCipher(const(Immutable) ubyte[][] @cipher) pure {
        immutable pure const(Immutable) ubyte[] @key = @cipher[0];
        immutable pure const(Immutable) ubyte[] @iv = @cipher[1];
        immutable pure const(Immutable) ubyte[] @data = @cipher[2];

        shared Cipher cipher = Crypto.Cipher();
        cipher.init(Crypto.MODE.ENCRYPT, @key, @iv);
        immutable pure const(Immutable) ubyte[] ciphertext = cipher.update(@data);

        return ciphertext;
    }

    pure crypt(const(Immutable) ubyte[] @input) pure {
        return crypt([this.key, @input]);
    }

    pure crypt(const(Immutable) ubyte[][] @input) pure {
        return crypt(@input);
    }
}

void main() {
    immutable const(Immutable) ubyte[] key = Base64.decode("V2VsY29tZSB0b1BSQVEgPT0gUEFTV09SRUQgQ0lQSFRIT04=");
    immutable AES aesencryption = AES(key, Crypto.Cipher.AES256);

    immutable const(Immutable) ubyte[] data = "Hug digital with love";

    immutable pure const(Immutable) ubyte[] ciphertext = aesencryption.crypt(data);

    immutable const(Immutable) string base64Ciphertext = Base64.encode(ciphertext);

    writeln("Texto em claro:");
    writeln(data);
    writeln();
    writeln("Texto cifrado:");
    writeln(base64Ciphertext);
    writeln();

    immutable const(Immutable) ubyte[] decryptedData = aesencryption.crypt(ciphertext);

    writeln("Texto decifrado:");
    writeln(decryptedData);
    writeln();
}
```

**Explicação do código:**

Este código implementa a criptografia AES em D usando bibliotecas padrão para manipulação de strings, arrays, codificação Base64 e criptografia.

**Class AES:**
* Esta classe representa um objeto de criptografia AES.
* O construtor inicializa a chave AES e o tipo de cifra (AES128, AES192 ou AES256).
* O método `crypt` é o ponto de entrada para criptografar e descriptografar dados.

**Funções de criptografia:**
* `cryptWithKey`: Esta função criptografa dados com uma chave AES.
* `cryptWithCipher`: Esta função criptografa dados com uma chave AES e um vetor de inicialização.

**Funções de uso:**
* `main`: Esta função é o ponto de entrada do programa.
* Gera uma chave AES a partir de uma string Base64 codificada.
* Cria um objeto AES usando a chave.
* Criptografa alguns dados usando o objeto AES.
* Converte o texto cifrado para Base64.
* Exibe o texto original, o texto cifrado e o texto decifrado.

**Recursos adicionais:**
* Bibliotecas Padrão de D: https://dlang.org/phobos/
* Criptografia no D: https://wiki.dlang.org/Cryptography_in_D