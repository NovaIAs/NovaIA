Claro! Aqui está um código complexo em TypeScript que cria uma classe de gerenciamento de usuários com autenticação e funções de CRUD (criar, ler, atualizar, excluir):

```typescript
interface Usuario {
  id: number;
  nome: string;
  email: string;
  senha: string;
}

class GerenciadorUsuarios {
  private usuarios: Usuario[];

  constructor() {
    this.usuarios = [];
  }

  public criarUsuario(nome: string, email: string, senha: string) {
    const novoId = this.usuarios.length + 1;
    const novoUsuario: Usuario = {
      id: novoId,
      nome: nome,
      email: email,
      senha: senha,
    };
    this.usuarios.push(novoUsuario);
    console.log(`Usuário "${nome}" criado com sucesso.`);
  }

  public autenticarUsuario(email: string, senha: string): Usuario | null {
    const usuario = this.usuarios.find((u) => u.email === email && u.senha === senha);
    if (usuario) {
      console.log(`Usuário "${usuario.nome}" autenticado.`);
      return usuario;
    } else {
      console.log(`Falha na autenticação. Verifique o e-mail e a senha.`);
      return null;
    }
  }

  public lerUsuario(id: number): Usuario | null {
    const usuario = this.usuarios.find((u) => u.id === id);
    if (usuario) {
      console.log(`Usuário encontrado: ${usuario.nome}`);
      return usuario;
    } else {
      console.log(`Usuário com ID ${id} não encontrado.`);
      return null;
    }
  }

  public atualizarUsuario(id: number, novasInformacoes: Partial<Usuario>) {
    const usuarioIndex = this.usuarios.findIndex((u) => u.id === id);
    if (usuarioIndex !== -1) {
      const usuarioAtualizado: Usuario = { ...this.usuarios[usuarioIndex], ...novasInformacoes };
      this.usuarios[usuarioIndex] = usuarioAtualizado;
      console.log(`Usuário ${usuarioAtualizado.nome} atualizado com sucesso.`);
    } else {
      console.log(`Usuário com ID ${id} não encontrado.`);
    }
  }

  public excluirUsuario(id: number) {
    const usuarioIndex = this.usuarios.findIndex((u) => u.id === id);
    if (usuarioIndex !== -1) {
      const usuarioRemovido = this.usuarios.splice(usuarioIndex, 1)[0];
      console.log(`Usuário ${usuarioRemovido.nome} removido com sucesso.`);
    } else {
      console.log(`Usuário com ID ${id} não encontrado.`);
    }
  }
}

// Exemplo de uso da classe GerenciadorUsuarios

const gerenciador = new GerenciadorUsuarios();

gerenciador.criarUsuario("João", "joao@example.com", "senha123");
gerenciador.criarUsuario("Maria", "maria@example.com", "abcd1234");

const usuarioAutenticado = gerenciador.autenticarUsuario("joao@example.com", "senha123");
if (usuarioAutenticado) {
  gerenciador.atualizarUsuario(usuarioAutenticado.id, { nome: "João Silva" });
  gerenciador.excluirUsuario(usuarioAutenticado.id);
}
```

Neste código, temos a classe `GerenciadorUsuarios` que possui métodos para criar, autenticar, ler, atualizar e excluir usuários. A classe mantém uma lista de usuários em memória. Cada usuário possui um ID, nome, e-mail e senha.

Ao criar uma instância do `GerenciadorUsuarios`, você pode chamar os métodos para realizar todas as operações necessárias. No exemplo de uso fornecido, criamos dois usuários, autenticamos o primeiro usuário, atualizamos seu nome para "João Silva" e, em seguida, excluímos o usuário autenticado.

Espero que este código atenda às suas necessidades!