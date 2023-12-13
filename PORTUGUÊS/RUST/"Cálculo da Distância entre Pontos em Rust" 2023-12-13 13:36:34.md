Claro! Aqui está um código complexo em Rust para você:

```rust
// Definindo uma estrutura de dados para representar um ponto no espaço
struct Ponto {
    x: f64,
    y: f64,
    z: f64,
}

// Função que calcula a distância entre dois pontos
fn calcular_distancia(p1: &Ponto, p2: &Ponto) -> f64 {
    let dx = p2.x - p1.x;
    let dy = p2.y - p1.y;
    let dz = p2.z - p1.z;
    
    (dx.powi(2) + dy.powi(2) + dz.powi(2)).sqrt()
}

// Função principal
fn main() {
    // Criando dois pontos
    let ponto1 = Ponto { x: 1.0, y: 2.0, z: 3.0 };
    let ponto2 = Ponto { x: 4.0, y: 5.0, z: 6.0 };
    
    // Calculando a distância entre os pontos
    let distancia = calcular_distancia(&ponto1, &ponto2);
    
    // Imprimindo o resultado
    println!("A distância entre os pontos é: {}", distancia);
}
```

Neste código, criamos uma estrutura de dados chamada `Ponto`, que representa um ponto no espaço com coordenadas `x`, `y` e `z`. Em seguida, temos uma função chamada `calcular_distancia` que recebe dois pontos como argumentos e calcula a distância euclidiana entre eles usando a fórmula `sqrt(dx^2 + dy^2 + dz^2)`. 

Na função principal `main`, criamos dois pontos, `ponto1` e `ponto2`, e chamamos a função `calcular_distancia` passando esses pontos como argumentos. O resultado é armazenado na variável `distancia` e, em seguida, é impresso na tela usando a função `println!`.

Espero que este código atenda às suas expectativas!