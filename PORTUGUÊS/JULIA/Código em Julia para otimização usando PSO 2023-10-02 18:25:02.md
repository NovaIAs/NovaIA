Claro! Aqui está um código em Julia que realiza uma otimização usando o algoritmo Particle Swarm Optimization (PSO), para encontrar o mínimo global de uma função multimodal:

```julia
# Definição da função de otimização
function rastrigin(x)
    sum(x .^ 2 .- 10 .* cos.(2 .* pi .* x)) .+ 10 .* length(x)
end

# Definição da classe de partícula
struct Particle
    position::Vector{Float64}
    velocity::Vector{Float64}
    pbest::Vector{Float64}
    pbest_value::Float64
end

# Função para inicializar a população de partículas
function initialize_particles(num_particles, num_dimensions, bounds)
    particles = Particle[]
    for _ in 1:num_particles
        position = [rand(bounds[i, 1]:bounds[i, 2]) for i in 1:num_dimensions]
        velocity = [rand(-bounds[i, 2]:bounds[i, 2]) for i in 1:num_dimensions]
        pbest = position
        pbest_value = rastrigin(position)
        push!(particles, Particle(position, velocity, pbest, pbest_value))
    end
    particles
end

# Função para atualizar a velocidade e posição de cada partícula
function update_particle(particle::Particle, gbest, c1, c2, w)
    r1 = rand()
    r2 = rand()
    particle.velocity = w .* particle.velocity .+ c1 .* r1 .* (particle.pbest .- particle.position) .+ c2 .* r2 .* (gbest .- particle.position)
    particle.position = particle.position .+ particle.velocity
end

# Função para encontrar o mínimo global usando PSO
function pso(num_particles, num_dimensions, bounds, max_iterations)
    particles = initialize_particles(num_particles, num_dimensions, bounds)
    gbest = particles[1].pbest
    gbest_value = particles[1].pbest_value
    
    for _ in 1:max_iterations
        for particle in particles
            if rastrigin(particle.position) < particle.pbest_value
                particle.pbest = particle.position
                particle.pbest_value = rastrigin(particle.position)
            end
            if rastrigin(particle.position) < gbest_value
                gbest = particle.position
                gbest_value = rastrigin(particle.position)
            end
            update_particle(particle, gbest, 2.0, 2.0, 0.9)
        end
    end
    
    gbest, gbest_value
end

# Parâmetros
num_particles = 50
num_dimensions = 10
bounds = [-5.12 5.12; -5.12 5.12; -5.12 5.12; -5.12 5.12; -5.12 5.12; -5.12 5.12; -5.12 5.12; -5.12 5.12; -5.12 5.12; -5.12 5.12]
max_iterations = 100

# Chamada da função PSO
gbest, gbest_value = pso(num_particles, num_dimensions, bounds, max_iterations)

# Resultado
println("Melhor solução encontrada: ", gbest)
println("Valor da função objetivo: ", gbest_value)
```

Esse código implementa o algoritmo PSO para encontrar o mínimo global da função de Rastrigin em um espaço multidimensional. O PSO é um algoritmo de otimização inspirado no comportamento de um enxame de partículas. Nesse código, cada partícula representa uma possível solução do problema e seu movimento é atualizado iterativamente com base em sua melhor posição encontrada (pbest) e na melhor posição global do enxame (gbest). A função de Rastrigin é uma função de teste com vários mínimos locais, o que torna o problema de otimização desafiador. O código retorna a melhor solução encontrada e o valor da função objetivo correspondente.