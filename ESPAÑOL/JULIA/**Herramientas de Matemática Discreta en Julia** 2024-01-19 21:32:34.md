```julia
# Definición de una función para calcular el factorial de un número.
factorial(n) = if n == 0
                 1
             else
                 n * factorial(n-1)
             end

# Definición de una función para generar todos los números primos hasta un límite.
primos(limite) = [2, 3]
              for i in 5:limite step 2
                  es_primo = true
                  for j in primos
                      if i % j == 0
                          es_primo = false
                          break
                      end
                  end
                  if es_primo
                      push!(primos, i)
                  end
              end

# Definición de una función para calcular el máximo común divisor de dos números.
mcd(a, b) = if b == 0
              a
           else
              mcd(b, a % b)
           end

# Definición de una función para calcular el mínimo común múltiplo de dos números.
mcm(a, b) = a * b / mcd(a, b)

# Definición de una función para generar todas las permutaciones de una lista.
permutaciones(lista) = if length(lista) == 1
                          [lista]
                      else
                          permutaciones_iterativas(lista)
                      end

function permutaciones_iterativas(lista)
    permutaciones = [lista[1]]
    for i in 2:length(lista)
        nueva_permutación = []
        for j in 1:length(lista)
            if j == i
                continue
            end
            for k in 1:length(lista)
                if k == i
                    continue
                end
                push!(nueva_permutación, lista[j], lista[i], lista[k])
            end
        end
        permutaciones = [permutaciones; nueva_permutación]
    end
    return permutaciones
end

# Definición de una función para generar todas las combinaciones de una lista.
combinaciones(lista, k) = if k == 1
                              lista
                          else
                              combinaciones_iterativas(lista, k)
                          end

function combinaciones_iterativas(lista, k)
    combinaciones = [[]]
    for i in 1:length(lista)
        nueva_combinación = []
        for j in 1:length(lista)
            if j == i
                continue
            end
            for l in 1:k-1
                push!(nueva_combinación, lista[i], lista[j])
            end
        end
        combinaciones = [combinaciones; nueva_combinación]
    end
    return combinaciones
end

# Definición de una función para generar todas las particiones de un número.
particiones(n) = if n == 0
                     [[]]
                 else
                     particiones_iterativas(n)
                 end

function particiones_iterativas(n)
    particiones = [[n]]
    for i in 1:n-1
        nueva_partición = []
        for j in 1:i
            for k in 1:n-i
                push!(nueva_partición, j, k)
            end
        end
        particiones = [particiones; nueva_partición]
    end
    return particiones
end

```