```lua
-- Módulo para manejar matrices en Lua
local M = {}

-- Crea una matriz vacía de tamaño mxn
function M.crear(m, n)
  local A = {}
  for i = 1, m do
    A[i] = {}
    for j = 1, n do
      A[i][j] = 0
    end
  end
  return A
end

-- Muestra la matriz en la consola
function M.mostrar(A)
  for i, fila in ipairs(A) do
    print(table.concat(fila, "\t"))
  end
end

-- Suma dos matrices de igual tamaño
function M.suma(A, B)
  local C = M.crear(#A, #A[1])
  for i = 1, #A do
    for j = 1, #A[1] do
      C[i][j] = A[i][j] + B[i][j]
    end
  end
  return C
end

-- Resta dos matrices de igual tamaño
function M.resta(A, B)
  local C = M.crear(#A, #A[1])
  for i = 1, #A do
    for j = 1, #A[1] do
      C[i][j] = A[i][j] - B[i][j]
    end
  end
  return C
end

-- Multiplica dos matrices de tamaño compatible
function M.multiplicar(A, B)
  if #A[1] ~= #B then
    error("Las matrices no son compatibles para la multiplicación")
  end

  local C = M.crear(#A, #B[1])
  for i = 1, #A do
    for j = 1, #B[1] do
      for k = 1, #A[1] do
        C[i][j] = C[i][j] + A[i][k] * B[k][j]
      end
    end
  end
  return C
end

-- Traspone una matriz (cambia filas por columnas)
function M.trasponer(A)
  local B = M.crear(#A[1], #A)
  for i = 1, #A do
    for j = 1, #A[1] do
      B[j][i] = A[i][j]
    end
  end
  return B
end

-- Inversa una matriz cuadrada (si es invertible)
function M.inversa(A)
  if #A ~= #A[1] then
    error("La matriz no es cuadrada")
  end

  -- Copia la matriz A
  local B = M.crear(#A, #A[1])
  for i = 1, #A do
    for j = 1, #A[1] do
      B[i][j] = A[i][j]
    end
  end

  -- Crea la matriz identidad
  local I = M.crear(#A, #A[1])
  for i = 1, #I do
    I[i][i] = 1
  end

  -- Realiza la reducción a forma escalonada reducida
  for i = 1, #A do
    -- Encuentra el pivote en la columna actual
    local pivot = 0
    for j = i, #A do
      if B[j][i] ~= 0 then
        pivot = j
        break
      end
    end

    -- Si no se encontró un pivote, la matriz no es invertible
    if pivot == 0 then
      error("La matriz no es invertible")
    end

    -- Intercambia la fila actual con la fila del pivote
    for j = 1, #A[1] do
      B[i][j], B[pivot][j] = B[pivot][j], B[i][j]
      I[i][j], I[pivot][j] = I[pivot][j], I[i][j]
    end

    -- Divide la fila actual por el pivote
    local divisor = B[i][i]
    for j = 1, #A[1] do
      B[i][j] = B[i][j] / divisor
      I[i][j] = I[i][j] / divisor
    end

    -- Resta las filas correspondientes para eliminar los elementos no deseados
    for j = 1, #A do
      if j ~= i then
        local factor = B[j][i]
        for k = 1, #A[1] do
          B[j][k] = B[j][k] - factor * B[i][k]
          I[j][k] = I[j][k] - factor * I[i][k]
        end
      end
    end
  end

  -- Devuelve la matriz inversa
  return I
end

-- Determina si una matriz es simétrica
function M.simetrica(A)
  for i = 1, #A do
    for j = 1, #A[1] do
      if A[i][j] ~= A[j][i] then
        return false
      end
    end
  end
  return true
end

-- Determina si una matriz es diagonal
function M.diagonal(A)
  for i = 1, #A do
    for j = 1, #A[1] do
      if i ~= j and A[i][j] ~= 0 then
        return false
      end
    end
  end
  return true
end

-- Determina si una matriz es triangular superior
function M.triangularSuperior(A)
  for i = 1, #A do
    for j = 1, i-1 do
      if A[i][j] ~= 0 then
        return false
      end
    end
  end
  return true
end

-- Determina si una matriz es triangular inferior
function M.triangularInferior(A)
  for i = 1, #A do
    for j = i+1, #A[1] do
      if A[i][j] ~= 0 then
        return false
      end
    end
  end
  return true
end

-- Devuelve los autovalores de una matriz cuadrada
function M.autovalores(A)
  if #A ~= #A[1] then
    error("La matriz no es cuadrada")
  end

  -- Inicializa la lista de autovalores
  local valores = {}

  -- Calcula el polinomio característico
  local p = M.crear(#A, #A[1])
  for i = 1, #A do
    p[i][i] = 1
  end
  for i = 2, #A do
    p[i][i-1] = -A[i][i-1]
  end
  for i = #A, 2, -1 do
    p[i][i] = -M.suma(A, M.multiplicar(p[i+1], M.crear(i-1, i-1)))[i][i]
  end

  -- Encuentra las raíces del polinomio característico
  for i = 1, #A do
    local r = M.raices(p[i])
    for j = 1, #r do
      valores[j] = r[j]
    end
  end

  -- Devuelve la lista de autovalores
  return valores
end

-- Calcula las raíces de un polinomio
function M.raices(p)
  -- Inicializa la lista de raíces
  local raices = {}

  -- Calcula las raíces reales
  for i = 1, #p do
    if p[i] ~= 0 then
      local r = -p[i]/p[i-1]
      raices[#raices+1] = r
    end
  end

  -- Calcula las raíces complejas
  local q = M.crear(#p-1, #p-1)
  for i = 1, #p-1 do
    for j = 1, #p-1 do
      q[i][j] = p[i+j]
    end
  end
  local r = M.raicesComplejas(q)
  for i = 1, #r do
    raices[#raices+1] = r[i]
  end

  -- Devuelve la lista de raíces
  return raices
end

-- Calcula las raíces complejas de un polinomio
function M.raicesComplejas(p)
  -- Inicializa la lista de raíces
  local raices = {}

  -- Calcula las raíces reales
  for i = 1, #p do
    if p[i] ~= 0 then
      local r = -p[i]/p[i-1]
      raices[#raices+1] = r
    end
  end

  -- Calcula la discriminante
  local disc = p[#p][#p] - 4 * p[#p-1][#p-1] * p[#p][#p-2