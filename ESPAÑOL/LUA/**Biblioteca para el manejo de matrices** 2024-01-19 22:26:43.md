```lua
-- Módulo para manejar matrices
local matrix = {}

-- Crea una matriz de dimensiones mxn
function matrix.create(m, n)
    local mat = {}
    for i=1,m do
        mat[i] = {}
        for j=1,n do
            mat[i][j] = 0
        end
    end
    return mat
end

-- Suma dos matrices de dimensiones mxn
function matrix.add(mat1, mat2)
    local mat3 = matrix.create(#mat1, #mat1[1])
    for i=1,#mat1 do
        for j=1,#mat1[1] do
            mat3[i][j] = mat1[i][j] + mat2[i][j]
        end
    end
    return mat3
end

-- Multiplica una matriz por un escalar
function matrix.scalar_multiply(mat, scalar)
    local mat2 = matrix.create(#mat, #mat[1])
    for i=1,#mat do
        for j=1,#mat[1] do
            mat2[i][j] = mat[i][j] * scalar
        end
    end
    return mat2
end

-- Multiplica dos matrices de dimensiones mxn y pxq
function matrix.multiply(mat1, mat2)
    if #mat1[1] ~= #mat2 then
        error("Las matrices no se pueden multiplicar")
    end
    local mat3 = matrix.create(#mat1, #mat2[1])
    for i=1,#mat1 do
        for j=1,#mat2[1] do
            for k=1,#mat1[1] do
                mat3[i][j] = mat3[i][j] + mat1[i][k] * mat2[k][j]
            end
        end
    end
    return mat3
end

-- Transpuesta de una matriz
function matrix.transpose(mat)
    local mat2 = matrix.create(#mat[1], #mat)
    for i=1,#mat do
        for j=1,#mat[1] do
            mat2[j][i] = mat[i][j]
        end
    end
    return mat2
end

-- Inversa de una matriz
function matrix.inverse(mat)
    if #mat ~= #mat[1] then
        error("La matriz no es cuadrada")
    end
    local n = #mat
    local mat2 = matrix.create(n, n*2)
    for i=1,n do
        for j=1,n do
            mat2[i][j] = mat[i][j]
            if i == j then
                mat2[i][n+j] = 1
            else
                mat2[i][n+j] = 0
            end
        end
    end
    for i=1,n do
        local pivot = mat2[i][i]
        for j=1,n*2 do
            mat2[i][j] = mat2[i][j] / pivot
        end
        for j=1,n do
            if i ~= j then
                local factor = mat2[j][i]
                for k=1,n*2 do
                    mat2[j][k] = mat2[j][k] - factor * mat2[i][k]
                end
            end
        end
    end
    local inv_mat = matrix.create(n, n)
    for i=1,n do
        for j=n+1,n*2 do
            inv_mat[i][j-n] = mat2[i][j]
        end
    end
    return inv_mat
end

-- Determinante de una matriz
function matrix.determinant(mat)
    if #mat ~= #mat[1] then
        error("La matriz no es cuadrada")
    end
    local n = #mat
    if n == 1 then
        return mat[1][1]
    elseif n == 2 then
        return mat[1][1]*mat[2][2] - mat[1][2]*mat[2][1]
    else
        local det = 0
        for i=1,n do
            local sub_mat = matrix.create(n-1, n-1)
            for j=2,n do
                for k=1,n do
                    if k < i then
                        sub_mat[j-1][k] = mat[j][k]
                    elseif k > i then
                        sub_mat[j-1][k-1] = mat[j][k]
                    end
                end
            end
            det = det + mat[1][i] * matrix.determinant(sub_mat) * (-1)^(i+1)
        end
        return det
    end
end

-- Eigenvalores y eigenvectores de una matriz
function matrix.eigen(mat)
    if #mat ~= #mat[1] then
        error("La matriz no es cuadrada")
    end
    local n = #mat
    local lambda = {}
    local v = {}
    for i=1,n do
        lambda[i] = 0
        v[i] = {}
        for j=1,n do
            v[i][j] = 0
        end
    end
    local tol = 1e-6
    local max_iter = 100
    local iter = 0
    while iter < max_iter do
        iter = iter + 1
        local lambda_old = {}
        for i=1,n do
            lambda_old[i] = lambda[i]
        end
        for i=1,n do
            local v_old = {}
            for j=1,n do
                v_old[j] = v[i][j]
            end
            local sum = 0
            for j=1,n