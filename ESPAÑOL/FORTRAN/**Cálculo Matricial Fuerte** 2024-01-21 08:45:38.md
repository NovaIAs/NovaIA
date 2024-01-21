```fortran
program CalculoFuerte

implicit none

! Variables globales
integer, parameter :: dim=16
real(kind=8), dimension(dim,dim) :: matrizA, matrizB, matrizC
real(kind=8), dimension(dim) :: vectorX, vectorB

! Funciones
real(kind=8) function suma(a,b)
  implicit none
  real(kind=8), intent(in) :: a, b
  suma = a + b
end function

real(kind=8) function producto(a,b)
  implicit none
  real(kind=8), intent(in) :: a, b
  producto = a * b
end function

subroutine productoMatrizVector(A, x, b)
  implicit none
  real(kind=8), intent(in) :: A(dim,dim), x(dim)
  real(kind=8), intent(out) :: b(dim)
  integer :: i, j
  do i=1, dim
    b(i) = 0.0d0
    do j=1, dim
      b(i) = b(i) + producto(A(i,j),x(j))
    end do
  end do
end subroutine

subroutine multiplicacionMatrices(A, B, C)
  implicit none
  real(kind=8), intent(in) :: A(dim,dim), B(dim,dim)
  real(kind=8), intent(out) :: C(dim,dim)
  integer :: i, j, k
  do i=1, dim
    do j=1, dim
      C(i,j) = 0.0d0
      do k=1, dim
        C(i,j) = C(i,j) + producto(A(i,k),B(k,j))
      end do
    end do
  end do
end subroutine

subroutine resolverSistema(A, b, x)
  implicit none
  real(kind=8), intent(in) :: A(dim,dim), b(dim)
  real(kind=8), intent(out) :: x(dim)
  integer :: i, j, k
  real(kind=8) :: m
  do i=1, dim-1
    do j=i+1, dim
      m = A(j,i) / A(i,i)
      do k=i, dim
        A(j,k) = A(j,k) - m * A(i,k)
      end do
      b(j) = b(j) - m * b(i)
    end do
  end do
  x(dim) = b(dim) / A(dim,dim)
  do i=dim-1, 1, -1
    x(i) = (b(i) - suma(productoMatrizVector(A(i,i+1:dim),x(i+1:dim)),0.0d0)) / A(i,i)
  end do
end subroutine

! Código principal
! Inicializar matrizA, matrizB, vectorX y vectorB
matrizA = 0.0d0
matrizB = 0.0d0
vectorX = 0.0d0
vectorB = 0.0d0
matrizA(1,1) = 2.0d0
matrizA(1,2) = 1.0d0
matrizA(1,3) = 1.0d0
matrizA(2,1) = 1.0d0
matrizA(2,2) = 2.0d0
matrizA(2,3) = 1.0d0
matrizA(3,1) = 1.0d0
matrizA(3,2) = 1.0d0
matrizA(3,3) = 2.0d0
matrizB(1,1) = 1.0d0
matrizB(1,2) = 0.0d0
matrizB(1,3) = 0.0d0
matrizB(2,1) = 0.0d0
matrizB(2,2) = 1.0d0
matrizB(2,3) = 0.0d0
matrizB(3,1) = 0.0d0
matrizB(3,2) = 0.0d0