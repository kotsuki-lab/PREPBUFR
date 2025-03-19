module lib_math_linalg_operator
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public operator (.t.)
  public operator (.mul.)
  public operator (.vmul.)
  public operator (.dot.)
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface operator (.t.)
    module procedure transpose__1d_dble
    module procedure transpose__2d_dble
  end interface

  interface operator (.mul.)
    module procedure matmul__2dx2d_dble
    module procedure matmul__2dx1d_dble
    module procedure matmul__1dx2d_dble
  end interface

  interface operator (.vmul.)
    module procedure vecmul__2dx1d_dble
    module procedure vecmul__1dx2d_dble
  end interface

  interface operator (.dot.)
    module procedure dot_product__dble_1d
    module procedure dot_product__dble_2d
  end interface
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
pure function transpose__1d_dble(a) result(b)
  implicit none
  real(8), intent(in) :: a(:)
  real(8) :: b(1,size(a))

  b(1,:) = a
end function transpose__1d_dble
!===============================================================
!
!===============================================================
pure function transpose__2d_dble(a) result(b)
  implicit none
  real(8), intent(in) :: a(:,:)
  real(8) :: b(size(a,2),size(a,1))

  b = transpose(a)
end function transpose__2d_dble
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! A: matrix (n,m)
! B: matrix (m,l)
! C: matrix (n,l)
!===============================================================
pure function matmul__2dx2d_dble(A, B) result(C)
  implicit none
  real(8), intent(in) :: A(:,:)      !(n,m)
  real(8), intent(in) :: B(:,:)      !(m,l)
  real(8) :: C(size(A,1),size(B,2))  !(n,l)

  C = matmul(A,B)
end function matmul__2dx2d_dble
!===============================================================
! A: matrix (n,m)
! B: matrix (m,1)
! C: matrix (n,1)
!===============================================================
pure function matmul__2dx1d_dble(A, B) result(C)
  implicit none
  real(8), intent(in) :: A(:,:)  !(n,m)
  real(8), intent(in) :: B(:)    !(m)
  real(8) :: C(size(A,1),1)      !(n,1)

  C = matmul(A,spread(B,2,1))
end function matmul__2dx1d_dble
!===============================================================
! A: matrix (1,n)
! B: matrix (n,m)
! C: matrix (1,m)
!===============================================================
pure function matmul__1dx2d_dble(A, B) result(C)
  implicit none
  real(8), intent(in) :: A(:)    !(n)
  real(8), intent(in) :: B(:,:)  !(n,m)
  real(8) :: C(1,size(B,2))      !(1,m)

  C = matmul(spread(A,1,1),B)
end function matmul__1dx2d_dble
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! A: vector (n)
! B: matrix (n,m)
! C: vector (m)
!===============================================================
pure function vecmul__2dx1d_dble(A, B) result(C)
  implicit none
  real(8), intent(in) :: A(:)    !(n)
  real(8), intent(in) :: B(:,:)  !(n,m)
  real(8) :: C(size(B,2))        !(m)

  C = maxval(matmul(spread(A,1,1),B),1)
end function vecmul__2dx1d_dble
!===============================================================
! A: matrix (n,m)
! B: vector (m)
! C: vector (n)
!===============================================================
pure function vecmul__1dx2d_dble(A, B) result(C)
  implicit none
  real(8), intent(in) :: A(:,:)  !(n,m)
  real(8), intent(in) :: B(:)    !(m)
  real(8) :: C(size(A,1))        !(n)

  C = maxval(matmul(A,spread(B,2,1)),2)
end function vecmul__1dx2d_dble
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
real(8) pure function dot_product__dble_1d(a, b) result(p)
  implicit none
  real(8), intent(in) :: a(:)  !(n)
  real(8), intent(in) :: b(:)  !(n)

  p = sum(a * b)
end function dot_product__dble_1d
!===============================================================
!
!===============================================================
real(8) pure function dot_product__dble_2d(a, b) result(p)
  implicit none
  real(8), intent(in) :: a(:,:)  !(n,m)
  real(8), intent(in) :: b(:,:)  !(n,m)

  p = sum(a * b)
end function dot_product__dble_2d
!===============================================================
!
!===============================================================
end module lib_math_linalg_operator
