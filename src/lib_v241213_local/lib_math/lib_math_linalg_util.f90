module lib_math_linalg_util
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: scalar

  public :: make_identity

  public :: make_diag_new
  public :: make_diag_sbt
  public :: get_vec_diag

  public :: calc_vec_inv

  public :: calc_cross_product
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface scalar
    module procedure scalar_dble_1d
    module procedure scalar_dble_2d
  end interface

  interface calc_vec_inv
    module procedure calc_vec_inv__out
    module procedure calc_vec_inv__inout
  end interface

  interface calc_cross_product
    module procedure calc_cross_product_array
    module procedure calc_cross_product_components
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
real(8) function scalar_dble_1d(a) result(x)
  implicit none
  real(8), intent(in) :: a(:)  !(1)

  x = a(1)
end function scalar_dble_1d
!===============================================================
!
!===============================================================
real(8) function scalar_dble_2d(a) result(x)
  implicit none
  real(8), intent(in) :: a(:,:)  !(1,1)

  x = a(1,1)
end function scalar_dble_2d
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
subroutine make_identity(A)
  implicit none
  real(8), intent(out) :: A(:,:)

  integer :: i

  A(:,:) = 0.d0
  do i = 1, size(A,1)
    A(i,i) = 1.d0
  enddo
end subroutine make_identity
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
subroutine make_diag_new(A, D1)
  implicit none
  real(8), intent(out) :: A(:,:)
  real(8), intent(in)  :: D1(:)

  integer :: i

  A(:,:) = 0.d0
  do i = 1, size(D1)
    A(i,i) = D1(i)
  enddo
end subroutine make_diag_new
!===============================================================
!
!===============================================================
subroutine make_diag_sbt(A, D1)
  implicit none
  real(8), intent(inout) :: A(:,:)
  real(8), intent(in)    :: D1(:)

  integer :: i

  do i = 1, size(D1)
    A(i,i) = D1(i)
  enddo
end subroutine make_diag_sbt
!===============================================================
!
!===============================================================
subroutine get_vec_diag(D1, A)
  implicit none
  real(8), intent(out) :: D1(:)
  real(8), intent(in)  :: A(:,:)

  integer :: i

  do i = 1, size(D1)
    D1(i) = A(i,i)
  enddo
end subroutine get_vec_diag
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
subroutine calc_vec_inv__out(x, y)
  implicit none
  real(8), intent(in) :: x(:)
  real(8), intent(out) :: y(:)

  integer :: i

  do i = 1, size(x)
    y(i) = 1.d0 / x(i)
  enddo
end subroutine calc_vec_inv__out
!===============================================================
!
!===============================================================
subroutine calc_vec_inv__inout(x)
  implicit none
  real(8), intent(inout) :: x(:)

  integer :: i

  do i = 1, size(x)
    x(i) = 1.d0 / x(i)
  enddo
end subroutine calc_vec_inv__inout
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
subroutine calc_cross_product_array(a, b, c)
  implicit none
  real(8), intent(in)  :: a(:), b(:)
  real(8), intent(out) :: c(:)

  c(1) = a(2)*b(3) - a(3)*b(2)
  c(2) = a(3)*b(1) - a(1)*b(3)
  c(3) = a(1)*b(2) - a(2)*b(1)
end subroutine calc_cross_product_array
!===============================================================
!
!===============================================================
subroutine calc_cross_product_components(a1, a2, a3, b1, b2, b3, c1, c2, c3)
  implicit none
  real(8), intent(in)  :: a1, a2, a3, b1, b2, b3
  real(8), intent(out) :: c1, c2, c3

  c1 = a2*b3 - a3*b2
  c2 = a3*b1 - a1*b3
  c3 = a1*b2 - a2*b1
end subroutine calc_cross_product_components
!===============================================================
!
!===============================================================
end module lib_math_linalg_util
