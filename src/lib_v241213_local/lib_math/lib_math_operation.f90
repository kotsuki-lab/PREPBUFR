module lib_math_operation
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: add
  public :: mul
  public :: div
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface add
    module procedure add_int1
    module procedure add_int2
    module procedure add_int4
    module procedure add_int8
    module procedure add_real
    module procedure add_dble
  end interface

  interface mul
    module procedure mul_dble_int4
    module procedure mul_dble_int8
    module procedure mul_dble_dble
  end interface

  interface div
    module procedure div_dble_int4
    module procedure div_dble_int8
    module procedure div_dble_dble
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine add_int1(x, inc)
  implicit none
  integer(1), intent(inout) :: x
  integer(1), intent(in), optional :: inc

  if( present(inc) )then
    x = x + inc
  else
    x = x + 1_1
  endif
end subroutine add_int1
!===============================================================
!
!===============================================================
subroutine add_int2(x, inc)
  implicit none
  integer(2), intent(inout) :: x
  integer(2), intent(in), optional :: inc

  if( present(inc) )then
    x = x + inc
  else
    x = x + 1_2
  endif
end subroutine add_int2
!===============================================================
!
!===============================================================
subroutine add_int4(x, inc)
  implicit none
  integer(4), intent(inout) :: x
  integer(4), intent(in), optional :: inc

  if( present(inc) )then
    x = x + inc
  else
    x = x + 1_1
  endif
end subroutine add_int4
!===============================================================
!
!===============================================================
subroutine add_int8(x, inc)
  implicit none
  integer(8), intent(inout) :: x
  integer(8), intent(in), optional :: inc

  if( present(inc) )then
    x = x + inc
  else
    x = x + 1_1
  endif
end subroutine add_int8
!===============================================================
!
!===============================================================
subroutine add_real(x, inc)
  implicit none
  real(4), intent(inout) :: x
  real(4), intent(in), optional :: inc

  if( present(inc) )then
    x = x + inc
  else
    x = x + 1.0
  endif
end subroutine add_real
!===============================================================
!
!===============================================================
subroutine add_dble(x, inc)
  implicit none
  real(8), intent(inout) :: x
  real(8), intent(in), optional :: inc

  if( present(inc) )then
    x = x + inc
  else
    x = x + 1.d0
  endif
end subroutine add_dble
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
subroutine mul_dble_int4(x, y)
  implicit none
  real(8)   , intent(inout) :: x
  integer(4), intent(in)    :: y

  x = x * y
end subroutine mul_dble_int4
!===============================================================
!
!===============================================================
subroutine mul_dble_int8(x, y)
  implicit none
  real(8)   , intent(inout) :: x
  integer(8), intent(in)    :: y

  x = x * y
end subroutine mul_dble_int8
!===============================================================
!
!===============================================================
subroutine mul_dble_dble(x, y)
  implicit none
  real(8), intent(inout) :: x
  real(8), intent(in)    :: y

  x = x * y
end subroutine mul_dble_dble
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
subroutine div_dble_int4(x, a)
  implicit none
  real(8)   , intent(inout) :: x
  integer(4), intent(in)    :: a

  x = x / a
end subroutine div_dble_int4
!===============================================================
!
!===============================================================
subroutine div_dble_int8(x, a)
  implicit none
  real(8)   , intent(inout) :: x
  integer(8), intent(in)    :: a

  x = x / a
end subroutine div_dble_int8
!===============================================================
!
!===============================================================
subroutine div_dble_dble(x, a)
  implicit none
  real(8), intent(inout) :: x
  real(8), intent(in)    :: a

  x = x / a
end subroutine div_dble_dble
!===============================================================
!
!===============================================================
end module lib_math_operation
