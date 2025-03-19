module lib_util_swap
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: swap
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface swap
    module procedure swap_int1_0d
    module procedure swap_int2_0d
    module procedure swap_int4_0d
    module procedure swap_int8_0d
    module procedure swap_real_0d
    module procedure swap_dble_0d
    module procedure swap_log4_0d
  end interface
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine swap_int1_0d(a, b)
  implicit none
  integer(1), intent(inout) :: a, b

  integer(1) :: c

  c = a
  a = b
  b = c
end subroutine swap_int1_0d
!===============================================================
!
!===============================================================
subroutine swap_int2_0d(a, b)
  implicit none
  integer(2), intent(inout) :: a, b

  integer(2) :: c

  c = a
  a = b
  b = c
end subroutine swap_int2_0d
!===============================================================
!
!===============================================================
subroutine swap_int4_0d(a, b)
  implicit none
  integer(4), intent(inout) :: a, b

  integer(4) :: c

  c = a
  a = b
  b = c
end subroutine swap_int4_0d
!===============================================================
!
!===============================================================
subroutine swap_int8_0d(a, b)
  implicit none
  integer(8), intent(inout) :: a, b

  integer(8) :: c

  c = a
  a = b
  b = c
end subroutine swap_int8_0d
!===============================================================
!
!===============================================================
subroutine swap_real_0d(a, b)
  implicit none
  real(4), intent(inout) :: a, b

  real(4) :: c

  c = a
  a = b
  b = c
end subroutine swap_real_0d
!===============================================================
!
!===============================================================
subroutine swap_dble_0d(a, b)
  implicit none
  real(8), intent(inout) :: a, b

  real(8) :: c

  c = a
  a = b
  b = c
end subroutine swap_dble_0d
!===============================================================
!
!===============================================================
subroutine swap_log4_0d(a, b)
  implicit none
  logical(4), intent(inout) :: a, b

  logical(4) :: c

  c = a
  a = b
  b = c
end subroutine swap_log4_0d
!===============================================================
!
!===============================================================
end module lib_util_swap
