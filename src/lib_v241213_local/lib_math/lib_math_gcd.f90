module lib_math_gcd
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: gcd
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface gcd
    module procedure gcd_int4
    module procedure gcd_int8
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer(4) function gcd_int4(x, y) result(i)
  implicit none
  integer(4), intent(in) :: x, y

  i = min(x,y)
  do while( mod(x,i) /= 0_4 .or. mod(y,i) /= 0_4 )
    i = i - 1_4
  enddo
end function gcd_int4
!===============================================================
!
!===============================================================
integer(8) function gcd_int8(x, y) result(i)
  implicit none
  integer(8), intent(in) :: x, y

  i = min(x,y)
  do while( mod(x,i) /= 0_8 .or. mod(y,i) /= 0_8 )
    i = i - 1_8
  enddo
end function gcd_int8
!===============================================================
!
!===============================================================
end module lib_math_gcd
