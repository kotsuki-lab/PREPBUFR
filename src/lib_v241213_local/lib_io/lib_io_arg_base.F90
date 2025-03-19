module lib_io_arg_base
  use lib_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: argnum
  public :: argument
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------

!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer function argnum()
  implicit none

#ifdef OPT_NO_F23
  argnum = iargc()
#else
  argnum = command_argument_count()
#endif
end function argnum
!===============================================================
!
!===============================================================
character(CLEN_LINE) function argument(i)
  implicit none
  integer, intent(in) :: i

#ifdef OPT_NO_F23
  call getarg(i, argument)
#else
  call get_command_argument(i, argument)
#endif
end function argument
!===============================================================
!
!===============================================================
end module lib_io_arg_base
