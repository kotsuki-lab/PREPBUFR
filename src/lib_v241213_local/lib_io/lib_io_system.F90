module lib_io_system
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: get_terminal_width
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine get_terminal_width(n, stat)
  implicit none
  integer, intent(out) :: n
  integer, intent(out) :: stat

#ifdef USE_C
  character(32), pointer :: s
  integer(4), pointer :: cl
  integer(4), pointer :: cstat
  integer :: ios
#endif

  call echo(code%bgn, 'get_terminal_width', '-p -x2')
  !-------------------------------------------------------------
  stat = 0

#ifdef USE_C
  allocate(s)
  allocate(cl)
  allocate(cstat)

  call c_system_tput_cols(s, cl, cstat)

  if( cstat == 0 )then
    read(s(:cl),*,iostat=ios) n
    if( ios /= 0 )then
      n = 0
      stat = 1
    endif
  else
    n = 0
    stat = 1
  endif

  deallocate(s)
  deallocate(cl)
  deallocate(cstat)
#elif TERMINAL_WIDTH <= 0
  n = 0
#elif TERMINAL_WIDTH <= 32
  n = 0
#elif TERMINAL_WIDTH <= 40
  n = 32
#elif TERMINAL_WIDTH <= 48
  n = 40
#elif TERMINAL_WIDTH <= 56
  n = 48
#else
  n = 128
#endif 
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_terminal_width
!===============================================================
!
!===============================================================
end module lib_io_system
