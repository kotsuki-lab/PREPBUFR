module lib_util_str
  use lib_const
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: str_bgn_sentence

  public :: str_coords
  !-------------------------------------------------------------
  interface str_coords
    module procedure str_coords_1d
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
function str_bgn_sentence(s) result(sout)
  implicit none
  character(*), intent(in) :: s
  character(len(s)) :: sout

  sout = s

  if( s == '')then
    continue
  else
    selectcase( s(1:1) )
    case( 'a' )
      sout(1:1) = 'A'
    case( 'b' )
      sout(1:1) = 'B'
    case( 'c' )
      sout(1:1) = 'C'
    case( 'd' )
      sout(1:1) = 'D'
    case( 'e' )
      sout(1:1) = 'E'
    case( 'f' )
      sout(1:1) = 'F'
    case( 'g' )
      sout(1:1) = 'G'
    case( 'h' )
      sout(1:1) = 'H'
    case( 'i' )
      sout(1:1) = 'I'
    case( 'j' )
      sout(1:1) = 'J'
    case( 'k' )
      sout(1:1) = 'K'
    case( 'l' )
      sout(1:1) = 'L'
    case( 'm' )
      sout(1:1) = 'M'
    case( 'n' )
      sout(1:1) = 'N'
    case( 'o' )
      sout(1:1) = 'O'
    case( 'p' )
      sout(1:1) = 'P'
    case( 'q' )
      sout(1:1) = 'Q'
    case( 'r' )
      sout(1:1) = 'R'
    case( 's' )
      sout(1:1) = 'S'
    case( 't' )
      sout(1:1) = 'T'
    case( 'u' )
      sout(1:1) = 'U'
    case( 'v' )
      sout(1:1) = 'V'
    case( 'w' )
      sout(1:1) = 'W'
    case( 'x' )
      sout(1:1) = 'X'
    case( 'y' )
      sout(1:1) = 'Y'
    case( 'z' )
      sout(1:1) = 'Z'
    case default
      continue
    endselect
  endif
end function str_bgn_sentence
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
function str_coords_1d(coords, coef, miss, wfmt, dlm, n1max) result(res)
  implicit none
  real(8)     , intent(in) :: coords(:)
  real(8)     , intent(in), optional :: coef
  real(8)     , intent(in), optional :: miss
  character(*), intent(in), optional :: wfmt
  character(*), intent(in), optional :: dlm
  integer     , intent(in), optional :: n1max
  character(clen_line) :: res

  real(8) :: coef_
  real(8) :: miss_
  character(:), allocatable :: dlm_
  character(clen_wfmt) :: wfmt_
  integer :: n1max_

  integer, parameter :: nmin = 5
  character(1), parameter :: str_miss = '-'
  character(3), parameter :: str_3dots = '...'

  integer :: i, n
  integer :: dgt_wfmt

  call echo(code%bgn, 'str_coords__MP__str_coords_1d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  coef_ = 1.d0
  if( present(coef) ) coef_ = coef

  miss_ = -1d20
  if( present(miss) ) miss_ = miss

  wfmt_ = 'f13.6'
  if( present(wfmt) ) wfmt_ = wfmt

  n1max_ = 5
  if( present(n1max) ) n1max_ = n1max

  allocate(character(1) :: dlm_)
  dlm_ = ', '
  if( present(dlm) ) dlm_ = dlm
  !-------------------------------------------------------------
  dgt_wfmt = cl(wfmt)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  n = size(coords)

  i = 1
  if( coords(i) == miss_ )then
    res = str('',-dgt_wfmt,str_miss)
  else
    res = str(coords(i)*coef_,wfmt_)
  endif

  if( n <= n1max_ )then
    do i = 2, n
      if( coords(i) == miss_ )then
        res = trim(res)//dlm_//str('',-dgt_wfmt,str_miss)
      else
        res = trim(res)//dlm_//str(coords(i)*coef_,wfmt_)
      endif
    enddo
  else
    do i = 2, (nmin-1)/2
      if( coords(i) == miss_ )then
        res = trim(res)//dlm_//str('',-dgt_wfmt,str_miss)
      else
        res = trim(res)//dlm_//str(coords(i)*coef_,wfmt_)
      endif
    enddo

    res = trim(res)//dlm_//trim(str_3dots)

    do i = n-(nmin-1)/2+1, n
      if( coords(i) == miss_ )then
        res = trim(res)//dlm_//str('',-dgt_wfmt,str_miss)
      else
        res = trim(res)//dlm_//str(coords(i)*coef_,wfmt_)
      endif
    enddo
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_coords_1d
!===============================================================
!
!===============================================================
end module lib_util_str
