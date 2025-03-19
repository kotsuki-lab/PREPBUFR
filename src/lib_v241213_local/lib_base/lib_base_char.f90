module lib_base_char
  use lib_const
  use lib_base_message
  implicit none
  private
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  public :: lower
  public :: upper
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
contains
!==============================================================
!
!==============================================================
pure function lower(c_in) result(c_out)
  implicit none
  character(*), intent(in) :: c_in
  character(len(c_in))     :: c_out
  integer :: i

  do i = 1, len(c_in)
    selectcase( c_in(i:i) )
    case( 'A' ); c_out(i:i) = 'a'
    case( 'B' ); c_out(i:i) = 'b'
    case( 'C' ); c_out(i:i) = 'c'
    case( 'D' ); c_out(i:i) = 'd'
    case( 'E' ); c_out(i:i) = 'e'
    case( 'F' ); c_out(i:i) = 'f'
    case( 'G' ); c_out(i:i) = 'g'
    case( 'H' ); c_out(i:i) = 'h'
    case( 'I' ); c_out(i:i) = 'i'
    case( 'J' ); c_out(i:i) = 'j'
    case( 'K' ); c_out(i:i) = 'k'
    case( 'L' ); c_out(i:i) = 'l'
    case( 'M' ); c_out(i:i) = 'm'
    case( 'N' ); c_out(i:i) = 'n'
    case( 'O' ); c_out(i:i) = 'o'
    case( 'P' ); c_out(i:i) = 'p'
    case( 'Q' ); c_out(i:i) = 'q'
    case( 'R' ); c_out(i:i) = 'r'
    case( 'S' ); c_out(i:i) = 's'
    case( 'T' ); c_out(i:i) = 't'
    case( 'U' ); c_out(i:i) = 'u'
    case( 'V' ); c_out(i:i) = 'v'
    case( 'W' ); c_out(i:i) = 'w'
    case( 'X' ); c_out(i:i) = 'x'
    case( 'Y' ); c_out(i:i) = 'y'
    case( 'Z' ); c_out(i:i) = 'z'
    case default; c_out(i:i) = c_in(i:i)
    endselect
  enddo
end function lower
!==============================================================
!
!==============================================================
pure function upper(c_in) result(c_out)
  implicit none
  character(*), intent(in) :: c_in
  character(len(c_in))     :: c_out
  integer :: i

  do i = 1, len(c_in)
    selectcase( c_in(i:i) )
    case( 'a' ); c_out(i:i) = 'A'
    case( 'b' ); c_out(i:i) = 'B'
    case( 'c' ); c_out(i:i) = 'C'
    case( 'd' ); c_out(i:i) = 'D'
    case( 'e' ); c_out(i:i) = 'E'
    case( 'f' ); c_out(i:i) = 'F'
    case( 'g' ); c_out(i:i) = 'G'
    case( 'h' ); c_out(i:i) = 'H'
    case( 'i' ); c_out(i:i) = 'I'
    case( 'j' ); c_out(i:i) = 'J'
    case( 'k' ); c_out(i:i) = 'K'
    case( 'l' ); c_out(i:i) = 'L'
    case( 'm' ); c_out(i:i) = 'M'
    case( 'n' ); c_out(i:i) = 'N'
    case( 'o' ); c_out(i:i) = 'O'
    case( 'p' ); c_out(i:i) = 'P'
    case( 'q' ); c_out(i:i) = 'Q'
    case( 'r' ); c_out(i:i) = 'R'
    case( 's' ); c_out(i:i) = 'S'
    case( 't' ); c_out(i:i) = 'T'
    case( 'u' ); c_out(i:i) = 'U'
    case( 'v' ); c_out(i:i) = 'V'
    case( 'w' ); c_out(i:i) = 'W'
    case( 'x' ); c_out(i:i) = 'X'
    case( 'y' ); c_out(i:i) = 'Y'
    case( 'z' ); c_out(i:i) = 'Z'
    case default; c_out(i:i) = c_in(i:i)
    endselect
  enddo
end function upper
!==============================================================
!
!==============================================================
end module lib_base_char
