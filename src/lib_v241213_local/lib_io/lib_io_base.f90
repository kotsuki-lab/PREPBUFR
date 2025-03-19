module lib_io_base
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: unit_number

  public :: byte_of_dtype

  public :: dtype_int
  public :: dtype_float

  public :: int_endian
  public :: endian_long_name
  public :: endian_short_name
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
function unit_number() result(un)
  implicit none
  integer :: un
  logical :: is_opened

  call echo(code%bgn, 'unit_number', '-p -x2')
  !-------------------------------------------------------------
  un = 21
  do while( un <= 9999 )
    inquire(unit=un, opened=is_opened)
    if( .not. is_opened ) exit
    un = un + 1
  enddo

  if( is_opened )then
    call eerr('Unopened unit number was not found.')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end function unit_number
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
integer function byte_of_dtype(dtype) result(res)
  implicit none
  character(*), intent(in) :: dtype

  call echo(code%bgn, 'byte_of_dtype', '-p -x2')
  !-------------------------------------------------------------
  selectcase( dtype )
  case( dtype_int1 )
    res = 1
  case( dtype_int2 )
    res = 2
  case( dtype_int4 )
    res = 4
  case( dtype_int8 )
    res = 8
  case( dtype_real )
    res = 4
  case( dtype_dble )
    res = 8
  case( dtype_undef )
    res = 0
  case default
    call eerr('Invalid value in $dtype: '//str(dtype))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function byte_of_dtype
!===============================================================
!
!===============================================================





!===============================================================
!
!===============================================================
character(clen_key) function dtype_int(byte) result(dtype)
  implicit none
  integer, intent(in) :: byte

  call echo(code%bgn, 'dtype_int', '-p')
  !-------------------------------------------------------------
  selectcase( byte )
  case( 1 )
    dtype = dtype_int1
  case( 2 )
    dtype = dtype_int2
  case( 4 )
    dtype = dtype_int4
  case( 8 )
    dtype = dtype_int8
  case default
    call eerr('Invalid value in $byte: '//str(byte))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function dtype_int
!===============================================================
!
!===============================================================
character(clen_key) function dtype_float(byte) result(dtype)
  implicit none
  integer, intent(in) :: byte

  call echo(code%bgn, 'dtype_float', '-p')
  !-------------------------------------------------------------
  selectcase( byte )
  case( 4 )
    dtype = dtype_real
  case( 8 )
    dtype = dtype_dble
  case default
    call eerr('Invalid value in $byte: '//str(byte))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function dtype_float
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
integer function int_endian(endian) result(res)
  use lib_base, only: lower
  implicit none
  character(*), intent(in) :: endian

  call echo(code%bgn, 'int_endian', '-p')
  !-------------------------------------------------------------
  selectcase( lower(endian) )
  case( endian_little, &
        endian_little_short )
    res = int_endian_little
  case( endian_big, &
        endian_big_short )
    res = int_endian_big
  case default
    call eerr('Invalid value in $endian: '//str(endian))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function int_endian
!===============================================================
!
!===============================================================
character(clen_key) function endian_long_name(endian) result(ret)
  implicit none
  character(*), intent(in) :: endian

  call echo(code%bgn, 'endian_long_name', '-p')
  !-------------------------------------------------------------
  selectcase( endian )
  case( endian_little, &
        endian_little_short )
    ret = endian_little
  case( endian_big, &
        endian_big_short )
    ret = endian_big
  case default
    call eerr('Invalid value in $endian: '//str(endian))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function endian_long_name
!===============================================================
!
!===============================================================
character(clen_key) function endian_short_name(endian) result(ret)
  implicit none
  character(*), intent(in) :: endian

  call echo(code%bgn, 'endian_short_name', '-p')
  !-------------------------------------------------------------
  selectcase( endian )
  case( endian_little, &
        endian_little_short )
    ret = endian_little_short
  case( endian_big, &
        endian_big_short )
    ret = endian_big_short
  case default
    call eerr('Invalid value in $endian: '//str(endian))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function endian_short_name
!===============================================================
!
!===============================================================
end module lib_io_base
