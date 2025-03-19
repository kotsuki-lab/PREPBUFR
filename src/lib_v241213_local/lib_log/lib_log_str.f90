module lib_log_str
  use lib_const
  use lib_base
  use lib_log_proc
  implicit none
  private
  !------------------------------------------------------------
  ! Public Procedures
  !------------------------------------------------------------
  public :: set_wfmt_real
  public :: get_wfmt_real
  public :: init_wfmt_real

  public :: str
  public :: dgt
  public :: cl

  public :: dgt_int8_0d_log  ! For comparison
  !------------------------------------------------------------
  ! Public Variables
  !------------------------------------------------------------
  public :: CODE
  !------------------------------------------------------------
  ! Interfaces
  !------------------------------------------------------------
  interface dgt
    module procedure dgt_char_0d
    module procedure dgt_int1_0d
    module procedure dgt_int1_1d
    module procedure dgt_int1_2d
    module procedure dgt_int1_3d
    module procedure dgt_int2_0d
    module procedure dgt_int2_1d
    module procedure dgt_int2_2d
    module procedure dgt_int2_3d
    module procedure dgt_int4_0d
    module procedure dgt_int4_1d
    module procedure dgt_int4_2d
    module procedure dgt_int4_3d
    module procedure dgt_int8_0d
    module procedure dgt_int8_1d
    module procedure dgt_int8_2d
    module procedure dgt_int8_3d
  end interface

  interface cl
    module procedure cl_char_0d
    module procedure cl_char_1d
    module procedure cl_log1_1d
    module procedure cl_log4_1d
    module procedure cl_int1_0d
    module procedure cl_int1_1d
    module procedure cl_int2_0d
    module procedure cl_int2_1d
    module procedure cl_int4_0d
    module procedure cl_int4_1d
    module procedure cl_int8_0d
    module procedure cl_int8_1d
    module procedure cl_real_1d
    module procedure cl_dble_1d
    module procedure cl_fmt_float
  end interface

  interface str
    module procedure str_char_0d_min
    module procedure str_char_0d_fmt_nofill
    module procedure str_char_0d_fmt_fill
    module procedure str_char_1d_min_nodlm
    module procedure str_char_1d_fmt_nodlm_nofill
    module procedure str_char_1d_fmt_dlm_nofill
    module procedure str_char_1d_fmt_dlm_fill
    module procedure str_log1_0d_min
    module procedure str_log1_0d_fmt
    module procedure str_log1_1d_min_nodlm
    module procedure str_log1_1d_min_dlm
    module procedure str_log1_1d_fmt_nodlm
    module procedure str_log1_1d_fmt_dlm
    module procedure str_log4_0d_min
    module procedure str_log4_0d_fmt
    module procedure str_log4_1d_min_nodlm
    module procedure str_log4_1d_min_dlm
    module procedure str_log4_1d_fmt_nodlm
    module procedure str_log4_1d_fmt_dlm
    module procedure str_int1_0d_min
    module procedure str_int1_0d_fmt
    module procedure str_int1_1d_min_nodlm
    module procedure str_int1_1d_min_dlm
    module procedure str_int1_1d_fmt_nodlm
    module procedure str_int1_1d_fmt_dlm
    module procedure str_int2_0d_min
    module procedure str_int2_0d_fmt
    module procedure str_int2_1d_min_nodlm
    module procedure str_int2_1d_min_dlm
    module procedure str_int2_1d_fmt_nodlm
    module procedure str_int2_1d_fmt_dlm
    module procedure str_int4_0d_min
    module procedure str_int4_0d_fmt
    module procedure str_int4_1d_min_nodlm
    module procedure str_int4_1d_min_dlm
    module procedure str_int4_1d_fmt_nodlm
    module procedure str_int4_1d_fmt_dlm
    module procedure str_int8_0d_min
    module procedure str_int8_0d_fmt
    module procedure str_int8_1d_min_nodlm
    module procedure str_int8_1d_min_dlm
    module procedure str_int8_1d_fmt_nodlm
    module procedure str_int8_1d_fmt_dlm
    module procedure str_real_0d_nofmt
    module procedure str_real_0d_fmt
    module procedure str_real_1d_nofmt_nodlm
    module procedure str_real_1d_fmt_nodlm
    module procedure str_real_1d_fmt_dlm
    module procedure str_dble_0d_nofmt
    module procedure str_dble_0d_fmt
    module procedure str_dble_1d_nofmt_nodlm
    module procedure str_dble_1d_fmt_nodlm
    module procedure str_dble_1d_fmt_dlm
  end interface
  !-------------------------------------------------------------
  ! Module Variables
  !-------------------------------------------------------------
  character(CLEN_WFMT), parameter :: WFMT_REAL_DEFAULT = 'es12.5'
  character(CLEN_WFMT) :: wfmt_real = WFMT_REAL_DEFAULT
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_wfmt_real(wfmt)
  implicit none
  character(*), intent(in) :: wfmt

  wfmt_real = wfmt
end subroutine set_wfmt_real
!===============================================================
!
!===============================================================
character(CLEN_WFMT) function get_wfmt_real() result(wfmt)
  implicit none

  wfmt = wfmt_real
end function get_wfmt_real
!===============================================================
!
!===============================================================
subroutine init_wfmt_real()
  implicit none

  wfmt_real = WFMT_REAL_DEFAULT
end subroutine init_wfmt_real
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
integer pure function dgt_char_0d(x) result(digit)
  implicit none
  character(*), intent(in) :: x

  digit = len_trim(x)
end function dgt_char_0d
!===============================================================
!
!===============================================================
integer pure function dgt_int1_0d(x) result(digit)
  implicit none
  integer(1), intent(in) :: x

  selectcase( x )
  case( INT1_LLIM:    -100_1 ); digit = 4
  case(     -99_1:     -10_1 ); digit = 3
  case(      -9_1:      -1_1 ); digit = 2
  case(       0_1:       9_1 ); digit = 1
  case(      10_1:      99_1 ); digit = 2
  case(     100_1: INT1_ULIM ); digit = 3
  endselect
end function dgt_int1_0d
!===============================================================
!
!===============================================================
integer pure function dgt_int2_0d(x) result(digit)
  integer(2), intent(in) :: x

  selectcase( x )
  case( INT2_LLIM: -10000_2 ); digit = 6
  case(   -9999_2:  -1000_2 ); digit = 5
  case(    -999_2:   -100_2 ); digit = 4
  case(     -99_2:    -10_2 ); digit = 3
  case(      -9_2:     -1_2 ); digit = 2
  case(       0_2:      9_2 ); digit = 1
  case(      10_2:     99_2 ); digit = 2
  case(     100_2:    999_2 ); digit = 3
  case(    1000_2:   9999_2 ); digit = 4
  case(   10000_2:INT2_ULIM ); digit = 5
  endselect
end function dgt_int2_0d
!===============================================================
!
!===============================================================
integer pure function dgt_int4_0d(x) result(digit)
  implicit none
  integer(4), intent(in) :: x

  selectcase( x )
  case( INT4_LLIM    :-1000000000_4 ); digit = 11
  case(  -999999999_4: -100000000_4 ); digit = 10
  case(   -99999999_4:  -10000000_4 ); digit =  9
  case(    -9999999_4:   -1000000_4 ); digit =  8
  case(     -999999_4:    -100000_4 ); digit =  7
  case(      -99999_4:     -10000_4 ); digit =  6
  case(       -9999_4:      -1000_4 ); digit =  5
  case(        -999_4:       -100_4 ); digit =  4
  case(         -99_4:        -10_4 ); digit =  3
  case(          -9_4:         -1_4 ); digit =  2
  case(           0_4:          9_4 ); digit =  1
  case(          10_4:         99_4 ); digit =  2
  case(         100_4:        999_4 ); digit =  3
  case(        1000_4:       9999_4 ); digit =  4
  case(       10000_4:      99999_4 ); digit =  5
  case(      100000_4:     999999_4 ); digit =  6
  case(     1000000_4:    9999999_4 ); digit =  7
  case(    10000000_4:   99999999_4 ); digit =  8
  case(   100000000_4:  999999999_4 ); digit =  9
  case(  1000000000_4: INT4_ULIM    ); digit = 10
  endselect
end function dgt_int4_0d
!===============================================================
!
!===============================================================
integer pure function dgt_int8_0d(x) result(digit)
  implicit none
  integer(8), intent(in) :: x

  selectcase( x )
  case( INT8_LLIM             :-1000000000000000000_8 ); digit = 20
  case(  -999999999999999999_8: -100000000000000000_8 ); digit = 19
  case(   -99999999999999999_8:  -10000000000000000_8 ); digit = 18
  case(    -9999999999999999_8:   -1000000000000000_8 ); digit = 17
  case(     -999999999999999_8:    -100000000000000_8 ); digit = 16
  case(      -99999999999999_8:     -10000000000000_8 ); digit = 15
  case(       -9999999999999_8:      -1000000000000_8 ); digit = 14
  case(        -999999999999_8:       -100000000000_8 ); digit = 13
  case(         -99999999999_8:        -10000000000_8 ); digit = 12
  case(          -9999999999_8:         -1000000000_8 ); digit = 11
  case(           -999999999_8:          -100000000_8 ); digit = 10
  case(            -99999999_8:           -10000000_8 ); digit =  9
  case(             -9999999_8:            -1000000_8 ); digit =  8
  case(              -999999_8:             -100000_8 ); digit =  7
  case(               -99999_8:              -10000_8 ); digit =  6
  case(                -9999_8:               -1000_8 ); digit =  5
  case(                 -999_8:                -100_8 ); digit =  4
  case(                  -99_8:                 -10_8 ); digit =  3
  case(                   -9_8:                  -1_8 ); digit =  2
  case(                    0_8:                   9_8 ); digit =  1
  case(                   10_8:                  99_8 ); digit =  2
  case(                  100_8:                 999_8 ); digit =  3
  case(                 1000_8:                9999_8 ); digit =  4
  case(                10000_8:               99999_8 ); digit =  5
  case(               100000_8:              999999_8 ); digit =  6
  case(              1000000_8:             9999999_8 ); digit =  7
  case(             10000000_8:            99999999_8 ); digit =  8
  case(            100000000_8:           999999999_8 ); digit =  9
  case(           1000000000_8:          9999999999_8 ); digit = 10
  case(          10000000000_8:         99999999999_8 ); digit = 11
  case(         100000000000_8:        999999999999_8 ); digit = 12
  case(        1000000000000_8:       9999999999999_8 ); digit = 13
  case(       10000000000000_8:      99999999999999_8 ); digit = 14
  case(      100000000000000_8:     999999999999999_8 ); digit = 15
  case(     1000000000000000_8:    9999999999999999_8 ); digit = 16
  case(    10000000000000000_8:   99999999999999999_8 ); digit = 17
  case(   100000000000000000_8:  999999999999999999_8 ); digit = 18
  case(  1000000000000000000_8: INT8_ULIM             ); digit = 19
  endselect
end function dgt_int8_0d
!===============================================================
!
!===============================================================
integer pure function dgt_int8_0d_log(x) result(digit)
  implicit none
  integer(8), intent(in) :: x

  selectcase( x )
  case( :-1_8 )
    digit = int(log(dble(-x))) + 2
  case( 0_8 )
    digit = 0
  case( 1_8: )
    digit = int(log(dble(x))) + 1
  endselect
end function dgt_int8_0d_log
!===============================================================
!
!===============================================================
integer pure function dgt_int1_1d(x, opt) result(digit)
  implicit none
  integer(1)  , intent(in)           :: x(:)
  character(*), intent(in), optional :: opt
  character(CLEN_KEY) :: opt_
  integer(4)          :: i

  opt_ = DGT_OPT_SUM
  if( present(opt) ) opt_ = lower(opt)

  selectcase( opt_ )
  case( DGT_OPT_SUM )
    digit = 0
    do i = 1, size(x)
      digit = digit + dgt(x(i))
    enddo
  case( DGT_OPT_MAX )
    digit = max(dgt(minval(x)), dgt(maxval(x)))
  case default
    digit = -1
  endselect
end function dgt_int1_1d
!===============================================================
!
!===============================================================
integer pure function dgt_int2_1d(x, opt) result(digit)
  implicit none
  integer(2)  , intent(in)           :: x(:)
  character(*), intent(in), optional :: opt
  character(CLEN_KEY) :: opt_
  integer(4)          :: i

  opt_ = DGT_OPT_SUM
  if( present(opt) ) opt_ = lower(opt)

  selectcase( opt_ )
  case( DGT_OPT_SUM )
    digit = 0
    do i = 1, size(x)
      digit = digit + dgt(x(i))
    enddo
  case( DGT_OPT_MAX )
    digit = max(dgt(minval(x)), dgt(maxval(x)))
  case default
    digit = -1
  endselect
end function dgt_int2_1d
!===============================================================
!
!===============================================================
integer pure function dgt_int4_1d(x, opt) result(digit)
  implicit none
  integer(4)  , intent(in)           :: x(:)
  character(*), intent(in), optional :: opt
  character(CLEN_KEY) :: opt_
  integer(4)          :: i

  opt_ = DGT_OPT_SUM
  if( present(opt) ) opt_ = lower(opt)

  selectcase( opt_ )
  case( DGT_OPT_SUM )
    digit = 0
    do i = 1, size(x)
      digit = digit + dgt(x(i))
    enddo
  case( DGT_OPT_MAX )
    digit = max(dgt(minval(x)), dgt(maxval(x)))
  case default
    digit = -1
  endselect
end function dgt_int4_1d
!===============================================================
!
!===============================================================
integer pure function dgt_int8_1d(x, opt) result(digit)
  implicit none
  integer(8)  , intent(in)           :: x(:)
  character(*), intent(in), optional :: opt
  character(CLEN_KEY) :: opt_
  integer(4)          :: i

  opt_ = DGT_OPT_SUM
  if( present(opt) ) opt_ = lower(opt)

  selectcase( opt_ )
  case( DGT_OPT_SUM )
    digit = 0
    do i = 1, size(x)
      digit = digit + dgt(x(i))
    enddo
  case( DGT_OPT_MAX )
    digit = max(dgt(minval(x)), dgt(maxval(x)))
  case default
    digit = -1
  endselect
end function dgt_int8_1d
!===============================================================
!
!===============================================================
integer pure function dgt_int1_2d(x, opt) result(digit)
  implicit none
  integer(1)  , intent(in)           :: x(:,:)
  character(*), intent(in), optional :: opt
  character(CLEN_KEY) :: opt_
  integer(4)          :: i, j

  opt_ = DGT_OPT_SUM
  if( present(opt) ) opt_ = lower(opt)

  selectcase( opt_ )
  case( DGT_OPT_SUM )
    digit = 0
    do j = 1, size(x,2)
      do i = 1, size(x,1)
        digit = digit + dgt(x(i,j))
      enddo
    enddo
  case( DGT_OPT_MAX )
    digit = max(dgt(minval(x)), dgt(maxval(x)))
  case default
    digit = -1
  endselect
end function dgt_int1_2d
!===============================================================
!
!===============================================================
integer pure function dgt_int2_2d(x, opt) result(digit)
  implicit none
  integer(2)  , intent(in)           :: x(:,:)
  character(*), intent(in), optional :: opt
  character(CLEN_KEY) :: opt_
  integer(4)          :: i, j

  opt_ = DGT_OPT_SUM
  if( present(opt) ) opt_ = lower(opt)

  selectcase( opt_ )
  case( DGT_OPT_SUM )
    digit = 0
    do j = 1, size(x,2)
      do i = 1, size(x,1)
        digit = digit + dgt(x(i,j))
      enddo
    enddo
  case( DGT_OPT_MAX )
    digit = max(dgt(minval(x)), dgt(maxval(x)))
  case default
    digit = -1
  endselect
end function dgt_int2_2d
!===============================================================
!
!===============================================================
integer pure function dgt_int4_2d(x, opt) result(digit)
  implicit none
  integer(4)  , intent(in)           :: x(:,:)
  character(*), intent(in), optional :: opt
  character(CLEN_KEY) :: opt_
  integer(4)          :: i, j

  opt_ = DGT_OPT_SUM
  if( present(opt) ) opt_ = lower(opt)

  selectcase( opt_ )
  case( DGT_OPT_SUM )
    digit = 0
    do j = 1, size(x,2)
      do i = 1, size(x,1)
        digit = digit + dgt(x(i,j))
      enddo
    enddo
  case( DGT_OPT_MAX )
    digit = max(dgt(minval(x)), dgt(maxval(x)))
  case default
    digit = -1
  endselect
end function dgt_int4_2d
!===============================================================
!
!===============================================================
integer pure function dgt_int8_2d(x, opt) result(digit)
  implicit none
  integer(8)  , intent(in)           :: x(:,:)
  character(*), intent(in), optional :: opt
  character(CLEN_KEY) :: opt_
  integer(4)          :: i, j

  opt_ = DGT_OPT_SUM
  if( present(opt) ) opt_ = lower(opt)

  selectcase( opt_ )
  case( DGT_OPT_SUM )
    digit = 0
    do j = 1, size(x,2)
      do i = 1, size(x,1)
        digit = digit + dgt(x(i,j))
      enddo
    enddo
  case( DGT_OPT_MAX )
    digit = max(dgt(minval(x)), dgt(maxval(x)))
  case default
    digit = -1
  endselect
end function dgt_int8_2d
!===============================================================
!
!===============================================================
integer pure function dgt_int1_3d(x, opt) result(digit)
  implicit none
  integer(1)  , intent(in)           :: x(:,:,:)
  character(*), intent(in), optional :: opt
  character(CLEN_KEY) :: opt_
  integer(4)          :: i1, i2, i3

  opt_ = DGT_OPT_SUM
  if( present(opt) ) opt_ = lower(opt)

  selectcase( opt_ )
  case( DGT_OPT_SUM )
    digit = 0
    do i3 = 1, size(x,3)
    do i2 = 1, size(x,2)
    do i1 = 1, size(x,1)
      digit = digit + dgt(x(i1,i2,i3))
    enddo
    enddo
    enddo
  case( DGT_OPT_MAX )
    digit = max(dgt(minval(x)), dgt(maxval(x)))
  case default
    digit = -1
  endselect
end function dgt_int1_3d
!===============================================================
!
!===============================================================
integer pure function dgt_int2_3d(x, opt) result(digit)
  implicit none
  integer(2)  , intent(in)           :: x(:,:,:)
  character(*), intent(in), optional :: opt
  character(CLEN_KEY) :: opt_
  integer(4)          :: i1, i2, i3

  opt_ = DGT_OPT_SUM
  if( present(opt) ) opt_ = lower(opt)

  selectcase( opt_ )
  case( DGT_OPT_SUM )
    digit = 0
    do i3 = 1, size(x,3)
    do i2 = 1, size(x,2)
    do i1 = 1, size(x,1)
      digit = digit + dgt(x(i1,i2,i3))
    enddo
    enddo
    enddo
  case( DGT_OPT_MAX )
    digit = max(dgt(minval(x)), dgt(maxval(x)))
  case default
    digit = -1
  endselect
end function dgt_int2_3d
!===============================================================
!
!===============================================================
integer pure function dgt_int4_3d(x, opt) result(digit)
  implicit none
  integer(4)  , intent(in)           :: x(:,:,:)
  character(*), intent(in), optional :: opt
  character(CLEN_KEY) :: opt_
  integer(4)          :: i1, i2, i3

  opt_ = DGT_OPT_SUM
  if( present(opt) ) opt_ = lower(opt)

  selectcase( opt_ )
  case( DGT_OPT_SUM )
    digit = 0
    do i3 = 1, size(x,3)
    do i2 = 1, size(x,2)
    do i1 = 1, size(x,1)
      digit = digit + dgt(x(i1,i2,i3))
    enddo
    enddo
    enddo
  case( DGT_OPT_MAX )
    digit = max(dgt(minval(x)), dgt(maxval(x)))
  case default
    digit = -1
  endselect
end function dgt_int4_3d
!===============================================================
!
!===============================================================
integer pure function dgt_int8_3d(x, opt) result(digit)
  implicit none
  integer(8)  , intent(in)           :: x(:,:,:)
  character(*), intent(in), optional :: opt
  character(CLEN_KEY) :: opt_
  integer(4)          :: i1, i2, i3

  opt_ = DGT_OPT_SUM
  if( present(opt) ) opt_ = lower(opt)

  selectcase( opt_ )
  case( DGT_OPT_SUM )
    digit = 0
    do i3 = 1, size(x,3)
    do i2 = 1, size(x,2)
    do i1 = 1, size(x,1)
      digit = digit + dgt(x(i1,i2,i3))
    enddo
    enddo
    enddo
  case( DGT_OPT_MAX )
    digit = max(dgt(minval(x)), dgt(maxval(x)))
  case default
    digit = -1
  endselect
end function dgt_int8_3d
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
integer pure function cl_fmt_float(wfmt) result(l)
  implicit none
  character(*), intent(in) :: wfmt
  character(CLEN_WFMT) :: wfmt_

  wfmt_ = wfmt_real
  if( wfmt /= '' ) wfmt_ = wfmt

  if( wfmt_(1:2) == 'es' )then
    read(wfmt_(3:index(wfmt_,'.')-1),*) l
  elseif( wfmt_(1:1) == 'e' .or. wfmt_(1:1) == 'f' )then
    read(wfmt_(2:index(wfmt_,'.')-1),*) l
  elseif( wfmt_(1:1) == 'b' )then
    read(wfmt_(2:index(wfmt_,'.')-1),*) l
  else
    l = 0
  endif
end function cl_fmt_float
!===============================================================
!
!===============================================================
integer pure function cl_char_0d(x, d) result(l)
  implicit none
  character(*), intent(in) :: x
  integer     , intent(in) :: d

  selectcase( d )
  case( 0 )
    l = len_trim(x)
  case default
    l = abs(d)
  endselect
end function cl_char_0d
!===============================================================
!
!===============================================================
integer pure function cl_char_1d(x, d, dlm) result(l)
  implicit none
  character(*), intent(in) :: x(:)
  integer     , intent(in) :: d
  character(*), intent(in) :: dlm
  integer :: i

  selectcase( d )
  case( 0 )
    l = len(dlm) * (size(x)-1)
    do i = 1, size(x)
      l = l + len_trim(x(i))
    enddo
  case default
    l = abs(d) * size(x) + len(dlm) * (size(x)-1)
  endselect
end function cl_char_1d
!===============================================================
!
!===============================================================
integer pure function cl_log1_1d(x, digit, dlm) result(l)
  implicit none
  logical(1)  , intent(in) :: x(:)
  integer     , intent(in) :: digit
  character(*), intent(in) :: dlm

  l = (max(digit,1) + len(dlm)) * size(x) - len(dlm)
end function cl_log1_1d
!===============================================================
!
!===============================================================
integer pure function cl_log4_1d(x, digit, dlm) result(l)
  implicit none
  logical(4)  , intent(in) :: x(:)
  integer     , intent(in) :: digit
  character(*), intent(in) :: dlm

  l = (max(digit,1) + len(dlm)) * size(x) - len(dlm)
end function cl_log4_1d
!===============================================================
!
!===============================================================
integer pure function cl_int1_0d(x, digit) result(l)
  implicit none
  integer(1), intent(in) :: x
  integer   , intent(in) :: digit

  selectcase( digit )
  case( 0 )
    l = dgt(x)
  case default
    l = abs(digit)
  endselect
end function cl_int1_0d
!===============================================================
!
!===============================================================
integer pure function cl_int1_1d(x, digit, dlm) result(l)
  implicit none
  integer(1)  , intent(in) :: x(:)
  integer     , intent(in) :: digit
  character(*), intent(in) :: dlm

  selectcase( digit )
  case( 0 )
    l = dgt(x) + len(dlm) * (size(x)-1)
  case default
    l = abs(digit) * size(x) + len(dlm) * (size(x)-1)
  endselect
end function cl_int1_1d
!===============================================================
!
!===============================================================
integer pure function cl_int2_0d(x, digit) result(l)
  implicit none
  integer(2), intent(in) :: x
  integer   , intent(in) :: digit

  selectcase( digit )
  case( 0 )
    l = dgt(x)
  case default
    l = abs(digit)
  endselect
end function cl_int2_0d
!===============================================================
!
!===============================================================
integer pure function cl_int2_1d(x, digit, dlm) result(l)
  implicit none
  integer(2)  , intent(in) :: x(:)
  integer     , intent(in) :: digit
  character(*), intent(in) :: dlm

  selectcase( digit )
  case( 0 )
    l = dgt(x) + len(dlm) * (size(x)-1)
  case default
    l = abs(digit) * size(x) + len(dlm) * (size(x)-1)
  endselect
end function cl_int2_1d
!===============================================================
!
!===============================================================
integer pure function cl_int4_0d(x, digit) result(l)
  implicit none
  integer(4), intent(in) :: x
  integer   , intent(in) :: digit

  selectcase( digit )
  case( 0 )
    l = dgt(x)
  case default
    l = abs(digit)
  endselect
end function cl_int4_0d
!===============================================================
!
!===============================================================
integer pure function cl_int4_1d(x, digit, dlm) result(l)
  implicit none
  integer(4)  , intent(in) :: x(:)
  integer     , intent(in) :: digit
  character(*), intent(in) :: dlm

  selectcase( digit )
  case( 0 )
    l = dgt(x) + len(dlm) * (size(x)-1)
  case default
    l = abs(digit) * size(x) + len(dlm) * (size(x)-1)
  endselect
end function cl_int4_1d
!===============================================================
!
!===============================================================
integer pure function cl_int8_0d(x, digit) result(l)
  implicit none
  integer(8), intent(in) :: x
  integer   , intent(in) :: digit

  selectcase( digit )
  case( 0 )
    l = dgt(x)
  case default
    l = abs(digit)
  endselect
end function cl_int8_0d
!===============================================================
!
!===============================================================
integer pure function cl_int8_1d(x, digit, dlm) result(l)
  implicit none
  integer(8)  , intent(in) :: x(:)
  integer     , intent(in) :: digit
  character(*), intent(in) :: dlm

  selectcase( digit )
  case( 0 )
    l = dgt(x) + len(dlm) * (size(x)-1)
  case default
    l = abs(digit) * size(x) + len(dlm) * (size(x)-1)
  endselect
end function cl_int8_1d
!===============================================================
!
!===============================================================
integer pure function cl_real_1d(x, fmt, dlm) result(l)
  implicit none
  real(4)     , intent(in) :: x(:)
  character(*), intent(in) :: fmt
  character(*), intent(in) :: dlm
  character(CLEN_WFMT) :: fmt_

  fmt_ = wfmt_real
  if( fmt /= '' ) fmt_ = fmt

  l = cl_fmt_float(fmt_) * size(x) + len(dlm) * (size(x)-1)
end function cl_real_1d
!===============================================================
!
!===============================================================
integer pure function cl_dble_1d(x, fmt, dlm) result(l)
  implicit none
  real(8)     , intent(in) :: x(:)
  character(*), intent(in) :: fmt
  character(*), intent(in) :: dlm
  character(CLEN_WFMT) :: fmt_

  fmt_ = wfmt_real
  if( fmt /= '' ) fmt_ = fmt

  l = cl_fmt_float(fmt_) * size(x) + len(dlm) * (size(x)-1)
end function cl_dble_1d
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
function str_char_0d_min(x) result(c)
  implicit none
  character(*), intent(in) :: x
  character(len_trim(x)) :: c

  c = x
end function str_char_0d_min
!===============================================================
!
!===============================================================
function str_char_0d_fmt_nofill(x, d) result(c)
  implicit none
  character(*), intent(in) :: x
  integer     , intent(in) :: d
  character(cl_char_0d(x,d)) :: c

  c = x
end function str_char_0d_fmt_nofill
!===============================================================
!
!===============================================================
function str_char_0d_fmt_fill(x, d, fill) result(c)
  implicit none
  character(*), intent(in) :: x
  integer     , intent(in) :: d
  character(*), intent(in) :: fill
  character(cl_char_0d(x,d)) :: c

  integer :: len_fill
  integer :: len_x
  integer :: i0

  len_fill = len(fill)

  selectcase( d )
  case( 0 )
    continue
  case( 1: )
    c = x
    i0 = len(x)
    do while( i0+1 < d+1 )
      c(i0+1:min(i0+len_fill,d)) = fill
      i0 = i0 + len_fill
    enddo
  case( :-1 )
    len_x = len(x)
    i0 = 0
    do while( i0+1 < -d-len_x+1 )
      c(i0+1:i0+len_fill) = fill
      i0 = i0 + len_fill
    enddo
    c(-d-len_x+1:) = x
  endselect
end function str_char_0d_fmt_fill
!===============================================================
!
!===============================================================
function str_char_1d_min_nodlm(x) result(c)
  implicit none
  character(*), intent(in) :: x(:)
  character(cl_char_1d(x,0,' ')) :: c

  integer :: i
  integer :: l0
  integer :: d

  c = ''
  l0 = 0
  do i = 1, size(x)
    d = len_trim(x(i))
    c(l0+1:l0+d) = x(i)
    l0 = l0 + d + 1
  enddo
end function str_char_1d_min_nodlm
!===============================================================
!
!===============================================================
function str_char_1d_min_dlm(x, dlm) result(c)
  implicit none
  character(*), intent(in) :: x(:)
  character(*), intent(in) :: dlm
  character(cl_char_1d(x,0,dlm)) :: c

  integer :: i
  integer :: l0
  integer :: len_dlm
  integer :: len_x

  len_dlm = len(dlm)

  l0 = 0
  do i = 1, size(x)
    if( i > 1 ) c(l0-len_dlm+1:l0) = dlm
    len_x = len_trim(x(i))
    c(l0+1:l0+len_x) = x(i)
    l0 = l0 + len_x + len_dlm
  enddo
end function str_char_1d_min_dlm
!===============================================================
! d = 0: call str_char_1d_min_nodlm
!   > 0: Left-aligned, divided by ' '
!   < 0: Right-aligned, divided by ' '
!===============================================================
function str_char_1d_fmt_nodlm_nofill(x, d) result(c)
  implicit none
  character(*), intent(in) :: x(:)
  integer     , intent(in) :: d
  character(cl_char_1d(x,d,' ')) :: c

  integer :: i
  integer :: l0

  selectcase( d )
  case( 0 )
    c = str_char_1d_min_nodlm(x)
  case( 1: )
    c = ''
    l0 = 0
    do i = 1, size(x)
      c(l0+1:l0+d) = x(i)
      l0 = l0 + d + 1
    enddo
  case( :-1 )
    c = ''
    l0 = 0
    do i = 1, size(x)
      c(l0+max(0,-d-len_trim(x(i)))+1:l0-d) = x(i)
      l0 = l0 - d + 1
    enddo
  endselect
end function str_char_1d_fmt_nodlm_nofill
!===============================================================
! d = 0: call str_char_1d_min_dlm
!   > 0: Left-aligned, divided by $dlm
!   < 0: Right-aligned, divided by $dlm
!===============================================================
function str_char_1d_fmt_dlm_nofill(x, d, dlm) result(c)
  implicit none
  character(*), intent(in) :: x(:)
  integer     , intent(in) :: d
  character(*), intent(in) :: dlm
  character(cl_char_1d(x,d,dlm)) :: c

  integer :: i
  integer :: l0
  integer :: len_dlm

  len_dlm = len(dlm)

  selectcase( d )
  case( 0 )
    c = str_char_1d_min_dlm(x, dlm)
  case( 1: )
    c = ''
    l0 = 0
    do i = 1, size(x)
      if( i > 1 ) c(l0-len_dlm+1:l0) = dlm
      c(l0+1:l0+d) = x(i)
      l0 = l0 + d + len_dlm
    enddo
  case( :-1 )
    c = ''
    l0 = 0
    do i = 1, size(x)
      if( i > 1 ) c(l0-len_dlm+1:l0) = dlm
      c(l0+max(0,-d-len_trim(x(i)))+1:l0-d) = x(i)
      l0 = l0 - d + len_dlm
    enddo
  endselect
end function str_char_1d_fmt_dlm_nofill
!===============================================================
! d = 0: call str_char_1d_min_dlm
!   > 0: Left-aligned, divided by $dlm, spaces are filled by $fill
!   < 0: Right-aligned, divided by $dlm, spaces are filled by $fill
!===============================================================
function str_char_1d_fmt_dlm_fill(x, d, dlm, fill) result(c)
  implicit none
  character(*), intent(in) :: x(:)
  integer     , intent(in) :: d
  character(*), intent(in) :: dlm
  character(*), intent(in) :: fill
  character(cl_char_1d(x,d,dlm)) :: c

  integer :: i
  integer :: l0, l1
  integer :: len_c, len_dlm, len_fill
  integer :: len_x

  len_c = len(c)
  len_dlm = len(dlm)
  len_fill = len(fill)

  selectcase( d )
  case( 0 )
  case( 1: )
    l0 = 0
    do i = 1, size(x)
      if( i > 1 ) c(l0-len_dlm+1:l0) = dlm

      len_x = len_trim(x(i))
      if( len_x < d )then
        c(l0+1:l0+len_x) = x(i)
        l1 = l0 + d
        l0 = l0 + len_x
        do while( l0 < l1 )
          c(l0+1:min(l0+len_fill,l1)) = fill
          l0 = l0 + len_fill
        enddo
        l0 = l1
      else
        c(l0+1:l0+d) = x(i)
        l0 = l0 + d
      endif

      l0 = l0 + len_dlm
    enddo  ! i/
  case( :-1 )
    l0 = 0
    do i = 1, size(x)
      if( i > 1 ) c(l0-len_dlm+1:l0) = dlm

      len_x = len_trim(x(i))
      if( len_x < -d )then
        l1 = l0 - d
        do while( l0 < l1 )
          c(l0+1:min(l0+len_fill,l1)) = fill
          l0 = l0 + len_fill
        enddo
        c(l1-len_x+1:l1) = x(i)
        l0 = l1
      else
        c(l0+1:l0-d) = x(i)
        l0 = l0 - d
      endif

      l0 = l0 + len_dlm
    enddo  ! i/
  endselect
end function str_char_1d_fmt_dlm_fill
!===============================================================
!
!===============================================================
function str_log1_0d_min(x) result(c)
  implicit none
  logical(1), intent(in) :: x
  character(1)           :: c

  write(c,"(l1)") x
end function str_log1_0d_min
!===============================================================
!
!===============================================================
function str_log1_0d_fmt(x, d) result(c)
  implicit none
  logical(1), intent(in) :: x
  integer   , intent(in) :: d
  character(max(1,d))    :: c

  write(c,"(l"//str(max(1,d))//")") x
end function str_log1_0d_fmt
!===============================================================
!
!===============================================================
function str_log1_1d_min_nodlm(x) result(c)
  implicit none
  logical(1), intent(in) :: x(:)
  character(cl_log1_1d(x,0,' ')) :: c

  write(c,"(l1,"//str(size(x)-1)//"(1x,l1))") x
end function str_log1_1d_min_nodlm
!===============================================================
!
!===============================================================
function str_log1_1d_min_dlm(x, dlm) result(c)
  implicit none
  logical(1)  , intent(in) :: x(:)
  character(*), intent(in) :: dlm
  character(cl_log1_1d(x,0,dlm)) :: c

  write(c,"(l1,"//str(size(x)-1)//"('"//dlm//"',l1))") x
end function str_log1_1d_min_dlm
!===============================================================
!
!===============================================================
function str_log1_1d_fmt_nodlm(x, d) result(c)
  implicit none
  logical(1), intent(in) :: x(:)
  integer   , intent(in) :: d
  character(cl_log1_1d(x,d,' ')) :: c

  write(c,"(l"//str(max(d,1))//","//str(size(x)-1)//"(1x,l"//str(max(d,1))//"))") x
end function str_log1_1d_fmt_nodlm
!===============================================================
!
!===============================================================
function str_log1_1d_fmt_dlm(x,d,dlm) result(c)
  implicit none
  logical(1)  , intent(in) :: x(:)
  integer     , intent(in) :: d
  character(*), intent(in) :: dlm
  character(cl_log1_1d(x,d,dlm)) :: c

  write(c,"(l"//str(max(d,1))//","//str(size(x)-1)//"('"//dlm//"',l"//str(max(d,1))//"))") x
end function str_log1_1d_fmt_dlm
!===============================================================
!
!===============================================================
function str_log4_0d_min(x) result(c)
  implicit none
  logical(4), intent(in) :: x
  character(1)           :: c

  write(c,"(l1)") x
end function str_log4_0d_min
!===============================================================
!
!===============================================================
function str_log4_0d_fmt(x, d) result(c)
  implicit none
  logical(4), intent(in) :: x
  integer   , intent(in) :: d
  character(max(1,d))    :: c

  write(c,"(l"//str(max(1,d))//")") x
end function str_log4_0d_fmt
!===============================================================
!
!===============================================================
function str_log4_1d_min_nodlm(x) result(c)
  implicit none
  logical(4), intent(in) :: x(:)
  character(cl_log4_1d(x,0,' ')) :: c

  write(c,"(l1,"//str(size(x)-1)//"(1x,l1))") x
end function str_log4_1d_min_nodlm
!===============================================================
!
!===============================================================
function str_log4_1d_min_dlm(x, dlm) result(c)
  implicit none
  logical(4)  , intent(in) :: x(:)
  character(*), intent(in) :: dlm
  character(cl_log4_1d(x,0,dlm)) :: c

  write(c,"(l1,"//str(size(x)-1)//"('"//dlm//"',l1))") x
end function str_log4_1d_min_dlm
!===============================================================
!
!===============================================================
function str_log4_1d_fmt_nodlm(x, d) result(c)
  implicit none
  logical(4), intent(in) :: x(:)
  integer   , intent(in) :: d
  character(cl_log4_1d(x,d,' ')) :: c

  write(c,"(l"//str(max(d,1))//","//str(size(x)-1)//"(1x,l"//str(max(d,1))//"))") x
end function str_log4_1d_fmt_nodlm
!===============================================================
!
!===============================================================
function str_log4_1d_fmt_dlm(x,d,dlm) result(c)
  implicit none
  logical(4)  , intent(in) :: x(:)
  integer     , intent(in) :: d
  character(*), intent(in) :: dlm
  character(cl_log4_1d(x,d,dlm)) :: c

  write(c,"(l"//str(max(d,1))//","//str(size(x)-1)//"('"//dlm//"',l"//str(max(d,1))//"))") x
end function str_log4_1d_fmt_dlm
!===============================================================
!
!===============================================================
function str_int1_0d_min(x) result(c)
  implicit none
  integer(1), intent(in) :: x
  character(dgt(x))      :: c

  write(c,"(i0)") x
end function str_int1_0d_min
!===============================================================
!
!===============================================================
function str_int1_0d_fmt(x, d) result(c)
  implicit none
  integer(1)  , intent(in)  :: x
  integer     , intent(in)  :: d
  character(cl_int1_0d(x,d)) :: c
  character(128) :: c_dgt
  integer :: i

  selectcase( d )
  case( 0 )
    write(c,"(i0)") x
  case( 1: )
    if( dgt(x) > d )then
      do i = 1, len(c)
        c(i:i) = '*'
      enddo
    else
      write(c,"(i"//str(d)//")") x
    endif
  case( :-1 )
    if( dgt(x) > -d )then
      do i = 1, len(c)
        c(i:i) = '*'
      enddo
    else
      if( x >= 0 )then
        write(c_dgt,"(i0)") -d
        write(c,"(i"//trim(c_dgt)//"."//trim(c_dgt)//")") x
      else
        write(c_dgt,"(i0)") -d-1
        write(c,"('-',i"//trim(c_dgt)//"."//trim(c_dgt)//")") -x
      endif
    endif
  endselect
end function str_int1_0d_fmt
!===============================================================
!
!===============================================================
function str_int1_1d_min_nodlm(x) result(c)
  implicit none
  integer(1), intent(in) :: x(:)
  character(cl_int1_1d(x,0,' ')) :: c
  integer :: l, ll
  integer :: i
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int1_1d_min_nodlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    write(c,"(i0)") x(1)
  case( 2: )
    c = ''
    l = dgt(x(1))
    write(c(1:l),"(i0)") x(1)
    do i = 2, size(x)
      ll = l + 1 + dgt(x(i))
      write(c(l+2:ll),"(i0)") x(i)
      l = ll
    enddo
  endselect
end function str_int1_1d_min_nodlm
!===============================================================
!
!===============================================================
function str_int1_1d_min_dlm(x, dlm) result(c)
  implicit none
  integer(1)  , intent(in) :: x(:)
  character(*), intent(in) :: dlm
  character(cl_int1_1d(x,0,dlm)) :: c
  integer :: i
  integer :: l, ll, l_dlm
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int1_1d_min_dlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str(x(1))
  case( 2: )
    l_dlm = len(dlm)
    c = ''
    l = dgt(x(1))
    write(c(1:l),"(i0)") x(1)
    do i = 2, size(x)
      ll = l + l_dlm + dgt(x(i))
      write(c(l+1:ll),"(a,i0)") dlm, x(i)
      l = ll
    enddo
  endselect
end function str_int1_1d_min_dlm
!===============================================================
!
!===============================================================
function str_int1_1d_fmt_nodlm(x, d) result(c)
  implicit none
  integer(1), intent(in) :: x(:)
  integer   , intent(in) :: d
  character(cl_int1_1d(x,d,' ')) :: c
  integer :: i
  integer :: l, ll
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int1_1d_fmt_nodlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc), '-p')
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str(x(1), d)
  case default
    if( d == 0 )then
      c = str(x)
    else
      c(1:abs(d)) = str(x(1),d)
      l = abs(d)
      do i = 2, size(x)
        ll = l + 1 + abs(d)
        c(l+1:ll) = ' '//str(x(i),d)
        l = ll
      enddo
    endif
  endselect
end function str_int1_1d_fmt_nodlm
!===============================================================
!
!===============================================================
function str_int1_1d_fmt_dlm(x, d, dlm) result(c)
  implicit none
  integer(1)  , intent(in) :: x(:)
  integer     , intent(in) :: d
  character(*), intent(in) :: dlm
  character(cl_int1_1d(x,d,dlm)) :: c
  integer :: i
  integer :: l, ll, l_dlm
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int1_1d_fmt_dlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str(x(1), d)
  case( 2: )
    if( d == 0 )then
      c = str(x, dlm)
    else
      l_dlm = len(dlm)
      c(1:abs(d)) = str(x(1),d)
      l = abs(d)
      do i = 2, size(x)
        ll = l + l_dlm + abs(d)
        c(l+1:ll) = dlm//str(x(i),d)
        l = ll
      enddo
    endif
  endselect
end function str_int1_1d_fmt_dlm
!===============================================================
!
!===============================================================
function str_int2_0d_min(x) result(c)
  implicit none
  integer(2), intent(in) :: x
  character(dgt(x))      :: c

  write(c,"(i0)") x
end function str_int2_0d_min
!===============================================================
!
!===============================================================
function str_int2_0d_fmt(x, d) result(c)
  implicit none
  integer(2)  , intent(in)  :: x
  integer     , intent(in)  :: d
  character(cl_int2_0d(x,d)) :: c
  character(128) :: c_dgt
  integer :: i

  selectcase( d )
  case( 0 )
    write(c,"(i0)") x
  case( 1: )
    if( dgt(x) > d )then
      do i = 1, len(c)
        c(i:i) = '*'
      enddo
    else
      write(c,"(i"//str(d)//")") x
    endif
  case( :-1 )
    if( dgt(x) > -d )then
      do i = 1, len(c)
        c(i:i) = '*'
      enddo
    else
      if( x >= 0 )then
        write(c_dgt,"(i0)") -d
        write(c,"(i"//trim(c_dgt)//"."//trim(c_dgt)//")") x
      else
        write(c_dgt,"(i0)") -d-1
        write(c,"('-',i"//trim(c_dgt)//"."//trim(c_dgt)//")") -x
      endif
    endif
  endselect
end function str_int2_0d_fmt
!===============================================================
!
!===============================================================
function str_int2_1d_min_nodlm(x) result(c)
  implicit none
  integer(2), intent(in) :: x(:)
  character(cl_int2_1d(x,0,' ')) :: c
  integer :: l, ll
  integer :: i
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int2_1d_min_nodlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    write(c,"(i0)") x(1)
  case( 2: )
    c = ''
    l = dgt(x(1))
    write(c(1:l),"(i0)") x(1)
    do i = 2, size(x)
      ll = l + 1 + dgt(x(i))
      write(c(l+2:ll),"(i0)") x(i)
      l = ll
    enddo
  endselect
end function str_int2_1d_min_nodlm
!===============================================================
!
!===============================================================
function str_int2_1d_min_dlm(x, dlm) result(c)
  implicit none
  integer(2)  , intent(in) :: x(:)
  character(*), intent(in) :: dlm
  character(cl_int2_1d(x,0,dlm)) :: c
  integer :: i
  integer :: l, ll, l_dlm
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int2_1d_min_dlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str(x(1))
  case( 2: )
    l_dlm = len(dlm)
    c = ''
    l = dgt(x(1))
    write(c(1:l),"(i0)") x(1)
    do i = 2, size(x)
      ll = l + l_dlm + dgt(x(i))
      write(c(l+1:ll),"(a,i0)") dlm, x(i)
      l = ll
    enddo
  endselect
end function str_int2_1d_min_dlm
!===============================================================
!
!===============================================================
function str_int2_1d_fmt_nodlm(x, d) result(c)
  implicit none
  integer(2), intent(in) :: x(:)
  integer   , intent(in) :: d
  character(cl_int2_1d(x,d,' ')) :: c
  integer :: i
  integer :: l, ll
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int2_1d_fmt_nodlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str(x(1), d)
  case default
    if( d == 0 )then
      c = str(x)
    else
      c(1:abs(d)) = str(x(1),d)
      l = abs(d)
      do i = 2, size(x)
        ll = l + 1 + abs(d)
        c(l+1:ll) = ' '//str(x(i),d)
        l = ll
      enddo
    endif
  endselect
end function str_int2_1d_fmt_nodlm
!===============================================================
!
!===============================================================
function str_int2_1d_fmt_dlm(x, d, dlm) result(c)
  implicit none
  integer(2)  , intent(in) :: x(:)
  integer     , intent(in) :: d
  character(*), intent(in) :: dlm
  character(cl_int2_1d(x,d,dlm)) :: c
  integer :: i
  integer :: l, ll, l_dlm
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int2_1d_fmt_dlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str(x(1), d)
  case( 2: )
    if( d == 0 )then
      c = str(x, dlm)
    else
      l_dlm = len(dlm)
      c(1:abs(d)) = str(x(1),d)
      l = abs(d)
      do i = 2, size(x)
        ll = l + l_dlm + abs(d)
        c(l+1:ll) = dlm//str(x(i),d)
        l = ll
      enddo
    endif
  endselect
end function str_int2_1d_fmt_dlm
!===============================================================
!
!===============================================================
function str_int4_0d_min(x) result(c)
  implicit none
  integer(4), intent(in) :: x
  character(dgt(x))      :: c

  write(c,"(i0)") x
end function str_int4_0d_min
!===============================================================
!
!===============================================================
function str_int4_0d_fmt(x, d) result(c)
  implicit none
  integer(4)  , intent(in)  :: x
  integer     , intent(in)  :: d
  character(cl_int4_0d(x,d)) :: c
  character(128) :: c_dgt
  integer :: i

  selectcase( d )
  case( 0 )
    write(c,"(i0)") x
  case( 1: )
    if( dgt(x) > d )then
      do i = 1, len(c)
        c(i:i) = '*'
      enddo
    else
      write(c,"(i"//str(d)//")") x
    endif
  case( :-1 )
    if( dgt(x) > -d )then
      do i = 1, len(c)
        c(i:i) = '*'
      enddo
    else
      if( x >= 0 )then
        write(c_dgt,"(i0)") -d
        write(c,"(i"//trim(c_dgt)//"."//trim(c_dgt)//")") x
      else
        write(c_dgt,"(i0)") -d-1
        write(c,"('-',i"//trim(c_dgt)//"."//trim(c_dgt)//")") -x
      endif
    endif
  endselect
end function str_int4_0d_fmt
!===============================================================
!
!===============================================================
function str_int4_1d_min_nodlm(x) result(c)
  implicit none
  integer(4), intent(in) :: x(:)
  character(cl_int4_1d(x,0,' ')) :: c
  integer :: l, ll
  integer :: i
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int4_1d_min_nodlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    write(c,"(i0)") x(1)
  case( 2: )
    c = ''
    l = dgt(x(1))
    write(c(1:l),"(i0)") x(1)
    do i = 2, size(x)
      ll = l + 1 + dgt(x(i))
      write(c(l+2:ll),"(i0)") x(i)
      l = ll
    enddo
  endselect
end function str_int4_1d_min_nodlm
!===============================================================
!
!===============================================================
function str_int4_1d_min_dlm(x, dlm) result(c)
  implicit none
  integer(4)  , intent(in) :: x(:)
  character(*), intent(in) :: dlm
  character(cl_int4_1d(x,0,dlm)) :: c
  integer :: i
  integer :: l, ll, l_dlm
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int4_1d_min_dlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str(x(1))
  case( 2: )
    l_dlm = len(dlm)
    c = ''
    l = dgt(x(1))
    write(c(1:l),"(i0)") x(1)
    do i = 2, size(x)
      ll = l + l_dlm + dgt(x(i))
      write(c(l+1:ll),"(a,i0)") dlm, x(i)
      l = ll
    enddo
  endselect
end function str_int4_1d_min_dlm
!===============================================================
!
!===============================================================
function str_int4_1d_fmt_nodlm(x, d) result(c)
  implicit none
  integer(4), intent(in) :: x(:)
  integer   , intent(in) :: d
  character(cl_int4_1d(x,d,' ')) :: c
  integer :: i
  integer :: l, ll
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int4_1d_fmt_nodlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str(x(1), d)
  case default
    if( d == 0 )then
      c = str(x)
    else
      c(1:abs(d)) = str(x(1),d)
      l = abs(d)
      do i = 2, size(x)
        ll = l + 1 + abs(d)
        c(l+1:ll) = ' '//str(x(i),d)
        l = ll
      enddo
    endif
  endselect
end function str_int4_1d_fmt_nodlm
!===============================================================
!
!===============================================================
function str_int4_1d_fmt_dlm(x, d, dlm) result(c)
  implicit none
  integer(4)  , intent(in) :: x(:)
  integer     , intent(in) :: d
  character(*), intent(in) :: dlm
  character(cl_int4_1d(x,d,dlm)) :: c
  integer :: i
  integer :: l, ll, l_dlm
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int4_1d_fmt_dlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str(x(1), d)
  case( 2: )
    if( d == 0 )then
      c = str(x, dlm)
    else
      l_dlm = len(dlm)
      c(1:abs(d)) = str(x(1),d)
      l = abs(d)
      do i = 2, size(x)
        ll = l + l_dlm + abs(d)
        c(l+1:ll) = dlm//str(x(i),d)
        l = ll
      enddo
    endif
  endselect
end function str_int4_1d_fmt_dlm
!===============================================================
!
!===============================================================
function str_int8_0d_min(x) result(c)
  implicit none
  integer(8), intent(in) :: x
  character(dgt(x))      :: c

  write(c,"(i0)") x
end function str_int8_0d_min
!===============================================================
!
!===============================================================
function str_int8_0d_fmt(x, d) result(c)
  implicit none
  integer(8)  , intent(in)  :: x
  integer     , intent(in)  :: d
  character(cl_int8_0d(x,d)) :: c
  character(128) :: c_dgt
  integer :: i

  selectcase( d )
  case( 0 )
    write(c,"(i0)") x
  case( 1: )
    if( dgt(x) > d )then
      do i = 1, len(c)
        c(i:i) = '*'
      enddo
    else
      write(c,"(i"//str(d)//")") x
    endif
  case( :-1 )
    if( dgt(x) > -d )then
      do i = 1, len(c)
        c(i:i) = '*'
      enddo
    else
      if( x >= 0 )then
        write(c_dgt,"(i0)") -d
        write(c,"(i"//trim(c_dgt)//"."//trim(c_dgt)//")") x
      else
        write(c_dgt,"(i0)") -d-1
        write(c,"('-',i"//trim(c_dgt)//"."//trim(c_dgt)//")") -x
      endif
    endif
  endselect
end function str_int8_0d_fmt
!===============================================================
!
!===============================================================
function str_int8_1d_min_nodlm(x) result(c)
  implicit none
  integer(8), intent(in) :: x(:)
  character(cl_int8_1d(x,0,' ')) :: c
  integer :: l, ll
  integer :: i
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int8_1d_min_nodlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    write(c,"(i0)") x(1)
  case( 2: )
    c = ''
    l = dgt(x(1))
    write(c(1:l),"(i0)") x(1)
    do i = 2, size(x)
      ll = l + 1 + dgt(x(i))
      write(c(l+2:ll),"(i0)") x(i)
      l = ll
    enddo
  endselect
end function str_int8_1d_min_nodlm
!===============================================================
!
!===============================================================
function str_int8_1d_min_dlm(x, dlm) result(c)
  implicit none
  integer(8)  , intent(in) :: x(:)
  character(*), intent(in) :: dlm
  character(cl_int8_1d(x,0,dlm)) :: c
  integer :: i
  integer :: l, ll, l_dlm
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int8_1d_min_dlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str(x(1))
  case( 2: )
    l_dlm = len(dlm)
    c = ''
    l = dgt(x(1))
    write(c(1:l),"(i0)") x(1)
    do i = 2, size(x)
      ll = l + l_dlm + dgt(x(i))
      write(c(l+1:ll),"(a,i0)") dlm, x(i)
      l = ll
    enddo
  endselect
end function str_int8_1d_min_dlm
!===============================================================
!
!===============================================================
function str_int8_1d_fmt_nodlm(x, d) result(c)
  implicit none
  integer(8), intent(in) :: x(:)
  integer   , intent(in) :: d
  character(cl_int8_1d(x,d,' ')) :: c
  integer :: i
  integer :: l, ll
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int8_1d_fmt_nodlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str(x(1), d)
  case default
    if( d == 0 )then
      c = str(x)
    else
      c(1:abs(d)) = str(x(1),d)
      l = abs(d)
      do i = 2, size(x)
        ll = l + 1 + abs(d)
        c(l+1:ll) = ' '//str(x(i),d)
        l = ll
      enddo
    endif
  endselect
end function str_int8_1d_fmt_nodlm
!===============================================================
!
!===============================================================
function str_int8_1d_fmt_dlm(x, d, dlm) result(c)
  implicit none
  integer(8)  , intent(in) :: x(:)
  integer     , intent(in) :: d
  character(*), intent(in) :: dlm
  character(cl_int8_1d(x,d,dlm)) :: c
  integer :: i
  integer :: l, ll, l_dlm
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_int8_1d_fmt_dlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str(x(1), d)
  case( 2: )
    if( d == 0 )then
      c = str(x, dlm)
    else
      l_dlm = len(dlm)
      c(1:d) = str(x(1),d)
      l = abs(d)
      do i = 2, size(x)
        ll = l + l_dlm + abs(d)
        c(l+1:ll) = dlm//str(x(i),d)
        l = ll
      enddo
    endif
  endselect
end function str_int8_1d_fmt_dlm
!===============================================================
!
!===============================================================
function str_real_0d_nofmt(x) result(c)
  implicit none
  real(4), intent(in) :: x
  character(cl_fmt_float('')) :: c

  write(c,'('//trim(wfmt_real)//')') x
end function str_real_0d_nofmt
!===============================================================
!
!===============================================================
function str_real_0d_fmt(x, fmt) result(c)
  implicit none
  real(4)     , intent(in) :: x
  character(*), intent(in) :: fmt
  character(cl_fmt_float(fmt)) :: c

  write(c,'('//trim(fmt)//')') x
end function str_real_0d_fmt
!===============================================================
!
!===============================================================
function str_real_1d_nofmt_nodlm(x) result(c)
  implicit none
  real(4), intent(in) :: x(:)
  character(cl_real_1d(x,'',' ')) :: c
  character(CLEN_WFMT) :: wfmt
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_real_1d_nofmt_nodlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str_real_0d_nofmt(x(1))
  case( 2: )
    write(wfmt,"('(',a,',',i0,'(1x,',a,'))')") &
          wfmt_real, size(x)-1, wfmt_real
    write(c,wfmt) x
  endselect
end function str_real_1d_nofmt_nodlm
!===============================================================
!
!===============================================================
function str_real_1d_fmt_nodlm(x, fmt) result(c)
  implicit none
  real(4)     , intent(in) :: x(:)
  character(*), intent(in) :: fmt
  character(cl_real_1d(x,fmt,' ')) :: c
  character(CLEN_WFMT) :: fmt_
  character(CLEN_WFMT) :: wfmt
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_real_1d_fmt_nodlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str_real_0d_fmt(x(1), fmt)
  case( 2: )
    fmt_ = wfmt_real
    if( fmt /= '' ) fmt_ = fmt
    write(wfmt,"('(',a,',',i0,'(1x,',a,'))')") &
          trim(fmt_), size(x)-1, trim(fmt_)
    write(c,wfmt) x
  endselect
end function str_real_1d_fmt_nodlm
!===============================================================
!
!===============================================================
function str_real_1d_fmt_dlm(x, fmt, dlm) result(c)
  implicit none
  real(4)     , intent(in) :: x(:)
  character(*), intent(in) :: fmt
  character(*), intent(in) :: dlm
  character(cl_real_1d(x,fmt,dlm)) :: c
  character(CLEN_WFMT) :: fmt_
  character(CLEN_WFMT) :: wfmt
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_real_1d_fmt_dlm'

  selectcase( size(x) )
  case( 0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str_real_0d_fmt(x(1), fmt)
  case( 2: )
    fmt_ = wfmt_real
    if( fmt /= '' ) fmt_ = fmt
    write(wfmt,"('(',a,',',i0,'(',a,',',a,'))')") &
          trim(fmt_), size(x)-1, "'"//dlm//"'", trim(fmt_)
    write(c,wfmt) x
  endselect
end function str_real_1d_fmt_dlm
!===============================================================
!
!===============================================================
function str_dble_0d_nofmt(x) result(c)
  implicit none
  real(8), intent(in) :: x
  character(cl_fmt_float('')) :: c

  write(c,'('//trim(wfmt_real)//')') x
end function str_dble_0d_nofmt
!===============================================================
!
!===============================================================
function str_dble_0d_fmt(x, fmt) result(c)
  implicit none
  real(8)     , intent(in) :: x
  character(*), intent(in) :: fmt
  character(cl_fmt_float(fmt)) :: c

  write(c,'('//trim(fmt)//')') x
end function str_dble_0d_fmt
!===============================================================
!
!===============================================================
function str_dble_1d_nofmt_nodlm(x) result(c)
  implicit none
  real(8), intent(in) :: x(:)
  character(cl_dble_1d(x,'',' ')) :: c
  character(CLEN_WFMT) :: wfmt
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_dble_1d_nofmt_nodlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str_dble_0d_nofmt(x(1))
  case( 2: )
    write(wfmt,"('(',a,',',i0,'(1x,',a,'))')") &
          trim(wfmt_real), size(x)-1, trim(wfmt_real)
    write(c,wfmt) x
  endselect
end function str_dble_1d_nofmt_nodlm
!===============================================================
!
!===============================================================
function str_dble_1d_fmt_nodlm(x, fmt) result(c)
  implicit none
  real(8)     , intent(in) :: x(:)
  character(*), intent(in) :: fmt
  character(cl_dble_1d(x,fmt,' ')) :: c
  character(CLEN_WFMT) :: fmt_
  character(CLEN_WFMT) :: wfmt
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_real_1d_fmt_nodlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str_dble_0d_fmt(x(1), fmt)
  case( 2: )
    fmt_ = wfmt_real
    if( fmt /= '' ) fmt_ = fmt
    write(wfmt,"('(',a,',',i0,'(1x,',a,'))')") &
          trim(fmt_), size(x)-1, trim(fmt_)
    write(c,wfmt) x
  endselect
end function str_dble_1d_fmt_nodlm
!===============================================================
!
!===============================================================
function str_dble_1d_fmt_dlm(x, fmt, dlm) result(c)
  implicit none
  real(8)     , intent(in) :: x(:)
  character(*), intent(in) :: fmt
  character(*), intent(in) :: dlm
  character(cl_dble_1d(x,fmt,dlm)) :: c
  character(CLEN_WFMT) :: fmt_
  character(CLEN_WFMT) :: wfmt
  character(CLEN_VAR), parameter :: proc = 'str__mp__str_dble_1d_fmt_dlm'

  selectcase( size(x) )
  case( :0 )
    call echo(CODE%BGN, trim(proc))
    call eerr('Size of array is invalid.')
    call echo(CODE%RET)
  case( 1 )
    c = str_dble_0d_fmt(x(1), fmt)
  case( 2: )
    fmt_ = wfmt_real
    if( fmt /= '' ) fmt_ = fmt
    write(wfmt,"('(',a,',',i0,'(',a,',',a,'))')") &
          trim(fmt_), size(x)-1, "'"//dlm//"'", trim(fmt_)
    write(c,wfmt) x
  endselect
end function str_dble_1d_fmt_dlm
!===============================================================
!
!===============================================================
end module lib_log_str
