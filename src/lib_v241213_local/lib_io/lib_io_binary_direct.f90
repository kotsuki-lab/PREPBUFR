module lib_io_binary_direct
  use lib_const
  use lib_log
  use lib_io_base, only: &
    unit_number, &
    byte_of_dtype, &
    endian_long_name
  use lib_io_file, only: &
    filesize, &
    check_permission, &
    make_empty_file, &
    remove
  use lib_io_binary_common, only: &
    open_input_file_stream, &
    open_output_file_stream, &
    close_file, &
    action_for_replace
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: rbin
  public :: wbin
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface rbin
    module procedure rb__int1_1d
    module procedure rb__int2_1d
    module procedure rb__int4_1d
    module procedure rb__int8_1d
    module procedure rb__real_1d
    module procedure rb__dble_1d
    module procedure rb__int1_2d
    module procedure rb__int2_2d
    module procedure rb__int4_2d
    module procedure rb__int8_2d
    module procedure rb__real_2d
    module procedure rb__dble_2d
    module procedure rb__int1_3d
    module procedure rb__int2_3d
    module procedure rb__int4_3d
    module procedure rb__int8_3d
    module procedure rb__real_3d
    module procedure rb__dble_3d
    module procedure rb__as1d__int1_2d
    module procedure rb__as1d__int2_2d
    module procedure rb__as1d__int4_2d
    module procedure rb__as1d__int8_2d
    module procedure rb__as1d__real_2d
    module procedure rb__as1d__dble_2d
  end interface

  interface wbin
    module procedure wb__int1_1d
    module procedure wb__int2_1d
    module procedure wb__int4_1d
    module procedure wb__int8_1d
    module procedure wb__real_1d
    module procedure wb__dble_1d
    module procedure wb__int1_2d
    module procedure wb__int2_2d
    module procedure wb__int4_2d
    module procedure wb__int8_2d
    module procedure wb__real_2d
    module procedure wb__dble_2d
    module procedure wb__int1_3d
    module procedure wb__int2_3d
    module procedure wb__int4_3d
    module procedure wb__int8_3d
    module procedure wb__real_3d
    module procedure wb__dble_3d
  end interface

  interface read_block
    module procedure read_block__int1
    module procedure read_block__int2
    module procedure read_block__int4
    module procedure read_block__int8
    module procedure read_block__real
    module procedure read_block__dble
  end interface

  interface write_block
    module procedure write_block__int1
    module procedure write_block__int2
    module procedure write_block__int4
    module procedure write_block__int8
    module procedure write_block__real
    module procedure write_block__dble
  end interface

  interface fill_block
    module procedure fill_block__int1
    module procedure fill_block__int2
    module procedure fill_block__int4
    module procedure fill_block__int8
    module procedure fill_block__real
    module procedure fill_block__dble
  end interface
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: NAME_LIB = 'lib_io_binary_direct'

  integer(8), parameter :: THRESH_DATASIZE = 100000000_8
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine rb__int1_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(1)  , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__int1_1d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, sz_, rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  call read_block(0, dat(:), dtype_, un, 0_8, nn)

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    call read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  call read_block(-1, dat(:), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__int1_1d
!===============================================================
!
!===============================================================
subroutine rb__int2_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(2)  , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__int2_1d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, sz_, rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  call read_block(0, dat(:), dtype_, un, 0_8, nn)

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    call read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  call read_block(-1, dat(:), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__int2_1d
!===============================================================
!
!===============================================================
subroutine rb__int4_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(4)  , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__int4_1d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, sz_, rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  call read_block(0, dat(:), dtype_, un, 0_8, nn)

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    call read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  call read_block(-1, dat(:), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__int4_1d
!===============================================================
!
!===============================================================
subroutine rb__int8_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(8)  , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__int8_1d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, sz_, rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  call read_block(0, dat(:), dtype_, un, 0_8, nn)

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    call read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  call read_block(-1, dat(:), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__int8_1d
!===============================================================
!
!===============================================================
subroutine rb__real_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  real(4)     , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__real_1d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, sz_, rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  call read_block(0, dat(:), dtype_, un, 0_8, nn)

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    call read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  call read_block(-1, dat(:), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__real_1d
!===============================================================
!
!===============================================================
subroutine rb__dble_1d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  real(8)     , intent(out) :: dat(:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__real_1d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(check_recl) ) check_recl_ = check_recl

  ub = lb_-1_8 + shp

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, sz_, rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  pos = sz_*(rec_-1)*byte + 1_8

  call read_block(0, dat(:), dtype_, un, 0_8, nn)

  pos = pos + (lb_-1_8)*byte
  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    call read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo
  pos = pos + (sz_-ub)*byte

  call read_block(-1, dat(:), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__dble_1d
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
subroutine rb__int1_2d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(1)  , intent(out) :: dat(:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__int1_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:,1), dtype_, un, 0_8, nn)

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call read_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  call read_block(-1, dat(:,1), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__int1_2d
!===============================================================
!
!===============================================================
subroutine rb__int2_2d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(2)  , intent(out) :: dat(:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__int2_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:,1), dtype_, un, 0_8, nn)

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call read_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  call read_block(-1, dat(:,1), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__int2_2d
!===============================================================
!
!===============================================================
subroutine rb__int4_2d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(4)  , intent(out) :: dat(:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__int4_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:,1), dtype_, un, 0_8, nn)

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call read_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  call read_block(-1, dat(:,1), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__int4_2d
!===============================================================
!
!===============================================================
subroutine rb__int8_2d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(8)  , intent(out) :: dat(:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__int8_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:,1), dtype_, un, 0_8, nn)

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call read_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  call read_block(-1, dat(:,1), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__int8_2d
!===============================================================
!
!===============================================================
subroutine rb__real_2d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  real(4)     , intent(out) :: dat(:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__real_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:,1), dtype_, un, 0_8, nn)

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call read_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  call read_block(-1, dat(:,1), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__real_2d
!===============================================================
!
!===============================================================
subroutine rb__dble_2d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  real(8)     , intent(out) :: dat(:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__dble_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:,1), dtype_, un, 0_8, nn)

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call read_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  call read_block(-1, dat(:,1), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__dble_2d
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
subroutine rb__int1_3d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(1)  , intent(out) :: dat(:,:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2, i3
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__int1_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:,1,1), dtype_, un, 0_8, nn)

  pos = pos + product(sz_(:2))*(lb(3)-1_8)*byte

  do i3 = 1_8, shp(3)
    pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    do i2 = 1_8, shp(2)
      pos = pos + (lb_(1)-1_8)*byte
      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        call read_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n)
        is = is + n
        pos = pos + n*byte
        n = nn
      enddo
      pos = pos + (sz_(1)-ub(1))*byte
    enddo
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  enddo
  pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte

  call read_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__int1_3d
!===============================================================
!
!===============================================================
subroutine rb__int2_3d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(2)  , intent(out) :: dat(:,:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2, i3
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__int2_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:,1,1), dtype_, un, 0_8, nn)

  pos = pos + product(sz_(:2))*(lb(3)-1_8)*byte

  do i3 = 1_8, shp(3)
    pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    do i2 = 1_8, shp(2)
      pos = pos + (lb_(1)-1_8)*byte
      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        call read_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n)
        is = is + n
        pos = pos + n*byte
        n = nn
      enddo
      pos = pos + (sz_(1)-ub(1))*byte
    enddo
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  enddo
  pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte

  call read_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__int2_3d
!===============================================================
!
!===============================================================
subroutine rb__int4_3d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(4)  , intent(out) :: dat(:,:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2, i3
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__int4_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:,1,1), dtype_, un, 0_8, nn)

  pos = pos + product(sz_(:2))*(lb(3)-1_8)*byte

  do i3 = 1_8, shp(3)
    pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    do i2 = 1_8, shp(2)
      pos = pos + (lb_(1)-1_8)*byte
      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        call read_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n)
        is = is + n
        pos = pos + n*byte
        n = nn
      enddo
      pos = pos + (sz_(1)-ub(1))*byte
    enddo
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  enddo
  pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte

  call read_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__int4_3d
!===============================================================
!
!===============================================================
subroutine rb__int8_3d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(8)  , intent(out) :: dat(:,:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2, i3
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__int8_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:,1,1), dtype_, un, 0_8, nn)

  pos = pos + product(sz_(:2))*(lb(3)-1_8)*byte

  do i3 = 1_8, shp(3)
    pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    do i2 = 1_8, shp(2)
      pos = pos + (lb_(1)-1_8)*byte
      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        call read_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n)
        is = is + n
        pos = pos + n*byte
        n = nn
      enddo
      pos = pos + (sz_(1)-ub(1))*byte
    enddo
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  enddo
  pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte

  call read_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__int8_3d
!===============================================================
!
!===============================================================
subroutine rb__real_3d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  real(4)     , intent(out) :: dat(:,:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2, i3
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__real_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:,1,1), dtype_, un, 0_8, nn)

  pos = pos + product(sz_(:2))*(lb(3)-1_8)*byte

  do i3 = 1_8, shp(3)
    pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    do i2 = 1_8, shp(2)
      pos = pos + (lb_(1)-1_8)*byte
      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        call read_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n)
        is = is + n
        pos = pos + n*byte
        n = nn
      enddo
      pos = pos + (sz_(1)-ub(1))*byte
    enddo
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  enddo
  pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte

  call read_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__real_3d
!===============================================================
!
!===============================================================
subroutine rb__dble_3d(&
    dat, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  real(8)     , intent(out) :: dat(:,:,:)  !--dtype--
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2, i3
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__dble_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = shape(dat,kind=8)

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:,1,1), dtype_, un, 0_8, nn)

  pos = pos + product(sz_(:2))*(lb(3)-1_8)*byte

  do i3 = 1_8, shp(3)
    pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    do i2 = 1_8, shp(2)
      pos = pos + (lb_(1)-1_8)*byte
      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        call read_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n)
        is = is + n
        pos = pos + n*byte
        n = nn
      enddo
      pos = pos + (sz_(1)-ub(1))*byte
    enddo
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  enddo
  pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte

  call read_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__dble_3d
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
subroutine rb__as1d__int1_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(1)  , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__as1d__int1_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:), dtype_, un, 0_8, nn)

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  call read_block(-1, dat(:), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__as1d__int1_2d
!===============================================================
!
!===============================================================
subroutine rb__as1d__int2_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(2)  , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__as1d__int2_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:), dtype_, un, 0_8, nn)

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  call read_block(-1, dat(:), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__as1d__int2_2d
!===============================================================
!
!===============================================================
subroutine rb__as1d__int4_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(4)  , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__as1d__int4_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_INT4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:), dtype_, un, 0_8, nn)

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  call read_block(-1, dat(:), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__as1d__int4_2d
!===============================================================
!
!===============================================================
subroutine rb__as1d__int8_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  integer(8)  , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__as1d__int8_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:), dtype_, un, 0_8, nn)

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  call read_block(-1, dat(:), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__as1d__int8_2d
!===============================================================
!
!===============================================================
subroutine rb__as1d__real_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  real(4)     , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__as1d__real_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:), dtype_, un, 0_8, nn)

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  call read_block(-1, dat(:), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__as1d__real_2d
!===============================================================
!
!===============================================================
subroutine rb__as1d__dble_2d(&
    dat, n1, n2, f, dtype, endian, rec, sz, lb, check_recl, ios)
  implicit none
  real(8)     , intent(out) :: dat(:)  !--dtype--
  integer(8)  , intent(in)  :: n1, n2
  character(*), intent(in)  :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  logical     , intent(in) , optional :: check_recl
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  logical             :: check_recl_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: i2
  integer(8) :: is
  integer(8) :: pos

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'rb__as1d__dble_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(1) = n1
  shp(2) = n2

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  check_recl_ = .false.

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(check_recl) ) check_recl_ = check_recl

  ub(:) = lb_(:) + shp(:) - 1_8

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         f, dtype_, product(sz_), rec_, check_recl_, opt, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, f, endian_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  pos = product(sz_)*(rec_-1)*byte + 1_8

  call read_block(0, dat(:), dtype_, un, 0_8, nn)

  pos = pos + sz_(1)*(lb_(2)-1_8)*byte
  do i2 = 1_8, shp(2)
    pos = pos + (lb_(1)-1_8)*byte
    is = shp(1)*(i2-1_8)
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call read_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo
    pos = pos + (sz_(1)-ub(1))*byte
  enddo
  pos = pos + sz_(1)*(sz_(2)-ub(2))*byte

  call read_block(-1, dat(:), dtype_, un, 0_8, 0_8)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine rb__as1d__dble_2d
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
subroutine wb__int1_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  integer(1)  , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  integer(1)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  integer(1)          :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__int1_1d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = 0_1  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, sz_, replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    call write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:), dtype_, un, pos, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__int1_1d
!===============================================================
!
!===============================================================
subroutine wb__int2_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  integer(2)  , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  integer(2)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  integer(2)          :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__int2_1d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = 0_2  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, sz_, replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    call write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:), dtype_, un, pos, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__int2_1d
!===============================================================
!
!===============================================================
subroutine wb__int4_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  integer(4)  , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  integer(4)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  integer(4)          :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__int4_1d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = 0_4  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, sz_, replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    call write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:), dtype_, un, pos, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__int4_1d
!===============================================================
!
!===============================================================
subroutine wb__int8_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  integer(8)  , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  integer(8)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  integer(8)          :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__int8_1d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = 0_8  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, sz_, replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    call write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:), dtype_, un, pos, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__int8_1d
!===============================================================
!
!===============================================================
subroutine wb__real_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  real(4)     , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  real(4)     , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  real(4)             :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__real_1d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = 0.0  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, sz_, replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    call write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:), dtype_, un, pos, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__real_1d
!===============================================================
!
!===============================================================
subroutine wb__dble_1d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  real(8)     , intent(in) :: dat(:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz
  integer(8)  , intent(in) , optional :: lb
  real(8)     , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_
  integer(8)          :: lb_
  real(8)             :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp
  integer(8) :: ub
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__dble_1d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp = size(dat,kind=8)

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_ = shp
  lb_ = 1_8
  replace_ = .false.
  fill_ = 0.d0  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_ = sz
  if( present(lb) ) lb_ = lb
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub = lb_ - 1_8 + shp

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, sz_, replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp-1_8) / thresh_ndata + 1_8
  nn = (shp-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(1, fs, present(fill), (/sz_/), (/lb_/), (/ub/))
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = sz_*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_ > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(lb_-1_8-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (lb_-1_8-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (lb_-1_8) * byte
  endif

  is = 0_8
  n = shp - nn*(nblock-1_8)
  do kk = 1_8, nblock
    call write_block(1, dat(is+1_8:is+n), dtype_, un, pos, n)
    is = is + n
    pos = pos + n*byte
    n = nn
  enddo

  ! Fill the tail of record
  if( ub < sz_ .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_-ub-1_8, thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_-ub-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + (sz_-ub)*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:), dtype_, un, pos, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__dble_1d
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
subroutine wb__int1_2d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  integer(1)  , intent(in) :: dat(:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(1)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  integer(1)          :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__int1_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_1  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, product(sz_), replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:,1), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)           
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(lb_(2)-1_8) * byte
  endif

  do i2 = 1_8, shp(2)
    if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
      do ifillblock = 1, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (lb_(1)-1_8)*byte
    endif

    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call write_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo

    if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (sz_(1)-ub(1))*byte
    endif
  enddo

  ! Fill the tail of record
  if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:,1), dtype_, un, 0_8, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__int1_2d
!===============================================================
!
!===============================================================
subroutine wb__int2_2d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  integer(2)  , intent(in) :: dat(:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(2)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  integer(2)          :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__int2_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_2  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, product(sz_), replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:,1), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)           
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(lb_(2)-1_8) * byte
  endif

  do i2 = 1_8, shp(2)
    if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
      do ifillblock = 1, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (lb_(1)-1_8)*byte
    endif

    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call write_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo

    if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (sz_(1)-ub(1))*byte
    endif
  enddo

  ! Fill the tail of record
  if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:,1), dtype_, un, 0_8, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__int2_2d
!===============================================================
!
!===============================================================
subroutine wb__int4_2d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  integer(4)  , intent(in) :: dat(:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(4)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  integer(4)          :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__int4_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_4  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, product(sz_), replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:,1), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)           
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(lb_(2)-1_8) * byte
  endif

  do i2 = 1_8, shp(2)
    if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
      do ifillblock = 1, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (lb_(1)-1_8)*byte
    endif

    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call write_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo

    if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (sz_(1)-ub(1))*byte
    endif
  enddo

  ! Fill the tail of record
  if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:,1), dtype_, un, 0_8, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__int4_2d
!===============================================================
!
!===============================================================
subroutine wb__int8_2d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  integer(8)  , intent(in) :: dat(:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(8)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  integer(8)          :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__int8_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_8  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, product(sz_), replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:,1), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)           
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(lb_(2)-1_8) * byte
  endif

  do i2 = 1_8, shp(2)
    if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
      do ifillblock = 1, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (lb_(1)-1_8)*byte
    endif

    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call write_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo

    if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (sz_(1)-ub(1))*byte
    endif
  enddo

  ! Fill the tail of record
  if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:,1), dtype_, un, 0_8, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__int8_2d
!===============================================================
!
!===============================================================
subroutine wb__real_2d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  real(4)     , intent(in) :: dat(:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  real(4)     , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  real(4)             :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__real_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0.0  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, product(sz_), replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:,1), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)           
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(lb_(2)-1_8) * byte
  endif

  do i2 = 1_8, shp(2)
    if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
      do ifillblock = 1, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (lb_(1)-1_8)*byte
    endif

    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call write_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo

    if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (sz_(1)-ub(1))*byte
    endif
  enddo

  ! Fill the tail of record
  if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:,1), dtype_, un, 0_8, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__real_2d
!===============================================================
!
!===============================================================
subroutine wb__dble_2d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  real(8)     , intent(in) :: dat(:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  real(8)     , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(2)
  integer(8)          :: lb_(2)
  real(8)             :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp(2)
  integer(8) :: ub(2)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__dble_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0.d0  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, product(sz_), replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:,1), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)           
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(lb_(2)-1_8) * byte
  endif

  do i2 = 1_8, shp(2)
    if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
      do ifillblock = 1, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (lb_(1)-1_8)*byte
    endif

    is = 0_8
    n = shp(1) - nn*(nblock-1_8)
    do kk = 1_8, nblock
      call write_block(1, dat(is+1_8:is+n,i2), dtype_, un, pos, n)
      is = is + n
      pos = pos + n*byte
      n = nn
    enddo

    if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + (sz_(1)-ub(1))*byte
    endif
  enddo

  ! Fill the tail of record
  if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:,1), dtype_, un, 0_8, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__dble_2d
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
subroutine wb__int1_3d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  integer(1)  , intent(in) :: dat(:,:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(1)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  integer(1)          :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2, i3

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__int1_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT1  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_1  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, product(sz_), replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:,1,1), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)           
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(3) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(lb_(3)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (product(sz_(:2))*(lb_(3)-1_8)-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(lb_(3)-1_8) * byte
  endif

  do i3 = 1_8, shp(3)
    if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    endif

    do i2 = 1_8, shp(2)
      if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
          call fill_block(1, fill_, dtype_, un, pos, nfill)
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (lb_(1)-1_8)*byte
      endif

      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        call write_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n)

        is = is + n
        pos = pos + n*byte
        n = nn
      enddo

      if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
          call fill_block(1, fill_, dtype_, un, pos, nfill)
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (sz_(1)-ub(1))*byte
      endif
    enddo  ! i2/

    if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
    endif
  enddo  ! i3/

  ! Fill the tail of record
  if( ub(3) < sz_(3) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(sz_(3)-ub(3))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (product(sz_(:2))*(sz_(3)-ub(3))-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__int1_3d
!===============================================================
!
!===============================================================
subroutine wb__int2_3d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  integer(2)  , intent(in) :: dat(:,:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(2)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  integer(2)          :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2, i3

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__int2_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT2  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_2  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, product(sz_), replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:,1,1), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)           
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(3) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(lb_(3)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (product(sz_(:2))*(lb_(3)-1_8)-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(lb_(3)-1_8) * byte
  endif

  do i3 = 1_8, shp(3)
    if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    endif

    do i2 = 1_8, shp(2)
      if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
          call fill_block(1, fill_, dtype_, un, pos, nfill)
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (lb_(1)-1_8)*byte
      endif

      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        call write_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n)

        is = is + n
        pos = pos + n*byte
        n = nn
      enddo

      if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
          call fill_block(1, fill_, dtype_, un, pos, nfill)
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (sz_(1)-ub(1))*byte
      endif
    enddo  ! i2/

    if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
    endif
  enddo  ! i3/

  ! Fill the tail of record
  if( ub(3) < sz_(3) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(sz_(3)-ub(3))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (product(sz_(:2))*(sz_(3)-ub(3))-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__int2_3d
!===============================================================
!
!===============================================================
subroutine wb__int4_3d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  integer(4)  , intent(in) :: dat(:,:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(4)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  integer(4)          :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2, i3

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__int4_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT4  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_4  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, product(sz_), replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:,1,1), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)           
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(3) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(lb_(3)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (product(sz_(:2))*(lb_(3)-1_8)-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(lb_(3)-1_8) * byte
  endif

  do i3 = 1_8, shp(3)
    if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    endif

    do i2 = 1_8, shp(2)
      if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
          call fill_block(1, fill_, dtype_, un, pos, nfill)
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (lb_(1)-1_8)*byte
      endif

      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        call write_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n)

        is = is + n
        pos = pos + n*byte
        n = nn
      enddo

      if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
          call fill_block(1, fill_, dtype_, un, pos, nfill)
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (sz_(1)-ub(1))*byte
      endif
    enddo  ! i2/

    if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
    endif
  enddo  ! i3/

  ! Fill the tail of record
  if( ub(3) < sz_(3) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(sz_(3)-ub(3))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (product(sz_(:2))*(sz_(3)-ub(3))-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__int4_3d
!===============================================================
!
!===============================================================
subroutine wb__int8_3d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  integer(8)  , intent(in) :: dat(:,:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  integer(8)  , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  integer(8)          :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2, i3

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__int8_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_INT8  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0_8  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, product(sz_), replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:,1,1), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)           
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(3) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(lb_(3)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (product(sz_(:2))*(lb_(3)-1_8)-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(lb_(3)-1_8) * byte
  endif

  do i3 = 1_8, shp(3)
    if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    endif

    do i2 = 1_8, shp(2)
      if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
          call fill_block(1, fill_, dtype_, un, pos, nfill)
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (lb_(1)-1_8)*byte
      endif

      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        call write_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n)

        is = is + n
        pos = pos + n*byte
        n = nn
      enddo

      if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
          call fill_block(1, fill_, dtype_, un, pos, nfill)
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (sz_(1)-ub(1))*byte
      endif
    enddo  ! i2/

    if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
    endif
  enddo  ! i3/

  ! Fill the tail of record
  if( ub(3) < sz_(3) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(sz_(3)-ub(3))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (product(sz_(:2))*(sz_(3)-ub(3))-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__int8_3d
!===============================================================
!
!===============================================================
subroutine wb__real_3d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  real(4)     , intent(in) :: dat(:,:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  real(4)     , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  real(4)             :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2, i3

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__real_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_REAL  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0.0  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, product(sz_), replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:,1,1), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)           
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(3) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(lb_(3)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (product(sz_(:2))*(lb_(3)-1_8)-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(lb_(3)-1_8) * byte
  endif

  do i3 = 1_8, shp(3)
    if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    endif

    do i2 = 1_8, shp(2)
      if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
          call fill_block(1, fill_, dtype_, un, pos, nfill)
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (lb_(1)-1_8)*byte
      endif

      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        call write_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n)

        is = is + n
        pos = pos + n*byte
        n = nn
      enddo

      if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
          call fill_block(1, fill_, dtype_, un, pos, nfill)
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (sz_(1)-ub(1))*byte
      endif
    enddo  ! i2/

    if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
    endif
  enddo  ! i3/

  ! Fill the tail of record
  if( ub(3) < sz_(3) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(sz_(3)-ub(3))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (product(sz_(:2))*(sz_(3)-ub(3))-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__real_3d
!===============================================================
!
!===============================================================
subroutine wb__dble_3d(&
    dat, f, dtype, endian, rec, sz, lb, fill, replace, ios)
  implicit none
  real(8)     , intent(in) :: dat(:,:,:)  !--dtype--
  character(*), intent(in) :: f
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer     , intent(in) , optional :: rec
  integer(8)  , intent(in) , optional :: sz(:)
  integer(8)  , intent(in) , optional :: lb(:)
  real(8)     , intent(in) , optional :: fill  !--dtype--
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: ios

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer             :: rec_
  integer(8)          :: sz_(3)
  integer(8)          :: lb_(3)
  real(8)             :: fill_  !--dtype--
  logical             :: replace_
  integer             :: ios_

  integer(8) :: shp(3)
  integer(8) :: ub(3)
  integer(8) :: byte
  integer(8) :: thresh_ndata
  integer(8) :: nblock, kk
  integer(8) :: nn, n
  integer(8) :: is
  integer(8) :: pos
  integer(8) :: fs
  integer(8) :: nfill_max
  integer(8) :: nfill
  integer(8) :: ifillblock
  integer(8) :: i2, i3

  integer :: un
  character(16) :: opt

  call echo(code%bgn, 'wb__dble_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  shp(:) = shape(dat,kind=8)

  dtype_ = DTYPE_DBLE  !--dtype--
  endian_ = ENDIAN_DEFAULT
  rec_ = 1
  sz_(:) = shp(:)
  lb_(:) = 1_8
  replace_ = .false.
  fill_ = 0.d0  !--dtype--

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(rec) ) rec_ = rec
  if( present(sz) ) sz_(:) = sz(:)
  if( present(lb) ) lb_(:) = lb(:)
  if( present(replace) ) replace_ = replace
  if( present(fill) ) fill_ = fill

  ub(:) = lb_(:) - 1_8 + shp(:)

  opt = ''
  if( present(ios) ) opt = '-q -a'

  ios_ = 0
  if( present(ios) ) ios = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(&
         f, dtype_, product(sz_), replace_, opt, fs, ios_)
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, f, endian_, replace_, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  byte = byte_of_dtype(dtype_)
  thresh_ndata = THRESH_DATASIZE / byte
  nblock = (shp(1)-1_8) / thresh_ndata + 1_8
  nn = (shp(1)-1_8) / nblock + 1_8

  ! Allocate
  call write_block(0, dat(:,1,1), dtype_, un, 0_8, nn)

  ! Allocate
  nfill_max = get_nfill_max(size(shp), fs, present(fill), sz_, lb_, ub)           
  if( nfill_max > 0_8 )then
    call fill_block(0, fill_, dtype_, un, 0_8, min(nfill_max,thresh_ndata))
  endif

  pos = product(sz_)*(rec_-1) * byte + 1_8

  ! Fill the head of record
  if( lb_(3) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(lb_(3)-1_8)-1_8,thresh_ndata)+1_8
    do ifillblock = 1, (product(sz_(:2))*(lb_(3)-1_8)-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(lb_(3)-1_8) * byte
  endif

  do i3 = 1_8, shp(3)
    if( lb_(2) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(lb_(2)-1_8)-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(lb_(2)-1_8)-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(lb_(2)-1_8)*byte
    endif

    do i2 = 1_8, shp(2)
      if( lb_(1) > 1_8 .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(lb_(1)-1_8-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (lb_(1)-1_8-1_8)/thresh_ndata+1_8
          call fill_block(1, fill_, dtype_, un, pos, nfill)
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (lb_(1)-1_8)*byte
      endif

      is = 0_8
      n = shp(1) - nn*(nblock-1)
      do kk = 1_8, nblock
        call write_block(1, dat(is+1:is+n,i2,i3), dtype_, un, pos, n)

        is = is + n
        pos = pos + n*byte
        n = nn
      enddo

      if( ub(1) < sz_(1) .and. (fs == 0_8 .or. present(fill)) )then
        nfill = mod(sz_(1)-ub(1)-1_8,thresh_ndata)+1_8
        do ifillblock = 1_8, (sz_(1)-ub(1)-1_8)/thresh_ndata+1_8
          call fill_block(1, fill_, dtype_, un, pos, nfill)
          pos = pos + nfill*byte
          nfill = thresh_ndata
        enddo
      else
        pos = pos + (sz_(1)-ub(1))*byte
      endif
    enddo  ! i2/

    if( ub(2) < sz_(2) .and. (fs == 0_8 .or. present(fill)) )then
      nfill = mod(sz_(1)*(sz_(2)-ub(2))-1_8,thresh_ndata)+1_8
      do ifillblock = 1_8, (sz_(1)*(sz_(2)-ub(2))-1_8)/thresh_ndata+1_8
        call fill_block(1, fill_, dtype_, un, pos, nfill)
        pos = pos + nfill*byte
        nfill = thresh_ndata
      enddo
    else
      pos = pos + sz_(1)*(sz_(2)-ub(2))*byte
    endif
  enddo  ! i3/

  ! Fill the tail of record
  if( ub(3) < sz_(3) .and. (fs == 0_8 .or. present(fill)) )then
    nfill = mod(product(sz_(:2))*(sz_(3)-ub(3))-1_8,thresh_ndata)+1_8
    do ifillblock = 1_8, (product(sz_(:2))*(sz_(3)-ub(3))-1_8)/thresh_ndata+1_8
      call fill_block(1, fill_, dtype_, un, pos, nfill)
      pos = pos + nfill*byte
      nfill = thresh_ndata
    enddo
  else
    pos = pos + product(sz_(:2))*(sz_(3)-ub(3))*byte
  endif

  ! Deallocate
  call write_block(-1, dat(:,1,1), dtype_, un, 0_8, 0_8)

  ! Deallocate
  if( nfill_max > 0_8 )then
    call fill_block(-1, fill_, dtype_, un, 0_8, 0_8)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call close_file(un, ios_, present(ios))
  if( ios_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call finalize()
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
subroutine finalize()
  implicit none

  if( ios_ /= 0 )then
    if( .not. present(ios) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  ios_ /= 0 .and. .not. present(ios)')
    endif
    ios = ios_
  endif
end subroutine finalize
end subroutine wb__dble_3d
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
subroutine read_block__int1(&
    job, dat, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  integer(1)  , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)
  integer(8) :: i

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE read_block__int1 ***\n'

  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Initialize
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); continue
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Read data
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      read(un,pos=pos) dat(:n)
    case( DTYPE_INT2 )
      read(un,pos=pos) tmpi2(:n)
      do i = 1_8, n
        dat(i) = int(tmpi2(i),1)
      enddo
    case( DTYPE_INT4 )
      read(un,pos=pos) tmpi4(:n)
      do i = 1_8, n
        dat(i) = int(tmpi4(i),1)
      enddo
    case( DTYPE_INT8 )
      read(un,pos=pos) tmpi8(:n)
      do i = 1_8, n
        dat(i) = int(tmpi8(i),1)
      enddo
    case( DTYPE_REAL )
      read(un,pos=pos) tmpr4(:n)
      do i = 1_8, n
        dat(i) = int(tmpr4(i),1)
      enddo
    case( DTYPE_DBLE )
      read(un,pos=pos) tmpr8(:n)
      do i = 1_8, n
        dat(i) = int(tmpr8(i),1)
      enddo
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Finalize
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); continue
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine read_block__int1
!===============================================================
!
!===============================================================
subroutine read_block__int2(&
    job, dat, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  integer(2)  , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)
  integer(8) :: i

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE read_block__int2 ***\n'

  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Initialize
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); continue
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Read data
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      read(un,pos=pos) tmpi1(:n)
      do i = 1_8, n
        dat(i) = int(tmpi1(i),2)
      enddo
    case( DTYPE_INT2 )
      read(un,pos=pos) dat(:n)
    case( DTYPE_INT4 )
      read(un,pos=pos) tmpi4(:n)
      do i = 1_8, n
        dat(i) = int(tmpi4(i),2)
      enddo
    case( DTYPE_INT8 )
      read(un,pos=pos) tmpi8(:n)
      do i = 1_8, n
        dat(i) = int(tmpi8(i),2)
      enddo
    case( DTYPE_REAL )
      read(un,pos=pos) tmpr4(:n)
      do i = 1_8, n
        dat(i) = int(tmpr4(i),2)
      enddo
    case( DTYPE_DBLE )
      read(un,pos=pos) tmpr8(:n)
      do i = 1_8, n
        dat(i) = int(tmpr8(i),2)
      enddo
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Finalize
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); continue
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine read_block__int2
!===============================================================
!
!===============================================================
subroutine read_block__int4(&
    job, dat, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  integer(4)  , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)
  integer(8) :: i

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE read_block__int4 ***\n'

  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Initialize
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); continue
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Read data
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      read(un,pos=pos) tmpi1(:n)
      do i = 1_8, n
        dat(i) = int(tmpi1(i),4)
      enddo
    case( DTYPE_INT2 )
      read(un,pos=pos) tmpi2(:n)
      do i = 1_8, n
        dat(i) = int(tmpi2(i),4)
      enddo
    case( DTYPE_INT4 )
      read(un,pos=pos) dat(:n)
    case( DTYPE_INT8 )
      read(un,pos=pos) tmpi8(:n)
      do i = 1_8, n
        dat(i) = int(tmpi8(i),4)
      enddo
    case( DTYPE_REAL )
      read(un,pos=pos) tmpr4(:n)
      do i = 1_8, n
        dat(i) = int(tmpr4(i),4)
      enddo
    case( DTYPE_DBLE )
      read(un,pos=pos) tmpr8(:n)
      do i = 1_8, n
        dat(i) = int(tmpr8(i),4)
      enddo
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Finalize
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); continue
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine read_block__int4
!===============================================================
!
!===============================================================
subroutine read_block__int8(&
    job, dat, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  integer(8)  , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)
  integer(8) :: i

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE read_block__int8 ***\n'

  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Initialize
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); continue
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Read data
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      read(un,pos=pos) tmpi1(:n)
      do i = 1_8, n
        dat(i) = int(tmpi1(i),8)
      enddo
    case( DTYPE_INT2 )
      read(un,pos=pos) tmpi2(:n)
      do i = 1_8, n
        dat(i) = int(tmpi2(i),8)
      enddo
    case( DTYPE_INT4 )
      read(un,pos=pos) tmpi4(:n)
      do i = 1_8, n
        dat(i) = int(tmpi4(i),8)
      enddo
    case( DTYPE_INT8 )
      read(un,pos=pos) dat(:n)
    case( DTYPE_REAL )
      read(un,pos=pos) tmpr4(:n)
      do i = 1_8, n
        dat(i) = int(tmpr4(i),8)
      enddo
    case( DTYPE_DBLE )
      read(un,pos=pos) tmpr8(:n)
      do i = 1_8, n
        dat(i) = int(tmpr8(i),8)
      enddo
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Finalize
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); continue
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine read_block__int8
!===============================================================
!
!===============================================================
subroutine read_block__real(&
    job, dat, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  real(4)     , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(8)   , allocatable, save :: tmpr8(:)
  integer(8) :: i

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE read_block__real ***\n'

  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Initialize
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); continue
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Read data
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      read(un,pos=pos) tmpi1(:n)
      do i = 1_8, n
        dat(i) = real(tmpi1(i),4)
      enddo
    case( DTYPE_INT2 )
      read(un,pos=pos) tmpi2(:n)
      do i = 1_8, n
        dat(i) = real(tmpi2(i),4)
      enddo
    case( DTYPE_INT4 )
      read(un,pos=pos) tmpi4(:n)
      do i = 1_8, n
        dat(i) = real(tmpi4(i),4)
      enddo
    case( DTYPE_INT8 )
      read(un,pos=pos) tmpi8(:n)
      do i = 1_8, n
        dat(i) = real(tmpi8(i),4)
      enddo
    case( DTYPE_REAL )
      read(un,pos=pos) dat(:n)
    case( DTYPE_DBLE )
      read(un,pos=pos) tmpr8(:n)
      do i = 1_8, n
        dat(i) = real(tmpr8(i),4)
      enddo
    endselect
  !-------------------------------------------------------------
  ! Case: Finalize
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); continue
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine read_block__real
!===============================================================
!
!===============================================================
subroutine read_block__dble(&
    job, dat, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  real(8)     , intent(inout) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  integer(8) :: i

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE read_block__dble ***\n'

  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Initialize
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); continue
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Read data
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      read(un,pos=pos) tmpi1(:n)
      do i = 1_8, n
        dat(i) = real(tmpi1(i),8)
      enddo
    case( DTYPE_INT2 )
      read(un,pos=pos) tmpi2(:n)
      do i = 1_8, n
        dat(i) = real(tmpi2(i),8)
      enddo
    case( DTYPE_INT4 )
      read(un,pos=pos) tmpi4(:n)
      do i = 1_8, n
        dat(i) = real(tmpi4(i),8)
      enddo
    case( DTYPE_INT8 )
      read(un,pos=pos) tmpi8(:n)
      do i = 1_8, n
        dat(i) = real(tmpi8(i),8)
      enddo
    case( DTYPE_REAL )
      read(un,pos=pos) tmpr4(:n)
      do i = 1_8, n
        dat(i) = real(tmpr4(i),8)
      enddo
    case( DTYPE_DBLE )
      read(un,pos=pos) dat(:n)
    endselect
  !-------------------------------------------------------------
  ! Case: Finalize
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); continue
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine read_block__dble
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
subroutine write_block__int1(&
    job, dat, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  integer(1)  , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)
  integer(8) :: i

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE write_block__int1 ***\n'

  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Initialize
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(1))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Read data
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      write(un,pos=pos) dat(:n)
    case( DTYPE_INT2 )
      do i = 1_8, n
        tmpi2(i) = int(dat(i),2)
      enddo
      write(un,pos=pos) tmpi2(:n)
    case( DTYPE_INT4 )
      do i = 1_8, n
        tmpi4(i) = int(dat(i),4)
      enddo
      write(un,pos=pos) tmpi4(:n)
    case( DTYPE_INT8 )
      do i = 1_8, n
        tmpi8(i) = int(dat(i),8)
      enddo
      write(un,pos=pos) tmpi8(:n)
    case( DTYPE_REAL )
      do i = 1_8, n
        tmpr4(i) = real(dat(i),4)
      enddo
      write(un,pos=pos) tmpr4(:n)
    case( DTYPE_DBLE )
      do i = 1_8, n
        tmpr8(i) = real(dat(i),8)
      enddo
      write(un,pos=pos) tmpr8(:n)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Finalize
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine write_block__int1
!===============================================================
!
!===============================================================
subroutine write_block__int2(&
    job, dat, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  integer(2)  , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)
  integer(8) :: i

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE write_block__int2 ***\n'

  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Initialize
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(1))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Read data
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      do i = 1_8, n
        tmpi1(i) = int(dat(i),1)
      enddo
      write(un,pos=pos) tmpi1(:n)
    case( DTYPE_INT2 )
      write(un,pos=pos) dat(:n)
    case( DTYPE_INT4 )
      do i = 1_8, n
        tmpi4(i) = int(dat(i),4)
      enddo
      write(un,pos=pos) tmpi4(:n)
    case( DTYPE_INT8 )
      do i = 1_8, n
        tmpi8(i) = int(dat(i),8)
      enddo
      write(un,pos=pos) tmpi8(:n)
    case( DTYPE_REAL )
      do i = 1_8, n
        tmpr4(i) = real(dat(i),4)
      enddo
      write(un,pos=pos) tmpr4(:n)
    case( DTYPE_DBLE )
      do i = 1_8, n
        tmpr8(i) = real(dat(i),8)
      enddo
      write(un,pos=pos) tmpr8(:n)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Finalize
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine write_block__int2
!===============================================================
!
!===============================================================
subroutine write_block__int4(&
    job, dat, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  integer(4)  , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)
  integer(8) :: i

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE write_block__int4 ***\n'

  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Initialize
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(1))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Read data
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      do i = 1_8, n
        tmpi1(i) = int(dat(i),1)
      enddo
      write(un,pos=pos) tmpi1(:n)
    case( DTYPE_INT2 )
      do i = 1_8, n
        tmpi2(i) = int(dat(i),2)
      enddo
      write(un,pos=pos) tmpi2(:n)
    case( DTYPE_INT4 )
      write(un,pos=pos) dat(:n)
    case( DTYPE_INT8 )
      do i = 1_8, n
        tmpi8(i) = int(dat(i),8)
      enddo
      write(un,pos=pos) tmpi8(:n)
    case( DTYPE_REAL )
      do i = 1_8, n
        tmpr4(i) = real(dat(i),4)
      enddo
      write(un,pos=pos) tmpr4(:n)
    case( DTYPE_DBLE )
      do i = 1_8, n
        tmpr8(i) = real(dat(i),8)
      enddo
      write(un,pos=pos) tmpr8(:n)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Finalize
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine write_block__int4
!===============================================================
!
!===============================================================
subroutine write_block__int8(&
    job, dat, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  integer(8)  , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)
  integer(8) :: i

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE write_block__int8 ***\n'

  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Initialize
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(1))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Read data
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      do i = 1_8, n
        tmpi1(i) = int(dat(i),1)
      enddo
      write(un,pos=pos) tmpi1(:n)
    case( DTYPE_INT2 )
      do i = 1_8, n
        tmpi2(i) = int(dat(i),2)
      enddo
      write(un,pos=pos) tmpi2(:n)
    case( DTYPE_INT4 )
      do i = 1_8, n
        tmpi4(i) = int(dat(i),4)
      enddo
      write(un,pos=pos) tmpi4(:n)
    case( DTYPE_INT8 )
      write(un,pos=pos) dat(:n)
    case( DTYPE_REAL )
      do i = 1_8, n
        tmpr4(i) = real(dat(i),4)
      enddo
      write(un,pos=pos) tmpr4(:n)
    case( DTYPE_DBLE )
      do i = 1_8, n
        tmpr8(i) = real(dat(i),8)
      enddo
      write(un,pos=pos) tmpr8(:n)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Finalize
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine write_block__int8
!===============================================================
!
!===============================================================
subroutine write_block__real(&
    job, dat, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  real(4)     , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)
  integer(8) :: i

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE write_block__real ***\n'

  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Initialize
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(1))
    case( DTYPE_DBLE ); allocate(tmpr8(n))
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Read data
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      do i = 1_8, n
        tmpi1(i) = int(dat(i),1)
      enddo
      write(un,pos=pos) tmpi1(:n)
    case( DTYPE_INT2 )
      do i = 1_8, n
        tmpi2(i) = int(dat(i),2)
      enddo
      write(un,pos=pos) tmpi2(:n)
    case( DTYPE_INT4 )
      do i = 1_8, n
        tmpi4(i) = int(dat(i),4)
      enddo
      write(un,pos=pos) tmpi4(:n)
    case( DTYPE_INT8 )
      do i = 1_8, n
        tmpi8(i) = int(dat(i),8)
      enddo
      write(un,pos=pos) tmpi8(:n)
    case( DTYPE_REAL )
      write(un,pos=pos) dat(:n)
    case( DTYPE_DBLE )
      do i = 1_8, n
        tmpr8(i) = real(dat(i),8)
      enddo
      write(un,pos=pos) tmpr8(:n)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Finalize
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine write_block__real
!===============================================================
!
!===============================================================
subroutine write_block__dble(&
    job, dat, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  real(8)     , intent(in) :: dat(:)
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: tmpi1(:)
  integer(2), allocatable, save :: tmpi2(:)
  integer(4), allocatable, save :: tmpi4(:)
  integer(8), allocatable, save :: tmpi8(:)
  real(4)   , allocatable, save :: tmpr4(:)
  real(8)   , allocatable, save :: tmpr8(:)
  integer(8) :: i

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE write_block__dble ***\n'

  selectcase( job )
  !-------------------------------------------------------------
  ! Case: Initialize
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(tmpi1(n))
    case( DTYPE_INT2 ); allocate(tmpi2(n))
    case( DTYPE_INT4 ); allocate(tmpi4(n))
    case( DTYPE_INT8 ); allocate(tmpi8(n))
    case( DTYPE_REAL ); allocate(tmpr4(n))
    case( DTYPE_DBLE ); allocate(tmpr8(1))
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Read data
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 )
      do i = 1_8, n
        tmpi1(i) = int(dat(i),1)
      enddo
      write(un,pos=pos) tmpi1(:n)
    case( DTYPE_INT2 )
      do i = 1_8, n
        tmpi2(i) = int(dat(i),2)
      enddo
      write(un,pos=pos) tmpi2(:n)
    case( DTYPE_INT4 )
      do i = 1_8, n
        tmpi4(i) = int(dat(i),4)
      enddo
      write(un,pos=pos) tmpi4(:n)
    case( DTYPE_INT8 )
      do i = 1_8, n
        tmpi8(i) = int(dat(i),8)
      enddo
      write(un,pos=pos) tmpi8(:n)
    case( DTYPE_REAL )
      do i = 1_8, n
        tmpr4(i) = real(dat(i),4)
      enddo
      write(un,pos=pos) tmpr4(:n)
    case( DTYPE_DBLE )
      write(un,pos=pos) dat(:n)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: Finalize
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(tmpi1)
    case( DTYPE_INT2 ); deallocate(tmpi2)
    case( DTYPE_INT4 ); deallocate(tmpi4)
    case( DTYPE_INT8 ); deallocate(tmpi8)
    case( DTYPE_REAL ); deallocate(tmpr4)
    case( DTYPE_DBLE ); deallocate(tmpr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine write_block__dble
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
integer(8) function get_nfill_max(ndim, fs, present_fill, sz, lb, ub) result(nfill_max)
  implicit none
  integer   , intent(in) :: ndim
  integer(8), intent(in) :: fs
  logical   , intent(in) :: present_fill
  integer(8), intent(in) :: sz(:), lb(:), ub(:)

  nfill_max = 0_8

  if( lb(1) > 1_8 .and. (fs == 0_8 .or. present_fill) )then
    nfill_max = max(nfill_max, lb(1)-1_8)
  endif
  if( ub(1) < sz(1) .and. (fs == 0_8 .or. present_fill) )then
    nfill_max = max(nfill_max, sz(1)-ub(1))
  endif

  if( ndim >= 2 )then
    if( lb(2) > 1_8 .and. (fs == 0_8 .or. present_fill) )then
      nfill_max = max(nfill_max, sz(1)*(lb(2)-1_8))
    endif
    if( ub(2) < sz(2) .and. (fs == 0_8 .or. present_fill) )then
      nfill_max = max(nfill_max, sz(1)*(sz(2)-ub(2)))
    endif
  endif

  if( ndim >= 3 )then
    if( lb(3) > 1_8 .and. (fs == 0_8 .or. present_fill) )then
      nfill_max = max(nfill_max, product(sz(:2))*(lb(3)-1_8))
    endif
    if( ub(3) < sz(3) .and. (fs == 0_8 .or. present_fill) )then
      nfill_max = max(nfill_max, product(sz(:2))*(sz(3)-ub(3)))
    endif
  endif
end function get_nfill_max
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
subroutine fill_block__int1(job, fill, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  integer(1)  , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: filli1(:)
  integer(2), allocatable, save :: filli2(:)
  integer(4), allocatable, save :: filli4(:)
  integer(8), allocatable, save :: filli8(:)
  real(4)   , allocatable, save :: fillr4(:)
  real(8)   , allocatable, save :: fillr8(:)

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE fill_block__int1 ***\n'

  selectcase( job )
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(filli1(n)); filli1(:) = int(fill,1)
    case( DTYPE_INT2 ); allocate(filli2(n)); filli2(:) = int(fill,2)
    case( DTYPE_INT4 ); allocate(filli4(n)); filli4(:) = int(fill,4)
    case( DTYPE_INT8 ); allocate(filli8(n)); filli8(:) = int(fill,8)
    case( DTYPE_REAL ); allocate(fillr4(n)); fillr4(:) = real(fill,4)
    case( DTYPE_DBLE ); allocate(fillr8(n)); fillr8(:) = real(fill,8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); write(un,pos=pos) filli1(:n)
    case( DTYPE_INT2 ); write(un,pos=pos) filli2(:n)
    case( DTYPE_INT4 ); write(un,pos=pos) filli4(:n)
    case( DTYPE_INT8 ); write(un,pos=pos) filli8(:n)
    case( DTYPE_REAL ); write(un,pos=pos) fillr4(:n)
    case( DTYPE_DBLE ); write(un,pos=pos) fillr8(:n)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(filli1)
    case( DTYPE_INT2 ); deallocate(filli2)
    case( DTYPE_INT4 ); deallocate(filli4)
    case( DTYPE_INT8 ); deallocate(filli8)
    case( DTYPE_REAL ); deallocate(fillr4)
    case( DTYPE_DBLE ); deallocate(fillr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine fill_block__int1
!===============================================================
!
!===============================================================
subroutine fill_block__int2(job, fill, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  integer(2)  , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: filli1(:)
  integer(2), allocatable, save :: filli2(:)
  integer(4), allocatable, save :: filli4(:)
  integer(8), allocatable, save :: filli8(:)
  real(4)   , allocatable, save :: fillr4(:)
  real(8)   , allocatable, save :: fillr8(:)

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE fill_block__int2 ***\n'

  selectcase( job )
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(filli1(n)); filli1(:) = int(fill,1)
    case( DTYPE_INT2 ); allocate(filli2(n)); filli2(:) = int(fill,2)
    case( DTYPE_INT4 ); allocate(filli4(n)); filli4(:) = int(fill,4)
    case( DTYPE_INT8 ); allocate(filli8(n)); filli8(:) = int(fill,8)
    case( DTYPE_REAL ); allocate(fillr4(n)); fillr4(:) = real(fill,4)
    case( DTYPE_DBLE ); allocate(fillr8(n)); fillr8(:) = real(fill,8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); write(un,pos=pos) filli1(:n)
    case( DTYPE_INT2 ); write(un,pos=pos) filli2(:n)
    case( DTYPE_INT4 ); write(un,pos=pos) filli4(:n)
    case( DTYPE_INT8 ); write(un,pos=pos) filli8(:n)
    case( DTYPE_REAL ); write(un,pos=pos) fillr4(:n)
    case( DTYPE_DBLE ); write(un,pos=pos) fillr8(:n)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(filli1)
    case( DTYPE_INT2 ); deallocate(filli2)
    case( DTYPE_INT4 ); deallocate(filli4)
    case( DTYPE_INT8 ); deallocate(filli8)
    case( DTYPE_REAL ); deallocate(fillr4)
    case( DTYPE_DBLE ); deallocate(fillr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine fill_block__int2
!===============================================================
!
!===============================================================
subroutine fill_block__int4(job, fill, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  integer(4)  , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: filli1(:)
  integer(2), allocatable, save :: filli2(:)
  integer(4), allocatable, save :: filli4(:)
  integer(8), allocatable, save :: filli8(:)
  real(4)   , allocatable, save :: fillr4(:)
  real(8)   , allocatable, save :: fillr8(:)

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE fill_block__int4 ***\n'

  selectcase( job )
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(filli1(n)); filli1(:) = int(fill,1)
    case( DTYPE_INT2 ); allocate(filli2(n)); filli2(:) = int(fill,2)
    case( DTYPE_INT4 ); allocate(filli4(n)); filli4(:) = int(fill,4)
    case( DTYPE_INT8 ); allocate(filli8(n)); filli8(:) = int(fill,8)
    case( DTYPE_REAL ); allocate(fillr4(n)); fillr4(:) = real(fill,4)
    case( DTYPE_DBLE ); allocate(fillr8(n)); fillr8(:) = real(fill,8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); write(un,pos=pos) filli1(:n)
    case( DTYPE_INT2 ); write(un,pos=pos) filli2(:n)
    case( DTYPE_INT4 ); write(un,pos=pos) filli4(:n)
    case( DTYPE_INT8 ); write(un,pos=pos) filli8(:n)
    case( DTYPE_REAL ); write(un,pos=pos) fillr4(:n)
    case( DTYPE_DBLE ); write(un,pos=pos) fillr8(:n)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(filli1)
    case( DTYPE_INT2 ); deallocate(filli2)
    case( DTYPE_INT4 ); deallocate(filli4)
    case( DTYPE_INT8 ); deallocate(filli8)
    case( DTYPE_REAL ); deallocate(fillr4)
    case( DTYPE_DBLE ); deallocate(fillr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine fill_block__int4
!===============================================================
!
!===============================================================
subroutine fill_block__int8(job, fill, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  integer(8)  , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: filli1(:)
  integer(2), allocatable, save :: filli2(:)
  integer(4), allocatable, save :: filli4(:)
  integer(8), allocatable, save :: filli8(:)
  real(4)   , allocatable, save :: fillr4(:)
  real(8)   , allocatable, save :: fillr8(:)

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE fill_block__int8 ***\n'

  selectcase( job )
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(filli1(n)); filli1(:) = int(fill,1)
    case( DTYPE_INT2 ); allocate(filli2(n)); filli2(:) = int(fill,2)
    case( DTYPE_INT4 ); allocate(filli4(n)); filli4(:) = int(fill,4)
    case( DTYPE_INT8 ); allocate(filli8(n)); filli8(:) = int(fill,8)
    case( DTYPE_REAL ); allocate(fillr4(n)); fillr4(:) = real(fill,4)
    case( DTYPE_DBLE ); allocate(fillr8(n)); fillr8(:) = real(fill,8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); write(un,pos=pos) filli1(:n)
    case( DTYPE_INT2 ); write(un,pos=pos) filli2(:n)
    case( DTYPE_INT4 ); write(un,pos=pos) filli4(:n)
    case( DTYPE_INT8 ); write(un,pos=pos) filli8(:n)
    case( DTYPE_REAL ); write(un,pos=pos) fillr4(:n)
    case( DTYPE_DBLE ); write(un,pos=pos) fillr8(:n)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(filli1)
    case( DTYPE_INT2 ); deallocate(filli2)
    case( DTYPE_INT4 ); deallocate(filli4)
    case( DTYPE_INT8 ); deallocate(filli8)
    case( DTYPE_REAL ); deallocate(fillr4)
    case( DTYPE_DBLE ); deallocate(fillr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine fill_block__int8
!===============================================================
!
!===============================================================
subroutine fill_block__real(job, fill, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  real(4)     , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: filli1(:)
  integer(2), allocatable, save :: filli2(:)
  integer(4), allocatable, save :: filli4(:)
  integer(8), allocatable, save :: filli8(:)
  real(4)   , allocatable, save :: fillr4(:)
  real(8)   , allocatable, save :: fillr8(:)

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE fill_block__real ***\n'

  selectcase( job )
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(filli1(n)); filli1(:) = int(fill,1)
    case( DTYPE_INT2 ); allocate(filli2(n)); filli2(:) = int(fill,2)
    case( DTYPE_INT4 ); allocate(filli4(n)); filli4(:) = int(fill,4)
    case( DTYPE_INT8 ); allocate(filli8(n)); filli8(:) = int(fill,8)
    case( DTYPE_REAL ); allocate(fillr4(n)); fillr4(:) = real(fill,4)
    case( DTYPE_DBLE ); allocate(fillr8(n)); fillr8(:) = real(fill,8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); write(un,pos=pos) filli1(:n)
    case( DTYPE_INT2 ); write(un,pos=pos) filli2(:n)
    case( DTYPE_INT4 ); write(un,pos=pos) filli4(:n)
    case( DTYPE_INT8 ); write(un,pos=pos) filli8(:n)
    case( DTYPE_REAL ); write(un,pos=pos) fillr4(:n)
    case( DTYPE_DBLE ); write(un,pos=pos) fillr8(:n)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(filli1)
    case( DTYPE_INT2 ); deallocate(filli2)
    case( DTYPE_INT4 ); deallocate(filli4)
    case( DTYPE_INT8 ); deallocate(filli8)
    case( DTYPE_REAL ); deallocate(fillr4)
    case( DTYPE_DBLE ); deallocate(fillr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine fill_block__real
!===============================================================
!
!===============================================================
subroutine fill_block__dble(job, fill, dtype, un, pos, n)
  implicit none
  integer     , intent(in) :: job
  real(8)     , intent(in) :: fill
  character(*), intent(in) :: dtype
  integer     , intent(in) :: un
  integer(8)  , intent(in) :: pos
  integer(8)  , intent(in) :: n

  integer(1), allocatable, save :: filli1(:)
  integer(2), allocatable, save :: filli2(:)
  integer(4), allocatable, save :: filli4(:)
  integer(8), allocatable, save :: filli8(:)
  real(4)   , allocatable, save :: fillr4(:)
  real(8)   , allocatable, save :: fillr8(:)

  character(128), parameter :: msg_proc = &
    '*** @ '//trim(NAME_LIB)//' SUBROUTINE fill_block__dble ***\n'

  selectcase( job )
  case( 0 )
    selectcase( dtype )
    case( DTYPE_INT1 ); allocate(filli1(n)); filli1(:) = int(fill,1)
    case( DTYPE_INT2 ); allocate(filli2(n)); filli2(:) = int(fill,2)
    case( DTYPE_INT4 ); allocate(filli4(n)); filli4(:) = int(fill,4)
    case( DTYPE_INT8 ); allocate(filli8(n)); filli8(:) = int(fill,8)
    case( DTYPE_REAL ); allocate(fillr4(n)); fillr4(:) = real(fill,4)
    case( DTYPE_DBLE ); allocate(fillr8(n)); fillr8(:) = real(fill,8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case( 1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); write(un,pos=pos) filli1(:n)
    case( DTYPE_INT2 ); write(un,pos=pos) filli2(:n)
    case( DTYPE_INT4 ); write(un,pos=pos) filli4(:n)
    case( DTYPE_INT8 ); write(un,pos=pos) filli8(:n)
    case( DTYPE_REAL ); write(un,pos=pos) fillr4(:n)
    case( DTYPE_DBLE ); write(un,pos=pos) fillr8(:n)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case( -1 )
    selectcase( dtype )
    case( DTYPE_INT1 ); deallocate(filli1)
    case( DTYPE_INT2 ); deallocate(filli2)
    case( DTYPE_INT4 ); deallocate(filli4)
    case( DTYPE_INT8 ); deallocate(filli8)
    case( DTYPE_REAL ); deallocate(fillr4)
    case( DTYPE_DBLE ); deallocate(fillr8)
    case default
      call eerr(trim(msg_proc)//'Invalid value in $dtype: '//str(dtype))
    endselect
  case default
    call eerr(trim(msg_proc)//'Invalid value in $job: '//str(job))
  endselect
end subroutine fill_block__dble
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
subroutine check_input_file(&
    path, dtype, sz, rec, check_recl, opt, info)
  implicit none
  character(*), intent(in)  :: path
  character(*), intent(in)  :: dtype
  integer(8)  , intent(in)  :: sz
  integer     , intent(in)  :: rec
  logical     , intent(in)  :: check_recl
  character(*), intent(in)  :: opt
  integer     , intent(out) :: info

  call echo(code%bgn, 'check_input_file', '-p -x2')
  !-------------------------------------------------------------
  call check_permission(&
         path, action_read, &
         allow_empty=.false., opt=opt, info=info)
  if( info /= 0 )then
    call echo(code%ret)
    return
  endif

  call check_input_filesize(&
         path, dtype, sz, rec, check_recl, opt, info)
  if( info /= 0 )then
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_input_file
!===============================================================
!
!===============================================================
subroutine prep_output_file(&
    path, dtype, sz, replace, opt, fs, info)
  implicit none
  character(*), intent(in)  :: path
  character(*), intent(in)  :: dtype
  integer(8)  , intent(in)  :: sz
  logical     , intent(in)  :: replace
  character(*), intent(in)  :: opt
  integer(8)  , intent(out) :: fs
  integer     , intent(out) :: info

  integer :: access

  call echo(code%bgn, 'prep_output_file', '-p -x2')
  !-------------------------------------------------------------
  fs = 0_8

  if( access(path,' ') == 0 )then
    call check_permission(&
           path, action_for_replace(replace), &
           allow_empty=.false., opt=opt, info=info)
    if( info /= 0 )then
      call echo(code%ret)
      return
    endif
  endif

  if( replace .and. access(path,' ') == 0 )then
    call remove(path, dir=.false., output=.false.)
  endif

  call check_output_filesize(path, dtype, sz, opt, fs, info)
  if( info /= 0 )then
    call echo(code%ret)
    return
  endif

  if( access(path,' ') /= 0 )then
    call make_empty_file(path, info, .false., opt)
    if( info /= 0 )then
      call echo(code%ret)
      return
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine prep_output_file
!===============================================================
!
!===============================================================
subroutine check_input_filesize(&
    path, dtype, sz, rec, check_recl, opt, info)
  implicit none
  character(*), intent(in)  :: path
  character(*), intent(in)  :: dtype
  integer(8)  , intent(in)  :: sz
  integer     , intent(in)  :: rec
  logical     , intent(in)  :: check_recl
  character(*), intent(in)  :: opt
  integer     , intent(out) :: info

  integer(8) :: fs
  integer    :: byte
  integer(8) :: recl
  integer(8) :: fs_min

  call echo(code%bgn, 'check_input_filesize', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0

  fs = filesize(path)
  byte = byte_of_dtype(dtype)
  recl = byte * sz
  fs_min = recl * rec

  if( mod(fs, recl) /= 0_8 )then
    if( check_recl )then
      call eerr('File size is invalid.'//&
              '\n  Path: '//str(path)//&
              '\n  Data type: '//str(dtype)//' ('//str(byte)//' byte)'//&
              '\n  Record length: '//str(recl)//&
              '\n  File size    : '//str(fs)//&
              '\nFile size must be a multiple number of record length.', &
              opt)
      info = 1
    endif
  else
    if( fs < fs_min )then
      call eerr('File size is invalid.'//&
              '\n  Path: '//str(path)//&
              '\n  Data type: '//str(dtype)//' ('//str(byte)//' byte)'//&
              '\n  File size                 : '//str(fs)//&
              '\n  Record length             : '//str(recl)//&
              '\n  Record number             : '//str(rec)//&
              '\n  Expected minimum file size: '//str(fs_min), &
              opt)
    info = 1
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_input_filesize
!===============================================================
!
!===============================================================
subroutine check_output_filesize(&
    path, dtype, sz, opt, fs, info)
  implicit none
  character(*), intent(in)  :: path
  character(*), intent(in)  :: dtype
  integer(8)  , intent(in)  :: sz
  character(*), intent(in)  :: opt
  integer(8)  , intent(out) :: fs
  integer     , intent(out) :: info

  integer    :: byte
  integer(8) :: recl

  integer :: access

  call echo(code%bgn, 'check_output_filesize', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0

  if( access(path,' ') /= 0 )then
    call echo(code%ret)
    return
  endif

  fs = filesize(path)
  byte = byte_of_dtype(dtype)
  recl = byte * sz

  if( mod(fs, recl) /= 0_8 )then
    call eerr('File size is invalid.'//&
            '\n  path: '//str(path)//&
            '\n  file size    : '//str(fs)//&
            '\n  record length: '//str(recl)//&
            '\nFile size must be a multiple number of record length.', &
            opt)
    info = 1
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_output_filesize
!===============================================================
!
!===============================================================
end module lib_io_binary_direct
