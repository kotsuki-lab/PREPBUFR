module lib_io_binary_stream
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
  public :: rbin_stream
  public :: wbin_stream
  !-------------------------------------------------------------
  interface rbin_stream
    module procedure :: rb__dble_1d
  end interface

  interface wbin_stream
    module procedure :: wb__dble_1d
  end interface
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine rb__dble_1d(&
    dat, path, dtype, endian, &
    pos, info)
  implicit none
  real(8)     , intent(out) :: dat(:)
  character(*), intent(in)  :: path
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer(8)  , intent(in) , optional :: pos
  integer     , intent(out), optional :: info

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer(8) :: pos_
  integer :: info_

  integer :: byte
  integer :: un

  character(clen_opt_error) :: opt

  call echo(code%bgn, 'rbin_stream__MP__rb__dble_1d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dtype_ = dtype_dble
  endian_ = endian_default
  pos_ = 1

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian
  if( present(pos) ) pos_ = pos

  byte = byte_of_dtype(dtype_)

  opt = ''
  if( present(info) ) opt = '-q -a'

  info_ = 0
  if( present(info) ) info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call check_input_file(&
         path, dtype_, size(dat,kind=8), pos_, opt, info_)
  if( info_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_input_file_stream(&
         un, path, endian_, info_, present(info))
  if( info_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif

  selectcase( dtype_ )
  case( dtype_int4 )
    dat = real(rb_core__int4_1d(un, pos_, shape(dat,kind=8)), 8)
  case( dtype_dble )
    dat = rb_core__dble_1d(un, pos_, shape(dat,kind=8))
  endselect

  call close_file(un, info_, present(info))
  if( info_ /= 0 )then
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

  if( info_ /= 0 )then
    if( .not. present(info) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  info_ /= 0 .and. .not. present(info)')
    endif
    info = info_
  endif
end subroutine finalize
end subroutine rb__dble_1d
!===============================================================
!
!===============================================================
subroutine wb__dble_1d(&
    dat, path, dtype, endian, &
    pos, replace, &
    info)
  implicit none
  real(8)     , intent(in) :: dat(:)
  character(*), intent(in) :: path
  character(*), intent(in) , optional :: dtype
  character(*), intent(in) , optional :: endian
  integer(8)  , intent(in) , optional :: pos
  logical     , intent(in) , optional :: replace
  integer     , intent(out), optional :: info

  character(clen_key) :: dtype_
  character(clen_key) :: endian_
  integer(8) :: pos_
  logical    :: replace_
  integer    :: info_

  integer :: un

  character(clen_opt_error) :: opt

  call echo(code%bgn, 'wbin_stream__MP__wb__dble_1d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dtype_ = dtype_dble
  endian_ = endian_default
  replace_ = .false.
  pos_ = 1_8

  if( present(dtype) ) dtype_ = dtype
  if( present(endian) ) endian_ = endian_long_name(endian)
  if( present(replace) ) replace_ = replace
  if( present(pos) ) pos_ = pos

  opt = ''
  if( present(info) ) opt = '-q -a'

  info_ = 0
  if( present(info) ) info = 0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call prep_output_file(path, replace_, opt, info_)
  if( info_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  un = unit_number()
  call open_output_file_stream(&
         un, path, endian_, replace_, &
         info_, present(info))
  if( info_ /= 0 )then
    call finalize()
    call echo(code%ret)
    return
  endif

  selectcase( dtype_ )
  case( dtype_int4 )
    call wb_core__int4_1d(int(dat,4), un, pos_)
  case( dtype_dble )
    call wb_core__dble_1d(dat, un, pos_)
  endselect

  call close_file(un, info_, present(info))
  if( info_ /= 0 )then
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

  if( info_ /= 0 )then
    if( .not. present(info) )then
      call eerr('INTERNAL ERROR: Unexpected condition'//&
              '\n  info_ /= 0 .and. .not. present(info)')
    endif
    info = info_
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
subroutine check_input_file(&
    path, dtype, length, pos, opt, info)
  implicit none
  character(*), intent(in)  :: path
  character(*), intent(in)  :: dtype
  integer(8)  , intent(in)  :: length
  integer(8)  , intent(in)  :: pos
  character(*), intent(in)  :: opt
  integer     , intent(out) :: info

  call echo(code%bgn, 'check_input_file', '-p -x2')
  !-------------------------------------------------------------
  info = 0

  call check_permission(&
         path, action_read, &
         allow_empty=.false., opt=opt, info=info)
  if( info /= 0 )then
    call echo(code%ret)
    return
  endif

  call check_input_filesize(&
         path, dtype, length, pos, opt, info)
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
subroutine prep_output_file(path, replace, opt, info)
  implicit none
  character(*), intent(in)  :: path
  logical     , intent(in)  :: replace
  character(*), intent(in)  :: opt
  integer     , intent(out) :: info

  integer :: access

  call echo(code%bgn, 'prep_output_file', '-p -x2')
  !-------------------------------------------------------------
  info = 0

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
    path, dtype, length, pos, opt, info)
  implicit none
  character(*), intent(in)  :: path
  character(*), intent(in)  :: dtype
  integer(8)  , intent(in)  :: length
  integer(8)  , intent(in)  :: pos
  character(*), intent(in)  :: opt
  integer     , intent(out) :: info

  integer(8) :: fs
  integer    :: byte
  integer(8) :: fs_min

  call echo(code%bgn, 'check_input_filesize', '-p -x2')
  !-------------------------------------------------------------
  info = 0

  fs = filesize(path)
  byte = byte_of_dtype(dtype)
  fs_min = byte * length + pos - 1

  if( fs < byte_of_dtype(dtype) * length + pos - 1 )then
    call eerr('File size is invalid.'//&
            '\n  Path: '//str(path)//&
            '\n  File size                 : '//str(fs)//&
            '\n  Starting position         : '//str(pos)//&
            '\n  Data size                 : '//str(byte*length)//&
            '\n  Expected minumum file size: '//str(fs_min), &
            opt)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_input_filesize
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
function rb_core__int4_1d(un, pos, shp) result(dat)
  implicit none
  integer   , intent(in) :: un
  integer(8), intent(in) :: pos
  integer(8), intent(in) :: shp(1)
  integer(4) :: dat(shp(1))

  read(un, pos=pos) dat
end function rb_core__int4_1d
!===============================================================
!
!===============================================================
function rb_core__dble_1d(un, pos, shp) result(dat)
  implicit none
  integer   , intent(in) :: un
  integer(8), intent(in) :: pos
  integer(8), intent(in) :: shp(1)
  real(8) :: dat(shp(1))

  read(un, pos=pos) dat
end function rb_core__dble_1d
!===============================================================
!
!===============================================================
subroutine wb_core__int4_1d(dat, un, pos)
  implicit none
  integer(4), intent(in) :: dat(:)
  integer   , intent(in) :: un
  integer(8), intent(in) :: pos

  write(un, pos=pos) dat
end subroutine wb_core__int4_1d
!===============================================================
!
!===============================================================
subroutine wb_core__dble_1d(dat, un, pos)
  implicit none
  real(8)   , intent(in) :: dat(:)
  integer   , intent(in) :: un
  integer(8), intent(in) :: pos

  write(un, pos=pos) dat
end subroutine wb_core__dble_1d
!===============================================================
!
!===============================================================
end module lib_io_binary_stream
