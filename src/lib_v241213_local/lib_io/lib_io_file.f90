module lib_io_file
  use lib_const, only: &
    int4_ulim, &
    clen_line, &
    clen_path, &
    clen_key, &
    clen_var, &
    clen_opt_error, &
    id_undef, &
    endian_little, &
    endian_big, &
    endian_little_short, &
    endian_big_short, &
    endian_default, &
    endian_undef, &
    dtype_int1, &
    dtype_int2, &
    dtype_int4, &
    dtype_int8, &
    dtype_real, &
    dtype_dble, &
    dtype_undef, &
    rec_undef, &
    action_read, &
    action_write, &
    action_readwrite, &
    action_undef, &
    permission_rw, &
    permission_undef, &
    status_unknown, &
    status_undef, &
    dgt_opt_max
  use lib_base, only: &
    msg_io_error, &
    msg_invalid_value, &
    msg_unexpected_condition
  use lib_log, only: &
    code, &
    str, &
    dgt, &
    echo, &
    edbg, &
    eerr
  use lib_io_base, only: &
    unit_number, &
    byte_of_dtype
  use lib_array, only: &
    reversed
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: file_
  public :: filedim

  public :: joined
  public :: dirname
  public :: filename
  public :: path_ins
  public :: filesize

  public :: init_file
  public :: update_file
  public :: set_path
  public :: set_dtype
  public :: set_rec
  public :: set_endian
  public :: set_stat
  public :: set_length
  public :: set_size
  public :: set_lower
  public :: set_upper

  public :: file

  public :: fileinfo

  public :: set_opt_check_permission
  public :: init_opt_check_permission
  public :: check_permission
  public :: check_file_size
  public :: try_make_empty_file
  public :: set_opt_mkdir
  public :: init_opt_mkdir
  public :: mkdir
  public :: remove
  public :: make_empty_file
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  interface set_path
    module procedure set_path_0d
    module procedure set_path_1d
  end interface

  interface set_dtype
    module procedure set_dtype_0d
    module procedure set_dtype_1d
  end interface

  interface set_rec
    module procedure set_rec_0d
    module procedure set_rec_1d
  end interface

  interface set_endian
    module procedure set_endian_log4_0d
    module procedure set_endian_log4_1d
    module procedure set_endian_char_0d
    module procedure set_endian_char_1d
  end interface

  interface set_stat
    module procedure set_stat_0d
    module procedure set_stat_1d
  end interface

  interface set_length
    module procedure set_length_0d
    module procedure set_length_1d
  end interface

  interface set_size
    module procedure set_size_0d
    module procedure set_size_1d
  end interface

  interface set_lower
    module procedure set_lower_0d
    module procedure set_lower_1d
  end interface

  interface set_upper
    module procedure set_upper_0d
    module procedure set_upper_1d
  end interface

  interface try_make_empty_file
    module procedure try_make_empty_file_file
    module procedure try_make_empty_file_dir
  end interface

  interface check_permission
    module procedure check_permission_file
    module procedure check_permission_path
  end interface
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  integer, parameter :: filedim = 3

  type file_
    character(clen_path) :: id         = ''
    character(clen_path) :: path       = ''
    character(clen_key)  :: dtype      = dtype_undef
    character(clen_key)  :: endian     = ''
    integer              :: rec        = rec_undef
    integer(8)           :: length     = 0
    character(clen_key)  :: status     = ''
    character(clen_key)  :: action     = action_undef
    integer              :: permission = permission_undef
    integer(8)           :: sz(filedim)  ! Size of each record
    integer(8)           :: lb(filedim)  ! Lower bounds in the records
    integer(8)           :: ub(filedim)  ! Upper bounds       "
  end type

  character(2)        , parameter :: opt_default_mkdir__hut = '+ '
  character(clen_line), save      :: opt_mkdir__hut = opt_default_mkdir__hut

  integer, parameter :: opt_default_mkdir__clen_hut = 2
  integer, save      :: opt_mkdir__clen_hut = opt_default_mkdir__clen_hut

  logical, parameter :: opt_default_mkdir__output = .false.
  logical, save      :: opt_mkdir__output = opt_default_mkdir__output

  logical, parameter :: opt_default_check_permission__allow_empty = .false.
  logical, save      :: opt_check_permission__allow_empty &
                          = opt_default_check_permission__allow_empty
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
function joined(path1, path2, is_dir) result(path)
  implicit none
  character(*), intent(in)           :: path1, path2
  logical     , intent(in), optional :: is_dir  ! path2 is dir
  character(len_joined(path1,path2)) :: path
  character(len_trim(path1)) :: path1_
  character(len_trim(path2)) :: path2_
  logical :: is_dir_

  is_dir_ = .false.
  if( present(is_dir) ) is_dir_ = is_dir

  if( len_trim(path2) == 0 )then
    if( is_dir_ )then
      path = path1
    else
      path = ''
    endif

  elseif( len_trim(path1) == 0 .or. path2(1:1) == '/' )then
    path = trim(path2)

  else
    if( path1(len_trim(path1):len_trim(path1)) == '/' )then
      path1_ = path1(:len_trim(path1)-1)
    else
      path1_ = path1
    endif

    path2_ = path2
    do while( index(path2_,'../') == 1 )
      path1_ = path1_(:len_trim(path1_)-index(reversed(trim(path1_)),'/'))
      path2_ = path2_(4:)
    enddo

    path = trim(path1_)//'/'//trim(path2_)
  endif
end function joined
!===============================================================
!
!===============================================================
integer pure function len_joined(dir, path) result(l)
  implicit none
  character(*), intent(in) :: dir
  character(*), intent(in) :: path

  if( len_trim(path) == 0 )then
    l = len_trim(dir)
  elseif( len_trim(dir) == 0 .or. path(1:1) == '/' )then
    l = len_trim(path)
  else
    if( dir(len_trim(dir):len_trim(dir)) == '/' )then
      l = len_trim(dir) + len_trim(path)
    else
      l = len_trim(dir) + len_trim(path) + 1
    endif
  endif

  l = max(1,l)
end function len_joined
!===============================================================
!
!===============================================================
function dirname(path) result(dir)
  implicit none
  character(*), intent(in)  :: path
  character(clen_dir(path)) :: dir

  if( index(path,'/') == 0 )then
    dir = './'
  else
    dir = path(:len_trim(path)-index(reversed(trim(path)),'/'))
  endif
end function dirname
!===============================================================
!
!===============================================================
function filename(path) result(file)
  implicit none
  character(*), intent(in)   :: path
  character(clen_file(path)) :: file

  if( index(path,'/') == 0 )then
    file = path
  else
    file = path(len_trim(path)-index(reversed(trim(path)),'/')+2:)
  endif
end function filename
!===============================================================
!
!===============================================================
integer pure function clen_dir(path) result(l)
  implicit none
  character(*), intent(in) :: path

  if( index(path,'/') == 0 )then
    l = 0
  else
    l = len_trim(path) - index(reversed(trim(path)),'/')
  endif
end function clen_dir
!===============================================================
!
!===============================================================
integer pure function clen_file(path) result(l)
  implicit none
  character(*), intent(in) :: path

  if( index(path,'/') == 0 )then
    l = len_trim(path)
  else
    l = index(reversed(trim(path)),'/') - 1
  endif
end function clen_file
!===============================================================
!
!===============================================================
integer(8) function filesize(path) result(sz)
  implicit none
  character(*), intent(in) :: path

  integer :: un
  integer :: ios

  call echo(code%bgn, 'filesize', '-p')
  !-------------------------------------------------------------
  un = unit_number()
  open(un, file=path, status='old', action='read', iostat=ios)

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\nFailed to open file.'//&
            '\n  path: '//str(path))
  endif

  inquire(un, size=sz)
  close(un)
  !-------------------------------------------------------------
  call echo(code%ret)
end function filesize
!===============================================================
!
!===============================================================
function path_ins(path, s, no_ext)
  implicit none
  character(*), intent(in)           :: path
  character(*), intent(in)           :: s
  logical     , intent(in), optional :: no_ext
  character(:), allocatable          :: path_ins

  character(len_trim(path)) :: f
  logical :: no_ext_
  integer :: cl_ext, cl
  integer :: loc

  call echo(code%bgn, 'path_ins', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  no_ext_ = .false.
  if( present(no_ext) ) no_ext_ = no_ext

  cl = len_trim(path) + len_trim(s)
  allocate(character(cl) :: path_ins)

  if( no_ext_ )then
    path_ins = trim(path)//trim(s)
  else
    f = filename(path)
    cl_ext = index(reversed(trim(f)),'.') - 1
    if( cl_ext == 0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Length of extension is zero')
    endif
    loc = len_trim(path) - (cl_ext + 1)
    path_ins = path(:loc)//trim(s)//path(loc+1:)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end function path_ins
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
type(file_) function file(path, dtype, endian, rec, &
                          length, status, action, permission, id, &
                          sz1, sz2, sz3, lb1, lb2, lb3, ub1, ub2, ub3) result(f)
  implicit none
  character(*), intent(in), optional :: path
  character(*), intent(in), optional :: dtype
  character(*), intent(in), optional :: endian
  integer     , intent(in), optional :: rec
  integer(8)  , intent(in), optional :: length
  character(*), intent(in), optional :: status
  character(*), intent(in), optional :: action
  integer     , intent(in), optional :: permission
  character(*), intent(in), optional :: id
  integer(8)  , intent(in), optional :: sz1, sz2, sz3
  integer(8)  , intent(in), optional :: lb1, lb2, lb3
  integer(8)  , intent(in), optional :: ub1, ub2, ub3

  call set_file_default(f)

  if( present(path)       ) f%path       = path
  if( present(dtype)      ) f%dtype      = dtype
  if( present(rec)        ) f%rec        = rec
  if( present(endian)     ) f%endian     = endian
  if( present(length)     ) f%length     = length
  if( present(status)     ) f%status     = status
  if( present(action)     ) f%action     = action
  if( present(permission) ) f%permission = permission
  if( present(id)         ) f%id         = id
  if( present(sz1)        ) f%sz(1)      = sz1
  if( present(sz2)        ) f%sz(2)      = sz2
  if( present(sz3)        ) f%sz(3)      = sz3
  if( present(lb1)        ) f%lb(1)      = lb1
  if( present(lb2)        ) f%lb(2)      = lb2
  if( present(lb3)        ) f%lb(3)      = lb3
  if( present(ub1)        ) f%ub(1)      = ub1
  if( present(ub2)        ) f%ub(2)      = ub2
  if( present(ub3)        ) f%ub(3)      = ub3
end function file
!===============================================================
!
!===============================================================
subroutine init_file(f)
  implicit none
  type(FILE_), intent(out) :: f

  f%id         = id_undef
  f%path       = ''
  f%dtype      = dtype_undef
  f%rec        = rec_undef
  f%endian     = endian_undef
  f%length     = 0_8
  f%status     = status_undef
  f%action     = action_undef
  f%permission = permission_undef
  f%sz(:)      = 0_8
  f%lb(:)      = 0_8
  f%ub(:)      = 0_8
end subroutine init_file
!===============================================================
!
!===============================================================
subroutine set_file_default(f)
  implicit none
  type(file_), intent(out) :: f

  f%id         = id_undef
  f%path       = ''
  f%dtype      = dtype_dble
  f%rec        = 1
  f%endian     = endian_default
  f%length     = 0_8
  f%status     = status_unknown
  f%action     = action_readwrite
  f%permission = permission_rw
  f%sz(:)      = 0_8
  f%lb(:)      = 0_8
  f%ub(:)      = 0_8
end subroutine set_file_default
!===============================================================
!
!===============================================================
subroutine update_file(&
    f, &
    id, path, dtype, rec, endian, &
    length, status, action, permission, sz, lb, ub)
  implicit none
  type(file_), intent(inout) :: f
  character(*), intent(in), optional :: id
  character(*), intent(in), optional :: path 
  character(*), intent(in), optional :: dtype
  integer     , intent(in), optional :: rec
  character(*), intent(in), optional :: endian
  integer(8)  , intent(in), optional :: length
  character(*), intent(in), optional :: status
  character(*), intent(in), optional :: action
  integer     , intent(in), optional :: permission
  integer(8)  , intent(in), optional :: sz(:)
  integer(8)  , intent(in), optional :: lb(:)
  integer(8)  , intent(in), optional :: ub(:)

  call echo(code%bgn, 'update_file', '-p -x2')
  !-------------------------------------------------------------
  if( present(id) ) f%id = id
  if( present(path) ) f%path = path
  if( present(dtype) ) f%dtype = dtype
  if( present(rec) ) f%rec = rec
  if( present(endian) ) f%endian = endian
  if( present(length) ) f%length = length
  if( present(status) ) f%status = status
  if( present(action) ) f%action = action
  if( present(permission) ) f%permission = permission
  if( present(sz) ) f%sz(:size(sz)) = f%sz(:size(sz))
  if( present(lb) ) f%lb(:size(lb)) = f%lb(:size(lb))
  if( present(ub) ) f%ub(:size(ub)) = f%ub(:size(ub))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine update_file
!===============================================================
!
!===============================================================
subroutine set_path_0d(path, path_specific, path_general, path_default, dir)
  implicit none
  character(*), intent(inout) :: path
  character(*), intent(in)    :: path_specific
  character(*), intent(in), optional :: path_general
  character(*), intent(in), optional :: path_default
  character(*), intent(in), optional :: dir
  
  if( path == '' )then
    if( present(path_default) )then
      if( path_default /= '' ) path = path_default
    endif

    if( present(path_general) )then
      if( path_general /= '' ) path = path_general
    endif

    if( path_specific /= '' ) path = path_specific
  endif

  if( present(dir) ) path = joined(dir, path)
end subroutine set_path_0d
!===============================================================
!
!===============================================================
subroutine set_path_1d(path, path_specific, path_general, path_default, dir)
  implicit none
  character(*), intent(inout) :: path(:)
  character(*), intent(in)    :: path_specific
  character(*), intent(in), optional :: path_general
  character(*), intent(in), optional :: path_default
  character(*), intent(in), optional :: dir
  integer :: i

  do i = 1, size(path)
    if( path(i) == '' )then
      if( present(path_default) )then
        if( path_default /= '' ) path(i) = path_default
      endif

      if( present(path_general) )then
        if( path_general /= '' ) path(i) = path_general
      endif

      if( path_specific /= '' ) path(i) = path_specific
    endif

    if( present(dir) ) path(i) = joined(dir, path(i))
  enddo
end subroutine set_path_1d
!===============================================================
!
!===============================================================
subroutine set_dtype_0d(dtype, dtype_specific, dtype_general, dtype_default)
  implicit none
  character(*), intent(inout) :: dtype
  character(*), intent(in)    :: dtype_specific
  character(*), intent(in), optional :: dtype_general
  character(*), intent(in), optional :: dtype_default

  if( dtype == '' )then
    if( present(dtype_default) )then
      if( dtype_default /= '' ) dtype = trim(dtype_default)
    endif

    if( present(dtype_general) )then
      if( dtype_general /= '' ) dtype = trim(dtype_general)
    endif

    if( dtype_specific /= '' ) dtype = trim(dtype_specific)
  endif
end subroutine set_dtype_0d
!===============================================================
!
!===============================================================
subroutine set_dtype_1d(dtype, dtype_specific, dtype_general, dtype_default)
  implicit none
  character(*), intent(inout) :: dtype(:)
  character(*), intent(in)    :: dtype_specific
  character(*), intent(in), optional :: dtype_general
  character(*), intent(in), optional :: dtype_default
  integer :: i

  do i = 1, size(dtype)
    if( dtype(i) == '' )then
      if( present(dtype_default) )then
        if( dtype_default /= '' ) dtype(i) = trim(dtype_default)
      endif

      if( present(dtype_general) )then
        if( dtype_general /= '' ) dtype(i) = trim(dtype_general)
      endif

      if( dtype_specific /= '' ) dtype(i) = trim(dtype_specific)
    endif
  enddo
end subroutine set_dtype_1d
!===============================================================
!
!===============================================================
subroutine set_rec_0d(rec, rec_specific, rec_general, rec_default)
  implicit none
  integer, intent(inout) :: rec
  integer, intent(in)    :: rec_specific
  integer, intent(in), optional :: rec_general
  integer, intent(in), optional :: rec_default

  if( rec <= 0 )then
    if( present(rec_default) )then
      if( rec_default > 0 ) rec = rec_default
    endif

    if( present(rec_general) )then
      if( rec_general > 0 ) rec = rec_general
    endif

    if( rec_specific > 0 ) rec = rec_specific
  endif
end subroutine set_rec_0d
!===============================================================
!
!===============================================================
subroutine set_rec_1d(rec, rec_specific, rec_general, rec_default)
  implicit none
  integer, intent(inout) :: rec(:)
  integer, intent(in)    :: rec_specific
  integer, intent(in), optional :: rec_general
  integer, intent(in), optional :: rec_default
  integer :: i

  do i = 1, size(rec)
    if( rec(i) <= 0 )then
      if( present(rec_default) )then
        if( rec_default > 0 ) rec(i) = rec_default
      endif

      if( present(rec_general) )then
        if( rec_general > 0 ) rec(i) = rec_general
      endif

      if( rec_specific > 0 ) rec(i) = rec_specific
    endif
  enddo
end subroutine set_rec_1d
!===============================================================
!
!===============================================================
subroutine set_endian_log4_0d(little, endian_specific, endian_general, endian_default)
  implicit none
  logical     , intent(out) :: little
  character(*), intent(in)  :: endian_specific
  character(*), intent(in), optional :: endian_general
  character(*), intent(in), optional :: endian_default

  if( present(endian_default) )then
    if( endian_default /= '' ) little = ( endian_default == endian_little )
  endif

  if( present(endian_general) )then
    if( endian_general /= '' ) little = ( endian_general == endian_little )
  endif

  if( endian_specific /= '' ) little = ( endian_specific == endian_little )
end subroutine set_endian_log4_0d
!===============================================================
!
!===============================================================
subroutine set_endian_log4_1d(little, endian_specific, endian_general, endian_default)
  implicit none
  logical     , intent(out) :: little(:)
  character(*), intent(in)  :: endian_specific
  character(*), intent(in), optional :: endian_general
  character(*), intent(in), optional :: endian_default

  if( present(endian_default) )then
    if( endian_default /= '' ) little = ( endian_default == endian_little )
  endif

  if( present(endian_general) )then
    if( endian_general /= '' ) little = ( endian_general == endian_little )
  endif

  if( endian_specific /= '' ) little = ( endian_specific == endian_little )
end subroutine set_endian_log4_1d
!===============================================================
!
!===============================================================
subroutine set_endian_char_0d(endian, endian_specific, endian_general, endian_default)
  implicit none
  character(*), intent(inout) :: endian
  character(*), intent(in)    :: endian_specific
  character(*), intent(in), optional :: endian_general
  character(*), intent(in), optional :: endian_default

  if( endian == '' )then
    if( present(endian_default) )then
      if( endian_default /= '' ) endian = endian_default
    endif

    if( present(endian_general) )then
      if( endian_general /= '' ) endian = endian_general
    endif

    if( endian_specific /= '' ) endian = endian_specific
  endif
end subroutine set_endian_char_0d
!===============================================================
!
!===============================================================
subroutine set_endian_char_1d(endian, endian_specific, endian_general, endian_default)
  implicit none
  character(*), intent(inout) :: endian(:)
  character(*), intent(in)    :: endian_specific
  character(*), intent(in), optional :: endian_general
  character(*), intent(in), optional :: endian_default
  integer :: i

  do i = 1, size(endian)
    if( endian(i) == '' )then
      if( present(endian_default) )then
        if( endian_default /= '' ) endian(i) = endian_default
      endif

      if( present(endian_general) )then
        if( endian_general /= '' ) endian(i) = endian_general
      endif

      if( endian_specific /= '' ) endian(i) = endian_specific
    endif
  enddo
end subroutine set_endian_char_1d
!===============================================================
!
!===============================================================
subroutine set_stat_0d(stat, stat_specific, stat_general, stat_default)
  implicit none
  character(*), intent(inout) :: stat
  character(*), intent(in)    :: stat_specific
  character(*), intent(in), optional :: stat_general
  character(*), intent(in), optional :: stat_default

  if( stat == '' )then
    if( present(stat_default) )then
      if( stat_default /= '' ) stat = stat_default
    endif

    if( present(stat_general) )then
      if( stat_general /= '' ) stat = stat_general
    endif

    if( stat_specific /= '' ) stat = stat_specific
  endif
end subroutine set_stat_0d
!===============================================================
!
!===============================================================
subroutine set_stat_1d(stat, stat_specific, stat_general, stat_default)
  implicit none
  character(*), intent(inout) :: stat(:)
  character(*), intent(in)    :: stat_specific
  character(*), intent(in), optional :: stat_general
  character(*), intent(in), optional :: stat_default
  integer :: i

  do i = 1, size(stat)
    if( stat(i) == '' )then
      if( present(stat_default) )then
        if( stat_default /= '' ) stat(i) = stat_default
      endif

      if( present(stat_general) )then
        if( stat_general /= '' ) stat(i) = stat_general
      endif

      if( stat_specific /= '' ) stat(i) = stat_specific
    endif
  enddo
end subroutine set_stat_1d
!===============================================================
!
!===============================================================
subroutine set_length_0d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v
  integer(8), intent(in)    :: v_specific
  integer(8), intent(in), optional :: v_general
  integer(8), intent(in), optional :: v_default

  if( v == 0 )then
    if( present(v_default) )then
      if( v_default /= 0 ) v = v_default
    endif

    if( present(v_general) )then
      if( v_general /= 0 ) v = v_general
    endif

    if( v_specific /= 0 ) v = v_specific
  endif
end subroutine set_length_0d
!===============================================================
!
!===============================================================
subroutine set_length_1d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v(:)
  integer(8), intent(in)    :: v_specific
  integer(8), intent(in), optional :: v_general
  integer(8), intent(in), optional :: v_default
  integer :: i

  do i = 1, size(v)
    if( v(i) == 0 )then
      if( present(v_default) )then
        if( v_default /= 0 ) v(i) = v_default
      endif

      if( present(v_general) )then
        if( v_general /= 0 ) v(i) = v_general
      endif

      if( v_specific /= 0 ) v(i) = v_specific
    endif
  enddo
end subroutine set_length_1d
!===============================================================
!
!===============================================================
subroutine set_size_0d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v
  integer(8), intent(in)    :: v_specific
  integer(8), intent(in), optional :: v_general
  integer(8), intent(in), optional :: v_default

  if( v == 0 )then
    if( present(v_default) )then
      if( v_default /= 0 ) v = v_default
    endif

    if( present(v_general) )then
      if( v_general /= 0 ) v = v_general
    endif

    if( v_specific /= 0 ) v = v_specific
  endif
end subroutine set_size_0d
!===============================================================
!
!===============================================================
subroutine set_size_1d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v(:)
  integer(8), intent(in)    :: v_specific(:)
  integer(8), intent(in), optional :: v_general(:)
  integer(8), intent(in), optional :: v_default(:)
  integer :: i

  do i = 1, size(v)
    if( v(i) == 0 )then
      if( present(v_default) )then
        if( v_default(i) /= 0 ) v(i) = v_default(i)
      endif

      if( present(v_general) )then
        if( v_general(i) /= 0 ) v(i) = v_general(i)
      endif

      if( v_specific(i) /= 0 ) v(i) = v_specific(i)
    endif
  enddo
end subroutine set_size_1d
!===============================================================
!
!===============================================================
subroutine set_lower_0d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v
  integer(8), intent(in)    :: v_specific
  integer(8), intent(in), optional :: v_general
  integer(8), intent(in), optional :: v_default

  if( v == 0 )then
    if( present(v_default) )then
      if( v_default /= 0 ) v = v_default
    endif

    if( present(v_general) )then
      if( v_general /= 0 ) v = v_general
    endif

    if( v_specific /= 0 ) v = v_specific
  endif
end subroutine set_lower_0d
!===============================================================
!
!===============================================================
subroutine set_lower_1d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v(:)
  integer(8), intent(in)    :: v_specific(:)
  integer(8), intent(in), optional :: v_general(:)
  integer(8), intent(in), optional :: v_default(:)
  integer :: i

  do i = 1, size(v)
    if( v(i) == 0 )then
      if( present(v_default) )then
        if( v_default(i) /= 0 ) v(i) = v_default(i)
      endif

      if( present(v_general) )then
        if( v_general(i) /= 0 ) v(i) = v_general(i)
      endif

      if( v_specific(i) /= 0 ) v(i) = v_specific(i)
    endif
  enddo
end subroutine set_lower_1d
!===============================================================
!
!===============================================================
subroutine set_upper_0d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v
  integer(8), intent(in)    :: v_specific
  integer(8), intent(in), optional :: v_general
  integer(8), intent(in), optional :: v_default

  if( v == 0 )then
    if( present(v_default) )then
      if( v_default /= 0 ) v = v_default
    endif

    if( present(v_general) )then
      if( v_general /= 0 ) v = v_general
    endif

    if( v_specific /= 0 ) v = v_specific
  endif
end subroutine set_upper_0d
!===============================================================
!
!===============================================================
subroutine set_upper_1d(v, v_specific, v_general, v_default)
  implicit none
  integer(8), intent(inout) :: v(:)
  integer(8), intent(in)    :: v_specific(:)
  integer(8), intent(in), optional :: v_general(:)
  integer(8), intent(in), optional :: v_default(:)
  integer :: i

  do i = 1, size(v)
    if( v(i) == 0 )then
      if( present(v_default) )then
        if( v_default(i) /= 0 ) v(i) = v_default(i)
      endif

      if( present(v_general) )then
        if( v_general(i) /= 0 ) v(i) = v_general(i)
      endif

      if( v_specific(i) /= 0 ) v(i) = v_specific(i)
    endif
  enddo
end subroutine set_upper_1d
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
function fileinfo(f) result(info)
  implicit none
  type(file_), intent(in)  :: f
  character(:), allocatable :: info
  character(clen_key) :: endian
  integer             :: clen_info
  logical             :: is_ok
  character(clen_key) :: opt
  character(4+dgt(INT4_ULIM)) :: str_rec

  call echo(code%bgn, 'fileinfo', '-p')
  !-------------------------------------------------------------
  if( f%path == '' )then
    allocate(character(10) :: info)
    info = '(Not specified)'

  else
    is_ok = .true.
    opt = '-q'
    !-----------------------------------------------------------
    ! Set endian
    !-----------------------------------------------------------
    selectcase( f%endian )
    case( endian_little, &
          endian_little_short )
      endian = endian_little_short
    case( endian_big, &
          endian_big_short )
      endian = endian_big_short
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  endian: '//str(f%endian), opt)
      is_ok = .false.
      opt = '-p '//trim(opt)
    endselect
    !-----------------------------------------------------------
    ! Check dtype
    !-----------------------------------------------------------
    selectcase( f%dtype )
    case( dtype_int1, &
          dtype_int2, &
          dtype_int4, &
          dtype_int8, &
          dtype_real, &
          dtype_dble )
      continue
    case( dtype_undef )
      continue
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  dtype: '//str(f%dtype)//&
              '\nOnly the following values are allowed for data type:'//&
              "\n'"//trim(dtype_int1)//"', '"//trim(dtype_int2)//"', '"//&
                     trim(dtype_int4)//"', '"//trim(dtype_int8)//"', '"//&
                     trim(dtype_real)//"', '"//trim(dtype_dble)//"'", &
                opt)
      is_ok = .false.
      opt = '-p '//trim(opt)
    endselect
    !-----------------------------------------------------------
    ! Check rec
    !-----------------------------------------------------------
    if( f%rec > 0 )then
      str_rec = 'rec:'//str(f%rec)
    elseif( f%rec == rec_undef )then
      str_rec = 'rec:undef'
    else
      call eerr(str(msg_invalid_value())//&
              '\n  rec: '//str(f%rec)//&
              '\nRecord number must be a positive number.', &
                opt)
      is_ok = .false.
      opt = '-p '//trim(opt)
    endif
    !-----------------------------------------------------------
    ! Check length
    !-----------------------------------------------------------
    if( f%length < 0 )then
      call eerr(str(msg_invalid_value())//&
              '\n  length: '//str(f%length)//&
              '\nData size must be positive.', &
                opt)
      is_ok = .false.
      opt = '-p '//trim(opt)
    endif
    !-----------------------------------------------------------
    ! Raise error message
    !-----------------------------------------------------------
    if( .not. is_ok )then
      call eerr('File settings are invalid.'//&
                '  \nID           : '//str(f%id)//&
                '  \nPath         : "'//str(f%path)//'"'//&
                '  \nData type    : "'//str(f%dtype)//'"'//&
                '  \nRecord number: '//str(f%rec)//&
                '  \nEndian       : "'//str(f%endian)//'"'//&
                '  \nData length  : '//str(f%length), &
                '+q')
    endif
    !-----------------------------------------------------------
    ! Generate the string of file information
    !-----------------------------------------------------------
    if( f%length == 0 )then
      clen_info = len_trim(f%path) &
                  + 2 + len_trim(endian) &
                  + 1 + len_trim(f%dtype) &
                  + 1 + len_trim(str_rec) &
                  + 1
      allocate(character(clen_info) :: info)
      info = str(f%path)//' ('//str(endian)//' '//&
             str(f%dtype)//' '//str(str_rec)//')'
    else
      clen_info = len_trim(f%path) &
                  + 1 + len_trim(endian) &
                  + 1 + len_trim(f%dtype) &
                  + 1 + len_trim(str_rec) &
                  + 5 + dgt(f%length) &
                  + 1
      allocate(character(clen_info) :: info)
      info = str(f%path)//' ('//str(endian)//' '//str(f%dtype)//&
             ' '//str(str_rec)//' length:'//str(f%length)//')'
    endif

  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end function fileinfo
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
subroutine set_opt_check_permission(allow_empty)
  implicit none
  logical, intent(in), optional :: allow_empty

  call echo(code%bgn, 'set_opt_check_permission', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(allow_empty) )then
    opt_check_permission__allow_empty = allow_empty
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_opt_check_permission
!===============================================================
!
!===============================================================
subroutine init_opt_check_permission(key)
  implicit none
  character(*), intent(in) :: key

  call echo(code%bgn, 'init_opt_check_permission', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( key )
  case( 'allow_empty' )
    opt_check_permission__allow_empty = opt_default_check_permission__allow_empty
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  key: '//str(key))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_opt_check_permission
!===============================================================
!
!===============================================================
subroutine check_permission_file(f, action, id, allow_empty)
  implicit none
  type(file_) , intent(in) :: f
  character(*), intent(in), optional :: action
  character(*), intent(in), optional :: id
  logical     , intent(in), optional :: allow_empty

  character(clen_key) :: action_
  character(:), allocatable :: id_
  logical :: allow_empty_

  character(clen_var), parameter :: id_default = 'f%path'

  integer :: cl

  integer :: access

  call echo(code%bgn, 'check_permission__MP__check_permission_file', '-p -x2')
  !-------------------------------------------------------------
  ! Options
  !-------------------------------------------------------------
  if( present(action) )then
    selectcase( action )
    case( action_read, &
          action_write, &
          action_readwrite, &
          action_undef )
      action_ = action
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  action: '//str(action))
    endselect
  else
    selectcase( f%action )
    case( action_read, &
          action_write, &
          action_readwrite, &
          action_undef )
      action_ = f%action
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  action: '//str(action)//&
              '\n  id: '//str(f%id)//&
              '\n  path: '//str(f%path))
    endselect
  endif

  if( present(id) )then
    cl = len(id)
    allocate(character(cl) :: id_)
    id_ = id
  else
    if( f%id == '' )then
      cl = len_trim(id_default)
      allocate(character(cl) :: id_)
      id_ = trim(id_default)
    else
      cl = len_trim(f%id)
      allocate(character(cl) :: id_)
      id_ = trim(f%id)
    endif
  endif

  allow_empty_ = opt_check_permission__allow_empty
  if( present(allow_empty) ) allow_empty_ = allow_empty
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( f%path == '' )then
    if( .not. allow_empty_ )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  f%path == "" .and. .not. allow_empty'//&
              '\n  id: '//str(f%id))
    endif

    call echo(code%ret)
    return
  endif

  selectcase( action_ )
  case( action_undef )
    continue
  case( action_read )
    if( access(f%path,' ') == 0 )then
      if( access(f%path,'r') /= 0 )then
        call eerr(str(msg_unexpected_condition())//&
                "\nYou don't have read permission for '"//'"'//str(f%path)//'".')
      endif
    else
      call eerr(str(msg_unexpected_condition())//&
              '\n  File does not exist.'//&
              '\n  id  : '//str(id_)//&
              '\n  path: '//str(f%path))
    endif
  case( action_write )
    if( access(f%path,' ') == 0 )then
      if( access(f%path,'w') /= 0 )then
        call eerr(str(msg_unexpected_condition())//&
                "\nYou don't have write permission for '"//'"'//str(f%path)//'".')
      endif
    else
      call try_make_empty_file(f)
    endif
  case( action_readwrite )
    if( access(f%path,' ') == 0 )then
      if( access(f%path,'r') /= 0 .or. access(f%path,'w') /= 0 )then
        call eerr(str(msg_unexpected_condition())//&
                "\nYou don't have readwrite permission for '"//'"'//str(f%path)//'".')
      endif
    else
      call try_make_empty_file(f)
    endif
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  action_: '//str(action_))
  endselect
  !-------------------------------------------------------------
  deallocate(id_)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_permission_file
!===============================================================
!
!===============================================================
subroutine check_permission_path(path, action, id, allow_empty, opt, info)
  implicit none
  character(*), intent(in) :: path
  character(*), intent(in) :: action
  character(*), intent(in) , optional :: id
  logical     , intent(in) , optional :: allow_empty
  character(*), intent(in) , optional :: opt
  integer     , intent(out), optional :: info

  character(:), allocatable :: id_
  logical :: allow_empty_
  integer :: cl
  character(clen_opt_error) :: opt_
  integer :: info_
  integer :: un

  integer :: access

  call echo(code%bgn, 'check_permission__MP__check_permission_path', '-p -x2')
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  allow_empty_ = opt_check_permission__allow_empty
  if( present(allow_empty) ) allow_empty_ = allow_empty

  if( present(id) )then
    cl = len_trim(id)
    allocate(character(cl) :: id_)
    id_ = trim(id)
  else
    allocate(character(16) :: id_)
    id_ = id_undef
  endif

  opt_ = ''
  if( present(info) ) opt_ = '-q -e'
  if( present(opt) ) opt_ = opt

  info_ = 0
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  if( path == '' )then
    if( .not. allow_empty_ )then
      info_ = 1
      call eerr(str(msg_unexpected_condition())//&
              "\nPath is an empty string."//&
              "\n  id: "//str(id_), opt)
    endif

    call finalize()
    call echo(code%ret)
    return
  endif

  selectcase( action )
  case( action_read )
    if( access(path, ' ') == 0 )then
      if( access(path, 'r') /= 0 )then
        info_ = 1
        call eerr(str(msg_unexpected_condition())//&
                "\nYou don't have read permission."//&
                "\n  path: "//str(path)//&
                "\n  id: "//str(id_), opt)
        if( present(info) )then
          info = info_
          call echo(code%ret)
        endif
      endif
    else
      info_ = 1
      call eerr(str(msg_unexpected_condition())//&
              "\nFile does not exist."//&
              "\n  path: "//str(path)//&
              "\n  id: "//str(id_), opt)
    endif
  case( action_write )
    if( access(path, ' ') == 0 )then
      if( access(path, 'w') /= 0 )then
        info_ = 1
        call eerr(str(msg_unexpected_condition())//&
                "\nYou don't have write permission."//&
                "\n  path: "//str(path)//&
                "\n  id: "//str(id_), opt)
      endif
    else
      un = unit_number()
      open(un, file=path, status='new', iostat=info_)
      if( info_ /= 0 )then
        call eerr(str(msg_unexpected_condition())//&
                "\nFailed to make a new file. "//&
                "Check if directory exists and you have write permission."//&
                "\n  path: "//str(path)//&
                "\n  id: "//str(id_), opt)
      endif
      close(un, status='delete')
    endif
  case( action_readwrite )
    if( access(path, ' ') == 0 )then
      if( access(path, 'r') /= 0 .or. access(path, 'w') /= 0 )then
        info_ = 1
        call eerr(str(msg_unexpected_condition())//&
                "\nYou don't have read write permission."//&
                "\n  path: "//str(path)//&
                "\n  id: "//str(id_), opt)
      endif
    else
      info_ = 1
      call eerr(str(msg_unexpected_condition())//&
              "\nFile does not exist."//&
              "\n  path: "//str(path)//&
              "\n  id: "//str(id_), opt)
    endif
  case default
    info_ = 1
    call eerr(str(msg_invalid_value())//&
            "\n  action: "//str(action)//&
            "\n  path: "//str(path)//&
            "\n  id: "//str(id_), opt)
  endselect

  call finalize()
  !------------------------------------------------------------
  call echo(code%ret)
!--------------------------------------------------------------
contains
!--------------------------------------------------------------
subroutine finalize()
  implicit none

  deallocate(id_)

  if( present(info) ) info = info_
end subroutine finalize
!--------------------------------------------------------------
end subroutine check_permission_path
!===============================================================
!
!===============================================================
subroutine check_file_size(f, allow_empty, allow_not_multiple)
  implicit none
  type(file_), intent(in) :: f
  logical, intent(in), optional :: allow_empty
  logical, intent(in), optional :: allow_not_multiple

  logical :: allow_empty_
  logical :: allow_not_multiple_

  integer :: d
  integer(8) :: fs
  integer(8) :: recl

  integer :: access

  call echo(code%ent, 'check_file_size', '-p -x2')
  !-------------------------------------------------------------
  ! Options
  !-------------------------------------------------------------
  allow_empty_ = .false.
  if( present(allow_empty) ) allow_empty_ = allow_empty

  allow_not_multiple_ = .false.
  if( present(allow_not_multiple) ) allow_not_multiple_ = allow_not_multiple
  !-------------------------------------------------------------
  ! Exceptions
  !-------------------------------------------------------------
  if( f%path == '' )then
    if( allow_empty_ )then
      call echo(code%ret)
      return
    else
      call eerr(str(msg_unexpected_condition())//&
              '\n  Path is empty.'//&
              '\n  id: '//str(f%id))
    endif
  endif

  if( access(f%path, ' ') /= 0 )then
    if( allow_empty_ )then
      call echo(code%ret)
      return
    else
      call eerr(str(msg_unexpected_condition())//&
              '\n  File does not exist.'//&
              '\n  path: '//str(f%path))
    endif
  endif

  selectcase( f%dtype )
  case( dtype_int1, &
        dtype_int2, &
        dtype_int4, &
        dtype_int8, &
        dtype_real, &
        dtype_dble )
    continue
  case( dtype_undef )
    call eerr(str(msg_unexpected_condition())//&
            '\n  f%dtype == dtype_undef')
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  f%dtype: '//str(f%dtype))
  endselect

  selectcase( f%rec )
  case( 1: )
    continue
  case( rec_undef )
    continue
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  f%rec: '//str(f%rec))
  endselect

  if( f%length <= 0_8 )then
    call eerr(str(msg_invalid_value())//&
            '\n  f%length: '//str(f%length))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fs = filesize(f%path)

  recl = byte_of_dtype(f%dtype) * f%length

  if( f%rec /= rec_undef )then
    if( recl*f%rec > fs )then
      d = dgt(recl*f%rec)

      call eerr(str(msg_unexpected_condition())//&
              '\n  recl * rec > fs'//&
              '\n  Expected file size exceeds actual file size.'//&
              '\n  id      : '//str(f%id)//&
              '\n  recl    : '//str(recl,d)//' (recl = byte * length)'//&
              '\n    dtype : '//str(f%dtype)//' (byte: '//str(byte_of_dtype(f%dtype))//')'//&
              '\n    length: '//str(f%length,d)//&
              '\n  rec     : '//str(f%rec,d)//&
              '\n  fs      : '//str(fs,d))
    endif
  endif

  if( .not. allow_not_multiple_ )then
    if( mod(fs, recl) /= 0_8 )then
      d = dgt((/fs,recl/),dgt_opt_max)

      call eerr(str(msg_unexpected_condition())//&
              '\n  mod(fs, recl) /= 0'//&
              '\n  File size (fs) is not a multiple of record length (recl).'//&
              '\n  id      : '//str(f%id)//&
              '\n  fs      : '//str(fs,d)//&
              '\n  recl    : '//str(recl,d)//' (recl = byte * length)'//&
              '\n    dtype : '//str(f%dtype)//' (byte: '//str(byte_of_dtype(f%dtype))//')'//&
              '\n    length: '//str(f%length,d))
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine check_file_size
!===============================================================
!
!===============================================================
subroutine try_make_empty_file_file(f)
  implicit none
  type(file_), intent(in) :: f

  integer :: un
  integer :: ios

  integer :: access

  call echo(code%bgn, 'try_make_empty_file', '-p -x2')
  !-------------------------------------------------------------
  if( f%path == '' )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  f%path is an empty string.'//&
            '\n  id: '//str(f%id))
  endif

  if( access(f%path,' ') == 0 )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  File exists.'//&
            '\n  id  : '//str(f%id)//&
            '\n  path: '//str(f%path))
  endif

  un = unit_number()
  open(un, file=f%path, status='new', iostat=ios)
  if( ios /= 0 )then
    call eerr('Failed to make an empty file.'//&
            '\n  id  : '//str(f%id)//&
            '\n  path: '//str(f%path))
  endif

  close(un, status='delete')
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine try_make_empty_file_file
!===============================================================
!
!===============================================================
subroutine try_make_empty_file_dir(dir)
  implicit none
  character(*), intent(in) :: dir

  character(len_trim(dir)+32) :: path_file
  integer :: i
  integer, parameter :: imax = 1000000
  integer :: ios

  integer :: access

  call echo(code%bgn, 'try_make_empty_file_dir', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  i = 0
  ios = 1
  do while( i < imax .and. ios /= 0 )
    i = i + 1

    path_file = joined(dir,'spring.empty.'//str(i))
    if( access(path_file,' ') == 0 ) cycle

    call make_empty_file(path_file, ios, remove=.true.)
    if( ios /= 0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  Failed to make an empty file.'//&
              '\n  dir: '//str(dir)//&
              '\nCheck if this directory exists and you have write permission.')
    endif

    if( i == imax )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  $i exceeded upper limit.')
    endif
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine try_make_empty_file_dir
!===============================================================
!
!===============================================================
subroutine set_opt_mkdir(output, hut)
  implicit none
  logical     , intent(in), optional :: output
  character(*), intent(in), optional :: hut

  call echo(code%bgn, 'set_opt_mkdir', '-p -x2')
  !-------------------------------------------------------------
  if( present(output) )then
    opt_mkdir__output = output
  endif

  if( present(hut) )then
    opt_mkdir__clen_hut = len(hut)
    opt_mkdir__hut = hut
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_opt_mkdir
!===============================================================
!
!===============================================================
subroutine init_opt_mkdir(key)
  implicit none
  character(*), intent(in) :: key

  call echo(code%bgn, 'set_opt_mkdir', '-p -x2')
  !-------------------------------------------------------------
  selectcase( key )
  case( 'output' )
    opt_mkdir__output = opt_default_mkdir__output
  case( 'hut' )
    opt_mkdir__hut = opt_default_mkdir__hut
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  key: '//str(key))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_opt_mkdir
!===============================================================
!
!===============================================================
subroutine mkdir(dir, output, hut)
  implicit none
  character(*), intent(in)           :: dir
  logical     , intent(in), optional :: output
  character(*), intent(in), optional :: hut

  type hist_
    character(clen_path) :: dir
    integer :: order
  end type

  logical :: output_
  character(:), allocatable :: hut_
  character(len_trim(dir)+9) :: command
  integer :: cl
  integer, parameter :: nmax = 30
  integer, save      :: n = 0
  integer            :: i
  integer            :: i_oldest
  type(hist_), allocatable, save :: hist(:)
  logical, save :: is_first = .true.

  !external :: system

  call echo(code%bgn, 'mkdir', '-p -x2')
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  selectcase( adjustl(dir) )
  case( '', './' )
    call echo(code%ret)
    return
  endselect
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  if( is_first )then
    is_first = .false.
    allocate(hist(nmax))
    hist(:)%dir = ''
    hist(:)%order = 0
  endif
  !------------------------------------------------------------
  ! Options
  !------------------------------------------------------------
  if( present(output) )then
    output_ = output
  else
    output_ = opt_mkdir__output
  endif

  if( present(hut) )then
    cl = len(hut)
    allocate(character(cl) :: hut_)
    hut_ = hut
  else
    allocate(character(opt_mkdir__clen_hut) :: hut_)

    if( opt_mkdir__clen_hut > 0 )then
      hut_ = opt_mkdir__hut(1:opt_mkdir__clen_hut)
    endif
  endif
  !------------------------------------------------------------
  ! Check if already made
  !------------------------------------------------------------
  do i = 1, n
    if( hist(i)%dir == dir )then
      call echo(code%ret)
      return
    endif
  enddo
  !------------------------------------------------------------
  ! Update history
  !------------------------------------------------------------
  selectcase( n )
  case( 0:nmax-1 )
    n = n + 1
    hist(n)%dir = dir
    hist(n)%order = n
  case( nmax )
    i_oldest = 0
    do i = 1, n
      hist(i)%order = hist(i)%order - 1
      if( hist(i)%order == 0 )then
        i_oldest = i
      endif
    enddo

    if( i_oldest == 0 )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  i_oldest == 0')
    endif

    hist(i_oldest)%dir = dir
    hist(i_oldest)%order = n
  case default
    call eerr(str(msg_unexpected_condition())//&
            '\n  n == '//str(n))
  endselect
  !------------------------------------------------------------
  ! Make
  !------------------------------------------------------------
  command = 'mkdir -p '//str(dir)
  if( output_ )then
    call edbg(hut_//str(command))
  endif

  call system(str(command))

  deallocate(hut_)
  !------------------------------------------------------------
  call echo(code%ret)
end subroutine mkdir
!===============================================================
!
!===============================================================
subroutine remove(path, dir, output)
  implicit none
  character(*), intent(in)           :: path
  logical     , intent(in), optional :: dir
  logical     , intent(in), optional :: output
  logical :: dir_
  logical :: output_

  !intrinsic system
  !------------------------------------------------------------
  ! Options
  !------------------------------------------------------------
  dir_  = .false.
  output_ = .false.
  if( present(dir)    ) dir_    = dir
  if( present(output) ) output_ = output
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  if( path /= '' )then
    if( dir_ )then
      if( output_ )then
        if( path(len_trim(path):len_trim(path)) == '/' )then
          call edbg('Remove '//str(path))
        else
          call edbg('Remove '//str(path)//'/')
        endif
      endif
      call system('rm -rf '//str(path))
    else
      if( output_ )then
        call edbg('Remove '//str(path))
      endif
      call system('rm -f '//str(path))
    endif
  endif
end subroutine remove
!===============================================================
!
!===============================================================
subroutine make_empty_file(path, ios, remove, opt)
  implicit none
  character(*), intent(in) :: path
  integer     , intent(out), optional :: ios
  logical     , intent(in) , optional :: remove
  character(*), intent(in) , optional :: opt
  integer :: ios_
  logical :: remove_
  character(:), allocatable :: opt_
  integer :: cl
  integer :: un

  call echo(code%bgn, 'make_empty_file', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( present(opt) )then
    cl = len_trim(opt)
    allocate(character(cl) :: opt_)
    opt_ = trim(opt)
  else
    allocate(character(1) :: opt_)
    opt_ = ''
  endif

  remove_ = .false.
  if( present(remove) ) remove_ = remove
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( path == '' )then
    call eerr(str(msg_unexpected_condition())//&
            '\n  $path is an empty string.')
  endif

  un = unit_number()
  open(un, file=path, status='replace', iostat=ios_)

  if( ios_ == 0 )then
    if( present(ios) ) ios = ios_

    if( remove_ )then
      close(un, status='delete')
    else
      close(un)
    endif
  else
    call eerr('Failed to make an empty file.'//&
            '\n  path: '//str(path), opt_)

    if( present(ios) ) ios = ios_
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_empty_file
!===============================================================
!
!===============================================================
end module lib_io_file
