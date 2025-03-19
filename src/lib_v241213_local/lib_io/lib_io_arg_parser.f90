module lib_io_arg_parser
  use lib_const
  use lib_log
  use lib_array
  use lib_io_system
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: addarg
  public :: parsearg
  public :: showarg
  public :: getarg
  public :: checkparsed
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface addarg
    module procedure addarg__flag
    module procedure addarg__char
    module procedure addarg__int4
    module procedure addarg__int8
    module procedure addarg__real
    module procedure addarg__dble
  end interface

  interface getarg
    module procedure getarg__flag
    module procedure getarg__char
    module procedure getarg__int4
    module procedure getarg__int8
    module procedure getarg__real
    module procedure getarg__dble
  end interface
  !-------------------------------------------------------------
  ! Module Variables
  !-------------------------------------------------------------
  integer, parameter :: CLEN_DTYPE_DESC = 16
  character(CLEN_DTYPE_DESC), parameter :: DTYPE_DESC_FLAG = 'flag'
  character(CLEN_DTYPE_DESC), parameter :: DTYPE_DESC_STR  = 'string'
  character(CLEN_DTYPE_DESC), parameter :: DTYPE_DESC_INT4 = '4-byte integer'
  character(CLEN_DTYPE_DESC), parameter :: DTYPE_DESC_INT8 = '8-byte integer'
  character(CLEN_DTYPE_DESC), parameter :: DTYPE_DESC_REAL = '4-byte float'
  character(CLEN_DTYPE_DESC), parameter :: DTYPE_DESC_DBLE = '8-byte float'

  type arg_flag_
    character(:), allocatable :: key_short
    character(:), allocatable :: key_long
    logical :: val_default
    logical :: val
    logical :: required
    character(:), allocatable :: description
    integer :: ntimes    ! Num. of inputs of argument
    integer :: nget = 0  ! Num. of times that getarg was called. For debugging
  end type

  type arg_char_
    character(:), allocatable :: key_short
    character(:), allocatable :: key_long
    character(:), allocatable :: val_default
    character(:), allocatable :: val
    logical :: required
    character(:), allocatable :: description
    integer :: ntimes
    integer :: nget = 0
  end type

  type arg_int4_
    character(:), allocatable :: key_short
    character(:), allocatable :: key_long
    integer(4) :: val_default
    integer(4) :: val
    logical :: required
    character(:), allocatable :: description
    integer :: ntimes
    integer :: nget = 0
  end type

  type arg_int8_
    character(:), allocatable :: key_short
    character(:), allocatable :: key_long
    integer(8) :: val_default
    integer(8) :: val
    logical :: required
    character(:), allocatable :: description
    integer :: ntimes
    integer :: nget = 0
  end type

  type arg_real_
    character(:), allocatable :: key_short
    character(:), allocatable :: key_long
    real(4) :: val_default
    real(4) :: val
    logical :: required
    character(:), allocatable :: description
    integer :: ntimes
    integer :: nget = 0
  end type

  type arg_dble_
    character(:), allocatable :: key_short
    character(:), allocatable :: key_long
    real(8) :: val_default
    real(8) :: val
    logical :: required
    character(:), allocatable :: description
    integer :: ntimes
    integer :: nget = 0
  end type

  type arglst_flag_
    integer :: narg
    type(arg_flag_), pointer :: arg(:)
    character(4) :: typename = 'flag'
    character(CLEN_DTYPE_DESC) :: dtype_desc = DTYPE_DESC_FLAG
  end type

  type arglst_char_
    integer :: narg
    type(arg_char_), pointer :: arg(:)
    character(4) :: typename = 'char'
    character(CLEN_DTYPE_DESC) :: dtype_desc = DTYPE_DESC_STR
  end type

  type arglst_int4_
    integer :: narg
    type(arg_int4_), pointer :: arg(:)
    character(4) :: typename = 'int4'
    character(CLEN_DTYPE_DESC) :: dtype_desc = DTYPE_DESC_INT4
  end type

  type arglst_int8_
    integer :: narg
    type(arg_int8_), pointer :: arg(:)
    character(4) :: typename = 'int8'
    character(CLEN_DTYPE_DESC) :: dtype_desc = DTYPE_DESC_INT8
  end type

  type arglst_real_
    integer :: narg
    type(arg_real_), pointer :: arg(:)
    character(4) :: typename = 'real'
    character(CLEN_DTYPE_DESC) :: dtype_desc = DTYPE_DESC_REAL
  end type

  type arglst_dble_
    integer :: narg
    type(arg_dble_), pointer :: arg(:)
    character(4) :: typename = 'dble'
    character(CLEN_DTYPE_DESC) :: dtype_desc = DTYPE_DESC_DBLE
  end type

  type argdct_
    integer :: narg
    integer, pointer :: typ(:)
    integer, pointer :: idx(:)
    type(arglst_flag_) :: arglst_flag
    type(arglst_char_) :: arglst_char
    type(arglst_int4_) :: arglst_int4
    type(arglst_int8_) :: arglst_int8
    type(arglst_real_) :: arglst_real
    type(arglst_dble_) :: arglst_dble
  end type

  type(argdct_), target :: argdct

  integer, parameter :: CLENMAX_KEY = 32
  integer, parameter :: NARG_INIT = 64

  integer, parameter :: ITYPE_FLAG = 1
  integer, parameter :: ITYPE_CHAR = 2
  integer, parameter :: ITYPE_INT1 = 3
  integer, parameter :: ITYPE_INT2 = 4
  integer, parameter :: ITYPE_INT4 = 5
  integer, parameter :: ITYPE_INT8 = 6
  integer, parameter :: ITYPE_REAL = 7
  integer, parameter :: ITYPE_DBLE = 8

  character(2), parameter :: KEY_HELP_SHORT = '-h'
  character(6), parameter :: KEY_HELP_LONG  = '--help'
  character(64), parameter :: DESCRIPTION_HELP = 'Show this message and stop program'
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine addarg__flag(key_short, key_long, required, default_value, description)
  implicit none
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  logical     , intent(in) :: required
  logical     , intent(in) :: default_value
  character(*), intent(in) :: description

  type(arg_flag_), pointer :: arg
  type(arglst_flag_), pointer :: lst
  type(arg_flag_), allocatable :: argtmp(:)

  lst => argdct%arglst_flag

  if( lst%narg == 0 )then
    allocate(lst%arg(NARG_INIT))
  elseif( lst%narg == size(lst%arg) )then
    allocate(argtmp(lst%narg))
    argtmp = lst%arg
    deallocate(lst%arg)
    allocate(lst%arg(lst%narg*2))
    lst%arg(:lst%narg) = argtmp
    deallocate(argtmp)
  endif
  lst%narg = lst%narg + 1

  call update_argdct(ITYPE_FLAG, lst%narg)

  arg => lst%arg(lst%narg)
  arg%key_short = trim(key_short)
  arg%key_long = trim(key_long)
  arg%required = required
  arg%val_default = default_value
  arg%val = default_value
  arg%description = description_default(description)
  arg%ntimes = 0

  nullify(arg)
  nullify(lst)
end subroutine addarg__flag
!===============================================================
!
!===============================================================
subroutine addarg__char(key_short, key_long, required, default_value, description)
  implicit none
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  logical     , intent(in) :: required
  character(*), intent(in) :: default_value
  character(*), intent(in) :: description

  type(arg_char_), pointer :: arg
  type(arglst_char_), pointer :: lst
  type(arg_char_), allocatable :: argtmp(:)

  lst => argdct%arglst_char

  if( lst%narg == 0 )then
    allocate(lst%arg(NARG_INIT))
  elseif( lst%narg == size(lst%arg) )then
    allocate(argtmp(lst%narg))
    argtmp = lst%arg
    deallocate(lst%arg)
    allocate(lst%arg(lst%narg*2))
    lst%arg(:lst%narg) = argtmp
    deallocate(argtmp)
  endif
  lst%narg = lst%narg + 1

  call update_argdct(ITYPE_CHAR, lst%narg)

  arg => argdct%arglst_char%arg(argdct%arglst_char%narg)
  arg%key_short = trim(key_short)
  arg%key_long = trim(key_long)
  arg%required = required
  arg%val_default = trim(default_value)
  arg%val = trim(default_value)
  arg%description = description_default(description)
  arg%ntimes = 0

  nullify(arg)
  nullify(lst)
end subroutine addarg__char
!===============================================================
!
!===============================================================
subroutine addarg__int4(key_short, key_long, required, default_value, description)
  implicit none
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  logical     , intent(in) :: required
  integer(4)  , intent(in) :: default_value
  character(*), intent(in) :: description

  type(arg_int4_), pointer :: arg
  type(arglst_int4_), pointer :: lst
  type(arg_int4_), allocatable :: argtmp(:)

  lst => argdct%arglst_int4

  if( lst%narg == 0 )then
    allocate(lst%arg(NARG_INIT))
  elseif( lst%narg == size(lst%arg) )then
    allocate(argtmp(lst%narg))
    argtmp = lst%arg
    deallocate(lst%arg)
    allocate(lst%arg(lst%narg*2))
    lst%arg(:lst%narg) = argtmp
    deallocate(argtmp)
  endif
  lst%narg = lst%narg + 1

  call update_argdct(ITYPE_INT4, lst%narg)

  arg => argdct%arglst_int4%arg(argdct%arglst_int4%narg)
  arg%key_short = trim(key_short)
  arg%key_long = trim(key_long)
  arg%required = required
  arg%val_default = default_value
  arg%val = default_value
  arg%description = description_default(description)
  arg%ntimes = 0

  nullify(arg)
  nullify(lst)
end subroutine addarg__int4
!===============================================================
!
!===============================================================
subroutine addarg__int8(key_short, key_long, required, default_value, description)
  implicit none
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  logical     , intent(in) :: required
  integer(8)  , intent(in) :: default_value
  character(*), intent(in) :: description

  type(arg_int8_), pointer :: arg
  type(arglst_int8_), pointer :: lst
  type(arg_int8_), allocatable :: argtmp(:)

  lst => argdct%arglst_int8

  if( lst%narg == 0 )then
    allocate(lst%arg(NARG_INIT))
  elseif( lst%narg == size(lst%arg) )then
    allocate(argtmp(lst%narg))
    argtmp = lst%arg
    deallocate(lst%arg)
    allocate(lst%arg(lst%narg*2))
    lst%arg(:lst%narg) = argtmp
    deallocate(argtmp)
  endif
  lst%narg = lst%narg + 1

  call update_argdct(ITYPE_INT8, lst%narg)

  arg => argdct%arglst_int8%arg(argdct%arglst_int8%narg)
  arg%key_short = trim(key_short)
  arg%key_long = trim(key_long)
  arg%required = required
  arg%val_default = default_value
  arg%val = default_value
  arg%description = description_default(description)
  arg%ntimes = 0

  nullify(arg)
  nullify(lst)
end subroutine addarg__int8
!===============================================================
!
!===============================================================
subroutine addarg__real(key_short, key_long, required, default_value, description)
  implicit none
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  logical     , intent(in) :: required
  real(4)     , intent(in) :: default_value
  character(*), intent(in) :: description

  type(arg_real_), pointer :: arg
  type(arglst_real_), pointer :: lst
  type(arg_real_), allocatable :: argtmp(:)

  lst => argdct%arglst_real

  if( lst%narg == 0 )then
    allocate(lst%arg(NARG_INIT))
  elseif( lst%narg == size(lst%arg) )then
    allocate(argtmp(lst%narg))
    argtmp = lst%arg
    deallocate(lst%arg)
    allocate(lst%arg(lst%narg*2))
    lst%arg(:lst%narg) = argtmp
    deallocate(argtmp)
  endif
  lst%narg = lst%narg + 1

  call update_argdct(ITYPE_REAL, lst%narg)

  arg => argdct%arglst_real%arg(argdct%arglst_real%narg)
  arg%key_short = trim(key_short)
  arg%key_long = trim(key_long)
  arg%required = required
  arg%val_default = default_value
  arg%val = default_value
  arg%description = description_default(description)
  arg%ntimes = 0

  nullify(arg)
  nullify(lst)
end subroutine addarg__real
!===============================================================
!
!===============================================================
subroutine addarg__dble(key_short, key_long, required, default_value, description)
  implicit none
  character(*), intent(in) :: key_short
  character(*), intent(in) :: key_long
  logical     , intent(in) :: required
  real(8)     , intent(in) :: default_value
  character(*), intent(in) :: description

  type(arg_dble_), pointer :: arg
  type(arglst_dble_), pointer :: lst
  type(arg_dble_), allocatable :: argtmp(:)

  lst => argdct%arglst_dble

  if( lst%narg == 0 )then
    allocate(lst%arg(NARG_INIT))
  elseif( lst%narg == size(lst%arg) )then
    allocate(argtmp(lst%narg))
    argtmp = lst%arg
    deallocate(lst%arg)
    allocate(lst%arg(lst%narg*2))
    lst%arg(:lst%narg) = argtmp
    deallocate(argtmp)
  endif
  lst%narg = lst%narg + 1

  call update_argdct(ITYPE_DBLE, lst%narg)

  arg => argdct%arglst_dble%arg(argdct%arglst_dble%narg)
  arg%key_short = trim(key_short)
  arg%key_long = trim(key_long)
  arg%required = required
  arg%val_default = default_value
  arg%val = default_value
  arg%description = description_default(description)
  arg%ntimes = 0

  nullify(arg)
  nullify(lst)
end subroutine addarg__dble
!===============================================================
!
!===============================================================
subroutine update_argdct(itype, narg)
  implicit none
  integer, intent(in) :: itype
  integer, intent(in) :: narg

  if( argdct%narg == 0 )then
    allocate(argdct%typ(NARG_INIT))
    allocate(argdct%idx(NARG_INIT))
  elseif( argdct%narg == size(argdct%typ) )then
    call realloc(argdct%typ, argdct%narg*2, clear=.false.)
    call realloc(argdct%idx, argdct%narg*2, clear=.false.)
  endif
  argdct%narg = argdct%narg + 1
  argdct%typ(argdct%narg) = itype
  argdct%idx(argdct%narg) = narg
end subroutine update_argdct
!===============================================================
!
!===============================================================
function description_default(description) result(res)
  implicit none
  character(*), intent(in) :: description
  character(:), allocatable :: res

  if( description == '' )then
    res = '(TBD)'
  else
    res = trim(description)
  endif
end function description_default
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
subroutine parsearg()
  implicit none
  type(arg_flag_), pointer :: argflag
  type(arg_char_), pointer :: argchar
  type(arg_int4_), pointer :: argint4
  type(arg_int8_), pointer :: argint8
  type(arg_real_), pointer :: argreal
  type(arg_dble_), pointer :: argdble
  character(CLENMAX_KEY) :: key
  character(:), allocatable :: val
  integer :: narg, iarg
  integer :: iargflag, iargchar, &
             iargint4, iargint8, &
             iargreal, iargdble

  call echo(code%bgn, 'parsearg', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  narg = command_argument_count()

  iarg = 0
  loop_outer:&
  do while( iarg < narg )
    iarg = iarg + 1
    call get_command_argument(iarg, key)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( key == KEY_HELP_SHORT .or. key == KEY_HELP_LONG )then
      call showarg(.true., .false.)
      call echo(code%ret)
      return
    endif
    !-----------------------------------------------------------
    ! Look in FLAG list
    !-----------------------------------------------------------
    do iargflag = 1, argdct%arglst_flag%narg
      argflag => argdct%arglst_flag%arg(iargflag)
      if( key == argflag%key_short .or. key == argflag%key_long )then
        if( argflag%ntimes == 0 )then
          argflag%val = .not. argflag%val
        endif
        argflag%ntimes = argflag%ntimes + 1
        cycle loop_outer
      endif
    enddo
    !-----------------------------------------------------------
    ! Look in CHAR list
    !-----------------------------------------------------------
    do iargchar = 1, argdct%arglst_char%narg
      argchar => argdct%arglst_char%arg(iargchar)
      if( key == argchar%key_short .or. key == argchar%key_long )then
        call assert_nextarg_exist(narg, iarg, key)
        iarg = iarg + 1
        call get_command_argument_extend(iarg, val)
        argchar%val = trim(val)
        argchar%ntimes = argchar%ntimes + 1
        deallocate(val)
        cycle loop_outer
      endif
    enddo
    !-----------------------------------------------------------
    ! Look in INT4 list
    !-----------------------------------------------------------
    do iargint4 = 1, argdct%arglst_int4%narg
      argint4 => argdct%arglst_int4%arg(iargint4)
      if( key == argint4%key_short .or. key == argint4%key_long )then
        call assert_nextarg_exist(narg, iarg, key)
        iarg = iarg + 1
        call get_command_argument_extend(iarg, val)
        read(val,*) argint4%val
        argint4%ntimes = argint4%ntimes + 1
        deallocate(val)
        cycle loop_outer
      endif
    enddo
    !-----------------------------------------------------------
    ! Look in INT8 list
    !-----------------------------------------------------------
    do iargint8 = 1, argdct%arglst_int8%narg
      argint8 => argdct%arglst_int8%arg(iargint8)
      if( key == argint8%key_short .or. key == argint8%key_long )then
        call assert_nextarg_exist(narg, iarg, key)
        iarg = iarg + 1
        call get_command_argument_extend(iarg, val)
        read(val,*) argint8%val
        argint8%ntimes = argint8%ntimes + 1
        deallocate(val)
        cycle loop_outer
      endif
    enddo
    !-----------------------------------------------------------
    ! Look in REAL list
    !-----------------------------------------------------------
    do iargreal = 1, argdct%arglst_real%narg
      argreal => argdct%arglst_real%arg(iargreal)
      if( key == argreal%key_short .or. key == argreal%key_long )then
        call assert_nextarg_exist(narg, iarg, key)
        iarg = iarg + 1
        call get_command_argument_extend(iarg, val)
        read(val,*) argreal%val
        argreal%ntimes = argreal%ntimes + 1
        deallocate(val)
        cycle loop_outer
      endif
    enddo
    !-----------------------------------------------------------
    ! Look in DBLE list
    !-----------------------------------------------------------
    do iargdble = 1, argdct%arglst_dble%narg
      argdble => argdct%arglst_dble%arg(iargdble)
      if( key == argdble%key_short .or. key == argdble%key_long )then
        call assert_nextarg_exist(narg, iarg, key)
        iarg = iarg + 1
        call get_command_argument_extend(iarg, val)
        read(val,*) argdble%val
        argdble%ntimes = argdble%ntimes + 1
        deallocate(val)
        cycle loop_outer
      endif
    enddo
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call eerr('Invalid argument: '//str(key))
    !-----------------------------------------------------------
  enddo& ! while( iarg < narg )/
  loop_outer
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine parsearg
!===============================================================
!
!===============================================================
subroutine get_command_argument_extend(i, arg)
  implicit none
  integer, intent(in) :: i
  character(:), allocatable, intent(out) :: arg

  integer :: cl
  character(:), allocatable :: tmp

  cl = 128
  do
    allocate(character(cl) :: tmp)
    call get_command_argument(i, tmp)
    if( tmp(cl:cl) == ' ' ) exit
    cl = cl * 2
    deallocate(tmp)
  enddo

  cl = len_trim(tmp)
  allocate(character(cl) :: arg)
  arg = trim(tmp)
end subroutine get_command_argument_extend
!===============================================================
!
!===============================================================
subroutine checkparsed()
  implicit none
  type(arg_flag_), pointer :: argflag
  type(arg_char_), pointer :: argchar
  type(arg_int4_), pointer :: argint4
  type(arg_int8_), pointer :: argint8
  type(arg_real_), pointer :: argreal
  type(arg_dble_), pointer :: argdble
  integer :: iargflag, iargchar, &
             iargint4, iargint8, &
             iargreal, iargdble
  character(16) :: opt

  call echo(code%bgn, 'checkparsed', '-p')
  !-------------------------------------------------------------
  opt = '-q -b'

  do iargflag = 1, argdct%arglst_flag%narg
    argflag => argdct%arglst_flag%arg(iargflag)
    if( argflag%nget == 0 )then
      call eerr('Argument "'//str(argflag%key_short)//'", "'//&
                str(argflag%key_long)//'" is not parsed', opt)
      opt = '-q -p -b'
    endif
  enddo

  do iargchar = 1, argdct%arglst_char%narg
    argchar => argdct%arglst_char%arg(iargchar)
    if( argchar%nget == 0 )then
      call eerr('Argument "'//str(argchar%key_short)//'", "'//&
                str(argchar%key_long)//'" is not parsed', opt)
      opt = '-q -p -b'
    endif
  enddo

  do iargint4 = 1, argdct%arglst_int4%narg
    argint4 => argdct%arglst_int4%arg(iargint4)
    if( argint4%nget == 0 )then
      call eerr('Argument "'//str(argint4%key_short)//'", "'//&
                str(argint4%key_long)//'" is not parsed', opt)
      opt = '-q -p -b'
    endif
  enddo

  do iargint8 = 1, argdct%arglst_int8%narg
    argint8 => argdct%arglst_int8%arg(iargint8)
    if( argint8%nget == 0 )then
      call eerr('Argument "'//str(argint8%key_short)//'", "'//&
                str(argint8%key_long)//'" is not parsed', opt)
      opt = '-q -p -b'
    endif
  enddo

  do iargreal = 1, argdct%arglst_real%narg
    argreal => argdct%arglst_real%arg(iargreal)
    if( argreal%nget == 0 )then
      call eerr('Argument "'//str(argreal%key_short)//'", "'//&
                str(argreal%key_long)//'" is not parsed', opt)
      opt = '-q -p -b'
    endif
  enddo

  do iargdble = 1, argdct%arglst_dble%narg
    argdble => argdct%arglst_dble%arg(iargdble)
    if( argdble%nget == 0 )then
      call eerr('Argument "'//str(argdble%key_short)//'", "'//&
                str(argdble%key_long)//'" is not parsed', opt)
      opt = '-q -p -b'
    endif
  enddo

  if( opt /= '-q -b' )then
    call eerr('', '-p')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine checkparsed
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
subroutine showarg(help, debug)
  implicit none
  logical, intent(in) :: help
  logical, intent(in) :: debug
  type(arg_flag_), pointer :: argflag
  type(arg_char_), pointer :: argchar
  type(arg_int4_), pointer :: argint4
  type(arg_int8_), pointer :: argint8
  type(arg_real_), pointer :: argreal
  type(arg_dble_), pointer :: argdble
  integer :: iarg
  integer :: clenmax_key_short, clenmax_key_long
  integer :: dgt_narg, dgt_size
  integer :: ncol_keys
  integer :: ncol_terminal
  integer :: stat

  call get_terminal_width(ncol_terminal, stat)

  if( debug )then
    call edbg('Terminal width: '//str(ncol_terminal))
    dgt_narg = max(4,dgt(argdct%narg))
    dgt_size = max(4,dgt(size(argdct%typ)))

    call edbg('     '//str('narg',dgt_narg)//' '//str('size',dgt_size))
    call edbg('all  '//str(argdct%narg,dgt_narg)//' '//str(size(argdct%typ),dgt_size))
    call edbg('flag '//str(argdct%arglst_flag%narg,dgt_narg)//&
              ' '//str(size(argdct%arglst_flag%arg),dgt_size))
    call edbg('char '//str(argdct%arglst_char%narg,dgt_narg)//&
              ' '//str(size(argdct%arglst_char%arg),dgt_size))
    call edbg('int4 '//str(argdct%arglst_int4%narg,dgt_narg)//&
              ' '//str(size(argdct%arglst_int4%arg),dgt_size))
    call edbg('int8 '//str(argdct%arglst_int8%narg,dgt_narg)//&
              ' '//str(size(argdct%arglst_int8%arg),dgt_size))
    call edbg('real '//str(argdct%arglst_real%narg,dgt_narg)//&
              ' '//str(size(argdct%arglst_real%arg),dgt_size))
    call edbg('dble '//str(argdct%arglst_dble%narg,dgt_narg)//&
              ' '//str(size(argdct%arglst_dble%arg),dgt_size))
  endif

  clenmax_key_short = len_trim(KEY_HELP_SHORT)
  clenmax_key_long  = len_trim(KEY_HELP_LONG)
  do iarg = 1, argdct%narg
    selectcase( argdct%typ(iarg) )
    case( ITYPE_FLAG )
      call check_idx(argdct%idx(iarg), iarg, argdct%arglst_flag%narg, argdct%arglst_flag%typename)
      argflag => argdct%arglst_flag%arg(argdct%idx(iarg))
      clenmax_key_short = max(len_trim(argflag%key_short), clenmax_key_short)
      clenmax_key_long  = max(len_trim(argflag%key_long ), clenmax_key_long )
    case( ITYPE_CHAR )
      call check_idx(argdct%idx(iarg), iarg, argdct%arglst_char%narg, argdct%arglst_char%typename)
      argchar => argdct%arglst_char%arg(argdct%idx(iarg))
      clenmax_key_short = max(len_trim(argchar%key_short), clenmax_key_short)
      clenmax_key_long  = max(len_trim(argchar%key_long ), clenmax_key_long )
    case( ITYPE_INT4 )
      call check_idx(argdct%idx(iarg), iarg, argdct%arglst_int4%narg, argdct%arglst_int4%typename)
      argint4 => argdct%arglst_int4%arg(argdct%idx(iarg))
      clenmax_key_short = max(len_trim(argint4%key_short), clenmax_key_short)
      clenmax_key_long  = max(len_trim(argint4%key_long ), clenmax_key_long )
    case( ITYPE_INT8 )
      call check_idx(argdct%idx(iarg), iarg, argdct%arglst_int8%narg, argdct%arglst_int8%typename)
      argint8 => argdct%arglst_int8%arg(argdct%idx(iarg))
      clenmax_key_short = max(len_trim(argint8%key_short), clenmax_key_short)
      clenmax_key_long  = max(len_trim(argint8%key_long ), clenmax_key_long )
    case( ITYPE_REAL )
      call check_idx(argdct%idx(iarg), iarg, argdct%arglst_real%narg, argdct%arglst_real%typename)
      argreal => argdct%arglst_real%arg(argdct%idx(iarg))
      clenmax_key_short = max(len_trim(argreal%key_short), clenmax_key_short)
      clenmax_key_long  = max(len_trim(argreal%key_long ), clenmax_key_long )
    case( ITYPE_DBLE )
      call check_idx(argdct%idx(iarg), iarg, argdct%arglst_dble%narg, argdct%arglst_dble%typename)
      argdble => argdct%arglst_dble%arg(argdct%idx(iarg))
      clenmax_key_short = max(len_trim(argdble%key_short), clenmax_key_short)
      clenmax_key_long  = max(len_trim(argdble%key_long ), clenmax_key_long )
    case default
      call eerr('Invalid value in $argdct%typ('//str(iarg)//'): '//str(argdct%typ(iarg)))
    endselect
  enddo
  ncol_keys = clenmax_key_short + clenmax_key_long + 3

  call edbg(get_full_description(KEY_HELP_SHORT, KEY_HELP_LONG, &
              DESCRIPTION_HELP, '', '', &
              '', 0), 'x1')
  do iarg = 1, argdct%narg
    selectcase( argdct%typ(iarg) )
    case( ITYPE_FLAG )
      argflag => argdct%arglst_flag%arg(argdct%idx(iarg))
      call edbg(get_full_description(argflag%key_short, argflag%key_long, &
                  argflag%description, str(argflag%val_default), str(argflag%val), &
                  argdct%arglst_flag%dtype_desc, argflag%ntimes), 'x1')
    case( ITYPE_CHAR )
      argchar => argdct%arglst_char%arg(argdct%idx(iarg))
      call edbg(get_full_description(argchar%key_short, argchar%key_long, &
                  argchar%description, str(argchar%val_default), str(argchar%val), &
                  argdct%arglst_char%dtype_desc, argchar%ntimes), 'x1')
    case( ITYPE_INT4 )
      argint4 => argdct%arglst_int4%arg(argdct%idx(iarg))
      call edbg(get_full_description(argint4%key_short, argint4%key_long, &
                  argint4%description, str(argint4%val_default), str(argint4%val), &
                  argdct%arglst_int4%dtype_desc, argint4%ntimes), 'x1')
    case( ITYPE_INT8 )
      argint8 => argdct%arglst_int8%arg(argdct%idx(iarg))
      call edbg(get_full_description(argint8%key_short, argint8%key_long, &
                  argint8%description, str(argint8%val_default), str(argint8%val), &
                  argdct%arglst_int8%dtype_desc, argint8%ntimes), 'x1')
    case( ITYPE_REAL )
      argreal => argdct%arglst_real%arg(argdct%idx(iarg))
      call edbg(get_full_description(argreal%key_short, argreal%key_long, &
                  argreal%description, str(argreal%val_default), str(argreal%val), &
                  argdct%arglst_real%dtype_desc, argreal%ntimes), 'x1')
    case( ITYPE_DBLE )
      argdble => argdct%arglst_dble%arg(argdct%idx(iarg))
      call edbg(get_full_description(argdble%key_short, argdble%key_long, &
                  argdble%description, str(argdble%val_default), str(argdble%val), &
                  argdct%arglst_dble%dtype_desc, argdble%ntimes), 'x1')
    endselect
  enddo

  if( help ) stop
contains

subroutine check_idx(idx, iarg, narg, nam)
  implicit none
  integer, intent(in) :: idx, iarg, narg
  character(*), intent(in) :: nam

  if( idx < 1 .or. idx > narg )then
    call eerr('!!! INTERNAL ERROR !!! Index is invalid.'//&
            '\nargdct%idx('//str(iarg)//'): '//str(idx)//&
            '\nargdct%arglst_'//str(nam)//'%narg: '//str(narg))
  endif
end subroutine check_idx

function get_full_description(&
    key_short, key_long, description, str_val_default, str_val, dtype_desc, ntimes) result(msg)
  implicit none
  character(*), intent(in) :: key_short, key_long
  character(*), intent(in) :: description
  character(*), intent(in) :: str_val_default
  character(*), intent(in) :: str_val
  character(*), intent(in) :: dtype_desc
  integer     , intent(in) :: ntimes
  character(:), allocatable :: msg

  integer :: ncol, ncol_this
  integer :: clen_left, clen_out
  integer :: i
  character(2) :: br
  integer :: clen_br
  character(:), allocatable :: c

  ! An allocatable character $c is used only for avoiding the annoying message
  ! "‘.__var_3_realloc_string’ may be used uninitialized in this function [-Wmaybe-uninitialized]",
  ! which appears even if it is actually initialized.
  allocate(character(1) :: c)

  msg = str(key_short,clenmax_key_short)//' '//str(key_long,clenmax_key_long)//':'

  if( ncol_terminal > 0 )then

    ! indent, keys of arguments
    ncol = ncol_terminal - 1 - ncol_keys
    clen_left = len_trim(description)
    clen_out = 0
    br = ''
    clen_br = 1
    do while( clen_left > 0 )
      if( ncol >= clen_left )then
        msg = trim(msg)//str(br,clen_br)//description(clen_out+1:)
        clen_left = 0
      else
        ncol_this = 0
        do i = clen_out+ncol, clen_out+1, -1
          if( description(i:i) == '' )then
            ncol_this = i - clen_out
            exit
          endif
        enddo
        if( ncol_this == 0 ) ncol_this = ncol

        msg = trim(msg)//str(br,clen_br)//description(clen_out+1:clen_out+ncol_this)

        clen_left = clen_left - ncol_this
        clen_out = clen_out + ncol_this
      endif

      br = '\n'
      clen_br = 2+ncol_keys
    enddo
  else
    c = trim(msg)
    msg = trim(c)//' '//description
  endif

  c = trim(msg)
  selectcase( dtype_desc )
  case( '', DTYPE_DESC_FLAG )
    msg = trim(c)
  case default
    msg = trim(c)//'\n'//str('',ncol_keys)//&
          '(data type: '//str(dtype_desc)//', default value: '//str_val_default//')'
  endselect

  if( debug )then
    c = trim(msg)
    msg = trim(c)//'\n'//str('',ncol_keys)//&
          'ntimes: '//str(ntimes)//' val: '//str_val
  endif
  deallocate(c)
end function get_full_description

end subroutine showarg
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
subroutine getarg__flag(key, val)
  implicit none
  character(*), intent(in)  :: key
  logical     , intent(out) :: val

  type(arg_flag_), pointer :: arg
  integer :: iarg

  do iarg = 1, argdct%narg
    if( argdct%typ(iarg) /= ITYPE_FLAG ) cycle

    arg => argdct%arglst_flag%arg(argdct%idx(iarg))
    if( arg%key_short == key .or. arg%key_long == key )then
      val = arg%val
      arg%nget = arg%nget + 1
      return
    endif
  enddo

  call eerr('!!! INTERNAL ERROR !!! '//&
            'Option "'//str(key)//'" is invalid for FLAG argument')
end subroutine getarg__flag
!===============================================================
!
!===============================================================
subroutine getarg__char(key, val)
  implicit none
  character(*), intent(in)  :: key
  character(*), intent(out) :: val

  type(arg_char_), pointer :: arg
  integer :: iarg

  do iarg = 1, argdct%narg
    if( argdct%typ(iarg) /= ITYPE_CHAR ) cycle

    arg => argdct%arglst_char%arg(argdct%idx(iarg))
    if( arg%key_short == key .or. arg%key_long == key )then
      val = arg%val
      arg%nget = arg%nget + 1
      return
    endif
  enddo

  call eerr('!!! INTERNAL ERROR !!! '//&
            'Option "'//str(key)//'" is invalid for CHAR argument')
end subroutine getarg__char
!===============================================================
!
!===============================================================
subroutine getarg__int4(key, val)
  implicit none
  character(*), intent(in)  :: key
  integer(4)  , intent(out) :: val

  type(arg_int4_), pointer :: arg
  integer :: iarg

  do iarg = 1, argdct%narg
    if( argdct%typ(iarg) /= ITYPE_INT4 ) cycle

    arg => argdct%arglst_int4%arg(argdct%idx(iarg))
    if( arg%key_short == key .or. arg%key_long == key )then
      val = arg%val
      arg%nget = arg%nget + 1
      return
    endif
  enddo

  call eerr('!!! INTERNAL ERROR !!! '//&
            'Option "'//str(key)//'" is invalid for INT4 argument')
end subroutine getarg__int4
!===============================================================
!
!===============================================================
subroutine getarg__int8(key, val)
  implicit none
  character(*), intent(in)  :: key
  integer(8)  , intent(out) :: val

  type(arg_int8_), pointer :: arg
  integer :: iarg

  do iarg = 1, argdct%narg
    if( argdct%typ(iarg) /= ITYPE_INT8 ) cycle

    arg => argdct%arglst_int8%arg(argdct%idx(iarg))
    if( arg%key_short == key .or. arg%key_long == key )then
      val = arg%val
      arg%nget = arg%nget + 1
      return
    endif
  enddo

  call eerr('!!! INTERNAL ERROR !!! '//&
            'Option "'//str(key)//'" is invalid for INT8 argument')
end subroutine getarg__int8
!===============================================================
!
!===============================================================
subroutine getarg__real(key, val)
  implicit none
  character(*), intent(in)  :: key
  real(4)     , intent(out) :: val

  type(arg_real_), pointer :: arg
  integer :: iarg

  do iarg = 1, argdct%narg
    if( argdct%typ(iarg) /= ITYPE_REAL ) cycle

    arg => argdct%arglst_real%arg(argdct%idx(iarg))
    if( arg%key_short == key .or. arg%key_long == key )then
      val = arg%val
      arg%nget = arg%nget + 1
      return
    endif
  enddo

  call eerr('!!! INTERNAL ERROR !!! '//&
            'Option "'//str(key)//'" is invalid for REAL argument')
end subroutine getarg__real
!===============================================================
!
!===============================================================
subroutine getarg__dble(key, val)
  implicit none
  character(*), intent(in)  :: key
  real(8)     , intent(out) :: val

  type(arg_dble_), pointer :: arg
  integer :: iarg

  do iarg = 1, argdct%narg
    if( argdct%typ(iarg) /= ITYPE_DBLE ) cycle

    arg => argdct%arglst_dble%arg(argdct%idx(iarg))
    if( arg%key_short == key .or. arg%key_long == key )then
      val = arg%val
      arg%nget = arg%nget + 1
      return
    endif
  enddo

  call eerr('!!! INTERNAL ERROR !!! '//&
            'Option "'//str(key)//'" is invalid for DBLE argument')
end subroutine getarg__dble
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
subroutine assert_nextarg_exist(narg, iarg, key)
  implicit none
  integer     , intent(in) :: narg, iarg
  character(*), intent(in) :: key

  if( iarg == narg )then
    call eerr('Option "'//str(key)//'" requires an argument')
  endif
end subroutine assert_nextarg_exist
!===============================================================
!
!===============================================================
end module lib_io_arg_parser
