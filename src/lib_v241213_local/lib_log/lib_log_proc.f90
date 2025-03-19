module lib_log_proc
  use lib_const
  use lib_time , only: &
    date_and_time_values, &
    timediff
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: echo
  public :: edbg
  public :: ewrn
  public :: eerr
  public :: elog

  public :: get_echo_indent
  !-------------------------------------------------------------
  ! Public variables
  !-------------------------------------------------------------
  public :: CODE
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  !-------------------------------------------------------------
  ! Module Variables
  !-------------------------------------------------------------
  integer, parameter :: CODE_DBG = -1
  integer, parameter :: CODE_WRN = -98
  integer, parameter :: CODE_ERR = -99
  integer, parameter :: CODE_BGN = -10
  integer, parameter :: CODE_RET = -11
  integer, parameter :: CODE_ENT = -12
  integer, parameter :: CODE_EXT = -13
  integer, parameter :: CODE_SET = -21
  integer, parameter :: CODE_STA = -22

  type code_
    integer :: DBG = CODE_DBG
    integer :: WRN = CODE_WRN
    integer :: ERR = CODE_ERR
    integer :: BGN = CODE_BGN
    integer :: RET = CODE_RET
    integer :: ENT = CODE_ENT
    integer :: EXT = CODE_EXT
    integer :: SET = CODE_SET
    integer :: STA = CODE_STA
  end type code_
  type(code_), save :: CODE

  type time_val_
    integer :: bgn(8)
  end type

  type log_stat_comp_
    logical :: is_updated
    logical :: is_recursive
    integer :: nCommand
    logical, pointer :: tf(:)
  end type

  type log_stat_
    integer :: depth
    integer :: indent
    type(log_stat_comp_), pointer :: echoProcess(:)
    type(log_stat_comp_), pointer :: echoContent(:)
    type(log_stat_comp_), pointer :: echoWarning(:)
    type(log_stat_comp_), pointer :: echoProcBar(:)
    type(log_stat_comp_), pointer :: quitAtError(:)
    type(log_stat_comp_), pointer :: measureTime(:)
    integer, pointer :: indentInc(:)
    character(CLEN_VAR), pointer :: proc(:)
    logical, pointer :: is_proc(:)
    type(time_val_), pointer :: time(:)
  end type

  type(log_stat_), target, save :: ls

  integer, parameter :: tf_true  = 0
  integer, parameter :: tf_false = 1
  integer, parameter :: tf_miss = -9

  integer, parameter :: indent_miss = -9999

  logical, parameter :: echoProcess_default = .true.
  logical, parameter :: echoContent_default = .true.
  logical, parameter :: echoWarning_default = .true.
  logical, parameter :: echoError_default   = .true.
  logical, parameter :: echoProcBar_default = .true.
  logical, parameter :: quitAtError_default = .true.
  logical, parameter :: measureTime_default = .false.

  integer, parameter :: STOP_CODE_ERROR = 1

  character(clen_wfmt), parameter :: WFMT_REAL_DEFAULT = 'es12.5'
!---------------------------------------------------------------
contains
!===============================================================
! <Options>
!   +p: Echo process.
!   -p: Mute process.
!
!   +c: Echo contents.
!   -c: Mute contents.
!
!   +w: Echo warnings.
!   -w: Mute warnings.
!
!   +e: Echo error message.
!   -e: Mute error message.
!
!   +a: Echo all. (exept error message)
!   -a: Mute all. (exept error message)
!
!   f: Force output ignoring any settings of muting.
!      This is active only for that message.
!
!   +t: Measure the time.
!   -t: Not measure the time.
!
!   +x[?]: Increase indent by [?] (integer).
!   -x[?]: Decrease indent by [?] (integer).
!    x[?]: Set indent to [?] (integer).
!
!   +n: Make a new line.
!   -n: Do not make a new line.
!
!   +q: Quit when error occured.
!   -q: Not quit when error occured.
!
!   +b: Echo the bar.
!   -b: Mute the bar.
!===============================================================
recursive subroutine echo(cd, msg, opt)
  implicit none
  integer     , intent(in) :: cd
  character(*), intent(in), optional :: msg
  character(*), intent(in), optional :: opt

  character(32) :: opt_, opt1
  integer :: tf_echoProcess
  integer :: tf_echoContent
  integer :: tf_echoWarning
  integer :: tf_echoError
  integer :: tf_echoProcBar
  integer :: tf_quitAtError
  integer :: tf_measureTime
  logical :: is_echoProcess_recursive
  logical :: is_echoContent_recursive
  logical :: is_echoWarning_recursive
  logical :: is_echoProcBar_recursive
  logical :: is_quitAtError_recursive
  logical :: is_measureTime_recursive
  integer :: tf_forceEcho
  integer :: tf_makeNewLine
  integer, save :: tf_makeNewLine_prev = tf_true
  integer :: indent, indentInc, indentDec

  type(log_stat_comp_), pointer :: lsc
  type(log_stat_comp_), pointer :: lsc_time
  integer :: iDepth
  character(:), allocatable :: c
  character(:), allocatable :: c_  ! for avoiding warning of maybe-uninitialized
  character(:), allocatable :: bar
  integer :: i

  integer :: ios

  logical, save :: is_init = .true.
  !-------------------------------------------------------------
  ! Initialize
  !-------------------------------------------------------------
  if( is_init )then
    is_init = .false.

    ls%depth = 0
    ls%indent = 1

    allocate(ls%echoProcess(0:PROCDEPTH))
    allocate(ls%echoContent(0:PROCDEPTH))
    allocate(ls%echoWarning(0:PROCDEPTH))
    allocate(ls%echoProcBar(0:PROCDEPTH))
    allocate(ls%quitAtError(0:PROCDEPTH))
    allocate(ls%measureTime(0:PROCDEPTH))

    do iDepth = 0, PROCDEPTH
      lsc => ls%echoProcess(iDepth)
      allocate(lsc%tf(0:PROCDEPTH))
      lsc%tf(0) = echoProcess_default

      lsc => ls%echoContent(iDepth)
      allocate(lsc%tf(0:PROCDEPTH))
      lsc%tf(0) = echoContent_default

      lsc => ls%echoWarning(iDepth)
      allocate(lsc%tf(0:PROCDEPTH))
      lsc%tf(0) = echoWarning_default

      lsc => ls%echoProcBar(iDepth)
      allocate(lsc%tf(0:PROCDEPTH))
      lsc%tf(0) = echoProcBar_default

      lsc => ls%quitAtError(iDepth)
      allocate(lsc%tf(0:PROCDEPTH))
      lsc%tf(0) = quitAtError_default

      lsc => ls%measureTime(iDepth)
      allocate(lsc%tf(0:PROCDEPTH))
      lsc%tf(0) = measureTime_default
    enddo

    ls%echoProcess(:)%nCommand = 0
    ls%echoContent(:)%nCommand = 0
    ls%echoWarning(:)%nCommand = 0
    ls%echoProcBar(:)%nCommand = 0
    ls%quitAtError(:)%nCommand = 0
    ls%measureTime(:)%nCommand = 0

    allocate(ls%indentInc(0:PROCDEPTH))
    allocate(ls%proc(0:PROCDEPTH))
    allocate(ls%is_proc(0:PROCDEPTH))
    allocate(ls%time(0:PROCDEPTH))

    ls%indentInc(:) = 0
    ls%proc(:) = ''
  endif
  !-------------------------------------------------------------
  ! Read options
  !-------------------------------------------------------------
  tf_echoProcess = tf_miss
  tf_echoContent = tf_miss
  tf_echoWarning = tf_miss
  tf_echoError   = tf_miss
  tf_echoProcBar = tf_miss
  tf_quitAtError = tf_miss
  tf_measureTime = tf_miss

  is_echoProcess_recursive = .false.
  is_echoContent_recursive = .false.
  is_echoWarning_recursive = .false.
  is_echoProcBar_recursive = .false.
  is_quitAtError_recursive = .false.
  is_measureTime_recursive = .false.

  tf_forceEcho   = tf_miss
  tf_makeNewLine = tf_miss

  indent = indent_miss
  indentInc = 0
  indentDec = 0

  if( cd == CODE_SET )then
    opt_ = msg
  elseif( present(opt) )then
    opt_ = adjustl(opt)
  else
    opt_ = ''
  endif

  do while( len_trim(opt_) > 0 )
    read(opt_,*) opt1

    selectcase( opt1 )
    case( '+p' )
      tf_echoProcess = tf_true
    case( '-p' )
      tf_echoProcess = tf_false
    case( '+pr' )
      tf_echoProcess = tf_true
      is_echoProcess_recursive = .true.
    case( '-pr' )
      tf_echoProcess = tf_false
      is_echoProcess_recursive = .true.
    case( '+c' )
      tf_echoContent = tf_true
    case( '-c' )
      tf_echoContent = tf_false
    case( '+cr' )
      tf_echoContent = tf_true
      is_echoContent_recursive = .true.
    case( '-cr' )
      tf_echoContent = tf_false
      is_echoContent_recursive = .true.
    case( '+w' )
      tf_echoWarning = tf_true
    case( '-w' )
      tf_echoWarning = tf_false
    case( '+wr' )
      tf_echoWarning = tf_true
      is_echoWarning_recursive = .true.
    case( '-wr' )
      tf_echoWarning = tf_false
      is_echoWarning_recursive = .true.
    case( '+e' )
      tf_echoError = tf_true
    case( '-e' )
      tf_echoError = tf_false
    case( '+a' )
      tf_echoProcess = tf_true
      tf_echoContent = tf_true
      tf_echoWarning = tf_true
    case( '-a' )
      tf_echoProcess = tf_false
      tf_echoContent = tf_false
      tf_echoWarning = tf_false
    case( '+ar' )
      tf_echoProcess = tf_true
      tf_echoContent = tf_true
      tf_echoWarning = tf_true
      is_echoProcess_recursive = .true.
      is_echoContent_recursive = .true.
      is_echoWarning_recursive = .true.
    case( '-ar' )
      tf_echoProcess = tf_false
      tf_echoContent = tf_false
      tf_echoWarning = tf_false
      is_echoProcess_recursive = .true.
      is_echoContent_recursive = .true.
      is_echoWarning_recursive = .true.
    case( '+b' )
      tf_echoProcBar = tf_true
    case( '-b' )
      tf_echoProcBar = tf_false
    case( '+br' )
      tf_echoProcBar = tf_true
      is_echoProcBar_recursive = .true.
    case( '-br' )
      tf_echoProcBar = tf_false
      is_echoProcBar_recursive = .true.
    case( '+q' )
      tf_quitAtError = tf_true
    case( '-q' )
      tf_quitAtError = tf_false
    case( '+qr' )
      tf_quitAtError = tf_true
      is_quitAtError_recursive = .true.
    case( '-qr' )
      tf_quitAtError = tf_false
      is_quitAtError_recursive = .true.
    case( '+t' )
      tf_measureTime = tf_true
    case( '-t' )
      tf_measureTime = tf_false
    case( '+tr' )
      tf_measureTime = tf_true
      is_measureTime_recursive = .true.
    case( '-tr' )
      tf_measureTime = tf_false
      is_measureTime_recursive = .true.
    case( 'f' )
      tf_forceEcho = tf_true
    case( '+n' )
      tf_makeNewLine = tf_true
    case( '-n' )
      tf_makeNewLine = tf_false
    case default
      if( opt1(:1) == 'x' )then
        read(opt1(2:),*,iostat=ios) indent
      elseif( opt1(:2) == '-x' )then
        read(opt1(3:),*,iostat=ios) indentDec
      elseif( opt1(:2) == '+x' )then
        read(opt1(3:),*,iostat=ios) indentInc
      else
        ios = 1
      endif
      if( ios /= 0 )then
        call eerr('Invalid format option: '//trim(opt_)//'\n'//&
                '\n  msg: "'//trim(msg)//'"'//&
                '\n  opt: "'//trim(opt)//'"')
      endif
    endselect
    opt_ = adjustl(opt_(len_trim(opt1)+1:))
  enddo

  indentInc = indentInc - indentDec
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( cd )
  !-------------------------------------------------------------
  ! Case: Unit number
  case( 0: )
    if( .not. present(msg) )then
      call eerr('Output string was not specified.')
    endif
    !-----------------------------------------------------------
    ! Set indent
    !-----------------------------------------------------------
    if( indent == indent_miss ) indent = 0
    indent = indent + indentInc
    !-----------------------------------------------------------
    ! Write
    !-----------------------------------------------------------
    call echo_lines(msg, cd, indent, .true.)
  !-------------------------------------------------------------
  !
  case( CODE_BGN, CODE_ENT )
    !-----------------------------------------------------------
    ! Update commands
    !-----------------------------------------------------------
    ls%depth = ls%depth + 1

    selectcase( cd )
    case( CODE_BGN )
      ls%is_proc(ls%depth) = .true.
    case( CODE_ENT )
      ls%is_proc(ls%depth) = .false.
    endselect

    call update_commands_in(tf_echoProcess, ls%echoProcess, ls%depth, is_echoProcess_recursive)
    call update_commands_in(tf_echoContent, ls%echoContent, ls%depth, is_echoContent_recursive)
    call update_commands_in(tf_echoWarning, ls%echoWarning, ls%depth, is_echoWarning_recursive)
    call update_commands_in(tf_echoProcBar, ls%echoProcBar, ls%depth, is_echoProcBar_recursive)
    call update_commands_in(tf_quitAtError, ls%quitAtError, ls%depth, is_quitAtError_recursive)
    call update_commands_in(tf_measureTime, ls%measureTime, ls%depth, is_measureTime_recursive)
    !-----------------------------------------------------------
    ! Set indent
    !-----------------------------------------------------------
    if( indent == indent_miss ) indent = ls%indent
    indent = indent + indentInc
    !-----------------------------------------------------------
    ! Echo message
    !-----------------------------------------------------------
    lsc => ls%echoProcess(ls%depth)

    if( lsc%tf(lsc%nCommand) )then
      selectcase( cd )
      case( CODE_BGN )
        call echo_lines('[+ '//trim(msg)//']', STDOUT, indent, .true.)
      case( CODE_ENT )
        call echo_lines(trim(msg), STDOUT, indent, .true.)
      endselect
    endif
    !-----------------------------------------------------------
    ! Save proc name
    !-----------------------------------------------------------
    ls%proc(ls%depth) = msg
    !-----------------------------------------------------------
    ! Update indent
    !-----------------------------------------------------------
    ls%indentInc(ls%depth) = indentInc
    ls%indent = ls%indent + 2 + ls%indentInc(ls%depth)
    !-----------------------------------------------------------
    ! Save time
    !-----------------------------------------------------------
    lsc_time => ls%measureTime(ls%depth)
    if( lsc_time%tf(lsc_time%nCommand) )then
      ls%time(ls%depth)%bgn = date_and_time_values()
    endif
  !-------------------------------------------------------------
  ! Echo log
  !-------------------------------------------------------------
  !
  case( CODE_RET, CODE_EXT )
    !-----------------------------------------------------------
    ! Update indent
    !-----------------------------------------------------------
    ls%indent = ls%indent - 2 - ls%indentInc(ls%depth)
    ls%indentInc(ls%depth) = 0
    !-----------------------------------------------------------
    ! Echo message
    !-----------------------------------------------------------
    lsc => ls%echoProcess(ls%depth)

    if( lsc%tf(lsc%nCommand) )then
      selectcase( cd )
      case( CODE_RET )
        lsc_time => ls%measureTime(ls%depth)
        if( lsc_time%tf(lsc_time%nCommand) )then
          !allocate(character(1) :: c)
          !c = '[- '//trim(ls%proc(ls%depth))//' ('//&
          !    str(timediff(ls%time(ls%depth)%bgn, &
          !                 date_and_time_values()),'f8.3')//' sec)]'
          allocate(character(8) :: c)
          write(c,"(f8.3)") timediff(ls%time(ls%depth)%bgn, date_and_time_values())
          allocate(character(1) :: c_)
          c_ = c
          c = '[- '//trim(ls%proc(ls%depth))//' ('//trim(c_)//' sec)]'
        else
          allocate(character(1) :: c)
          c = '[- '//trim(ls%proc(ls%depth))//']'
        endif
        call echo_lines(c, STDOUT, ls%indent, .true.)
      case( CODE_EXT )
        continue
      endselect
    endif
    !-----------------------------------------------------------
    ! Update commands
    !-----------------------------------------------------------
    call update_commands_out(ls%echoProcess, ls%depth)
    call update_commands_out(ls%echoContent, ls%depth)
    call update_commands_out(ls%echoWarning, ls%depth)
    call update_commands_out(ls%echoProcBar, ls%depth)
    call update_commands_out(ls%quitAtError, ls%depth)
    call update_commands_out(ls%measureTime, ls%depth)

    ls%depth = ls%depth - 1
  !-------------------------------------------------------------
  !
  case( CODE_DBG )
    if( indent == indent_miss )then
      indent = ls%indent + indentInc
    endif

    lsc => ls%echoContent(ls%depth)
    if( lsc%tf(lsc%nCommand) .or. tf_forceEcho == tf_true )then
      call echo_lines(msg, STDOUT, indent, tf_makeNewLine/=tf_false)
    endif
  !-------------------------------------------------------------
  !
  case( CODE_WRN )
    if( indent == indent_miss ) indent = ls%indent
    indent = indent + indentInc

    lsc => ls%echoWarning(ls%depth)
    if( lsc%tf(lsc%nCommand) .or. tf_forceEcho == tf_true )then
      allocate(character(1) :: c)
      c = '****** WARNING @ '//trim(ls%proc(ls%depth))//' ******'

      !call update_tf_miss(tf_echoProcess, ls%echoProcess(ls%depth))
      !call update_tf_miss(tf_echoProcBar, ls%echoProcBar(ls%depth))

      if( tf_makeNewLine_prev == tf_false ) write(STDOUT,*)

      !if( tf_echoProcess == tf_true )then
      if( tf_echoProcess /= tf_false )then
        call echo_lines(trim(c), STDOUT, indent, .true.)
      endif

      if( msg /= '' )then
        call echo_lines(msg, STDOUT, indent, tf_makeNewLine/=tf_false)
      endif

      !if( tf_echoProcBar == tf_true )then
      if( tf_echoProcBar /= tf_false )then
        !call echo_lines(str('', len_trim(c), '*'), STDOUT, indent, .true.)
        allocate(character(1) :: bar)
        bar = trim(c)
        do i = 1, len_trim(c)
          bar(i:i) = '*'
        enddo
        call echo_lines(bar, STDOUT, indent, .true.)
      endif
    endif
  !-------------------------------------------------------------
  !
  case( CODE_ERR )
    if( indent == indent_miss ) indent = 0
    indent = indent + indentInc

    if( tf_echoError /= tf_false )then
      allocate(character(1) :: c)
      c = '****** ERROR @ '//trim(ls%proc(ls%depth))//' ******'

      call update_tf_miss(tf_quitAtError, ls%quitAtError(ls%depth))

      if( tf_makeNewLine_prev == tf_false ) write(STDOUT,*)

      if( tf_echoProcess /= tf_false )then
        call echo_lines(trim(c), STDOUT, indent, .true.)

        do iDepth = ls%depth, 1, -1
          if( ls%is_proc(iDepth) )then
            call echo_lines('[- '//trim(ls%proc(iDepth))//']', STDOUT, indent, .true.)
          else
            call echo_lines('(- '//trim(ls%proc(iDepth))//')', STDOUT, indent, .true.)
          endif
        enddo
      endif

      if( msg /= '' )then
        call echo_lines(msg, STDOUT, indent, .true.)
      endif

      if( tf_echoProcBar /= tf_false )then
        !call echo_lines(str('', len_trim(c), '*'), STDOUT, indent, .true.)
        allocate(character(1) :: bar)
        bar = trim(c)
        do i = 1, len_trim(c)
          bar(i:i) = '*'
        enddo
        call echo_lines(bar, STDOUT, indent, .true.)
      endif
    endif

    if( tf_quitAtError /= tf_false )then
      stop STOP_CODE_ERROR
    endif
  !-------------------------------------------------------------
  !
  case( CODE_SET )
    if( tf_forceEcho /= tf_miss )then
      call eerr("Option 'f' is invalid for the mode 'SET'.")
    endif

    if( indent /= indent_miss )then
      call eerr("Option 'x' is invalid for the mode 'SET'.")
    endif
    !-----------------------------------------------------------
    ! Set %echoProcess
    !-----------------------------------------------------------
    call update_commands_in(tf_echoProcess, ls%echoProcess, ls%depth, is_echoProcess_recursive)
    call update_commands_in(tf_echoContent, ls%echoContent, ls%depth, is_echoContent_recursive)
    call update_commands_in(tf_echoWarning, ls%echoWarning, ls%depth, is_echoWarning_recursive)
    call update_commands_in(tf_echoProcBar, ls%echoProcBar, ls%depth, is_echoProcBar_recursive)
    call update_commands_in(tf_quitAtError, ls%quitAtError, ls%depth, is_quitAtError_recursive)
    call update_commands_in(tf_measureTime, ls%measureTime, ls%depth, is_measureTime_recursive)
    !-----------------------------------------------------------
    ! Update indent
    !-----------------------------------------------------------
    ls%indent = ls%indent + indentInc
    ls%indentInc(ls%depth) = ls%indentInc(ls%depth) + indentInc
  !-------------------------------------------------------------
  !
  case default

  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  tf_makeNewLine_prev = tf_makeNewLine
end subroutine echo
!===============================================================
!
!===============================================================
subroutine update_commands_in(cmd, ls_comp, depth, is_recursive)
  implicit none
  integer             , intent(in) :: cmd
  type(log_stat_comp_), pointer    :: ls_comp(:)
  integer             , intent(in) :: depth
  logical             , intent(in) :: is_recursive

  integer :: iDepth
  type(log_stat_comp_), pointer :: lsc

  if( cmd == tf_miss ) return

  ls_comp(depth)%is_updated = .true.
  ls_comp(depth)%is_recursive = is_recursive

  if( is_recursive )then
    do iDepth = depth, PROCDEPTH
      lsc => ls_comp(iDepth)
      lsc%nCommand = lsc%nCommand + 1
      lsc%tf(lsc%nCommand) = cmd == tf_true
    enddo
  else
    lsc => ls_comp(depth)
    lsc%nCommand = lsc%nCommand + 1
    lsc%tf(lsc%nCommand) = cmd == tf_true
  endif
end subroutine update_commands_in
!===============================================================
!
!===============================================================
subroutine update_commands_out(ls_comp, depth)
  implicit none
  type(log_stat_comp_), pointer :: ls_comp(:)
  integer, intent(in) :: depth

  if( ls_comp(depth)%is_updated )then
    if( ls_comp(depth)%is_recursive )then
      ls_comp(depth:)%nCommand = ls_comp(depth:)%nCommand - 1
    else
      ls_comp(depth)%nCommand = ls_comp(depth)%nCommand - 1
    endif

    ls_comp(depth)%is_updated = .false.
  endif
end subroutine update_commands_out
!===============================================================
!
!===============================================================
subroutine echo_lines(msg, un, idt, adv)
  implicit none
  character(*), intent(in) :: msg
  integer     , intent(in) :: un
  integer     , intent(in) :: idt
  logical     , intent(in) :: adv
  character(len_trim(msg)) :: msg_
  character(16) :: wfmt
  character(4)  :: advance
  integer :: leng
  integer :: loc
  !-------------------------------------------------------------
  ! Start a new line after writing the message if $adv is true.
  !-------------------------------------------------------------
  if( adv )then
    advance = 'yes'
  else
    advance = 'no'
  endif
  !-------------------------------------------------------------
  ! Modify indent.
  !-------------------------------------------------------------
  if( idt == 0 )then
    wfmt = "(a)"
  else
    write(wfmt,"(a,i0,a)") '(',idt,'x,a)'
  endif
  !-------------------------------------------------------------
  ! Write the message.
  !-------------------------------------------------------------
  if( index(msg,'\n') == 0 )then
    write(un, wfmt, advance=advance) trim(msg)

  else

    msg_ = msg
    leng = len_trim(msg)

    do
      loc = index(msg_,'\n')

      selectcase( loc )

      case( 0 )
        write(un, wfmt, advance=advance) trim(msg_)
        exit

      case( 1 )
        write(un, wfmt) ''

      case( 2: )
        write(un, wfmt) trim(msg_(:loc-1))

      endselect

      if( loc+1 == len_trim(msg_) )then
        exit
      else
        msg_ = msg_(loc+2:)
      endif
    enddo

  endif
end subroutine echo_lines
!==============================================================
!
!==============================================================
subroutine update_tf_miss(tf, lsc)
  implicit none
  integer, intent(inout) :: tf
  type(log_stat_comp_), intent(in) :: lsc

  if( tf /= tf_miss ) return

  if( lsc%tf(lsc%nCommand) )then
    tf = tf_true
  else
    tf = tf_false
  endif
end subroutine update_tf_miss
!==============================================================
!
!==============================================================
subroutine edbg(msg, opt)
  implicit none
  character(*), intent(in) :: msg
  character(*), intent(in), optional :: opt

  if( present(opt) )then
    call echo(CODE%DBG, msg, opt)
  else
    call echo(CODE%DBG, msg)
  endif
end subroutine edbg
!==============================================================
!
!==============================================================
subroutine ewrn(msg, opt)
  implicit none
  character(*), intent(in) :: msg
  character(*), intent(in), optional :: opt

  if( present(opt) )then
    call echo(CODE%WRN, msg, opt)
  else
    call echo(CODE%WRN, msg)
  endif
end subroutine ewrn
!==============================================================
!
!==============================================================
subroutine eerr(msg, opt)
  implicit none
  character(*), intent(in) :: msg
  character(*), intent(in), optional :: opt

  if( present(opt) )then
    call echo(CODE%ERR, msg, opt)
  else
    call echo(CODE%ERR, msg)
  endif
end subroutine eerr
!==============================================================
!
!==============================================================
subroutine elog(un, msg)
  implicit none
  integer     , intent(in) :: un
  character(*), intent(in) :: msg

  write(un, "(a)") msg
end subroutine elog
!==============================================================
!
!==============================================================
subroutine get_echo_indent(res)
  implicit none
  integer, intent(out) :: res

  res = ls%indent
end subroutine get_echo_indent
!==============================================================
!
!==============================================================
end module lib_log_proc
