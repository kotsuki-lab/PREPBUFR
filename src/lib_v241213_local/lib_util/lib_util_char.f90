module lib_util_char
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  public :: num_of_words
  public :: count_words
  public :: devide_into_words

  public :: log4_char
  public :: int1_char
  public :: int2_char
  public :: int4_char
  public :: int8_char
  public :: real_char
  public :: dble_char

  public :: char_to_val

  public :: remove_quoted
  public :: search_word
  public :: count_word
  public :: remove_word
  public :: replace_word
  public :: remove_comment

  public :: split
  public :: splitted
  public :: sliced
  !------------------------------------------------------------
  !
  !------------------------------------------------------------
  interface char_to_val
    module procedure char_to_log4
    module procedure char_to_int1
    module procedure char_to_int2
    module procedure char_to_int4
    module procedure char_to_int8
    module procedure char_to_real
    module procedure char_to_dble
  end interface
  !------------------------------------------------------------
contains
!==============================================================
!
!==============================================================
integer function num_of_words(str, dlm)
  implicit none
  character(*), intent(in) :: str
  character(*), intent(in), optional :: dlm
  character(len(str)) :: dlm_
  integer :: len_dlm

  dlm_ = ' '
  len_dlm = 1
  if( present(dlm) )then
    dlm_ = dlm
    len_dlm = len(dlm)
  endif

  call count_words(str, num_of_words, dlm_(:len_dlm))
end function num_of_words
!==============================================================
!
!==============================================================
subroutine count_words(str, n, dlm)
  implicit none
  character(*), intent(in)  :: str
  integer     , intent(out) :: n
  character(*), intent(in), optional :: dlm
  character(len(str)) :: str_
  character(len(str)) :: dlm_
  integer :: loc
  integer :: clen
  integer :: len_dlm

  n = 1
  str_ = trim(adjustl(str))
  clen = len_trim(str_)

  dlm_ = ' '
  len_dlm = 1
  if( present(dlm) )then
    dlm_ = dlm
    len_dlm = len(dlm)
  endif

  do
    loc = index(str_(:clen), dlm_(:len_dlm))
    if( loc == 0 ) exit
    str_ = adjustl(str_(loc+1:clen))
    clen = len_trim(str_)
    n = n + 1
  enddo
end subroutine count_words
!==============================================================
!
!==============================================================
subroutine devide_into_words(str, words)
  implicit none
  character(*), intent(in)  :: str
  character(*), intent(out) :: words(:)

  read(str,*) words(:)
end subroutine devide_into_words
!==============================================================
!
!==============================================================
!
!
!
!
!
!==============================================================
!
!==============================================================
integer(1) function int1_char(c,varname) result(res)
  implicit none
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  allocate(character(1) :: varname_)

  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\nFailed to read "'//str(c)//'" as an integer(1).'//&
            '\n  Variable name: '//str(varname_))
    res = 0_1
  endif

  deallocate(varname_)
end function int1_char
!==============================================================
!
!==============================================================
integer(2) function int2_char(c,varname) result(res)
  implicit none
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  allocate(character(1) :: varname_)

  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\nFailed to read "'//str(c)//'" as an integer(2).'//&
            '\n  Variable name: '//str(varname_))
    res = 0_2
  endif

  deallocate(varname_)
end function int2_char
!==============================================================
!
!==============================================================
integer(4) function int4_char(c,varname) result(res)
  implicit none
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  allocate(character(1) :: varname_)

  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\nFailed to read "'//str(c)//'" as an integer(4).'//&
            '\n  Variable name: '//str(varname_))
    res = 0_4
  endif

  deallocate(varname_)
end function int4_char
!==============================================================
!
!==============================================================
integer(8) function int8_char(c,varname) result(res)
  implicit none
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  allocate(character(1) :: varname_)

  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\nFailed to read "'//str(c)//'" as an integer(8).'//&
            '\n  Variable name: '//str(varname_))
    res = 0_8
  endif

  deallocate(varname_)
end function int8_char
!==============================================================
!
!==============================================================
real(4) function real_char(c,varname) result(res)
  implicit none
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  allocate(character(1) :: varname_)

  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\nFailed to read "'//str(c)//'" as an real(4).'//&
            '\n  Variable name: '//str(varname_))
    res = 0.0
  endif

  deallocate(varname_)
end function real_char
!==============================================================
!
!==============================================================
real(8) function dble_char(c,varname) result(res)
  implicit none
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  allocate(character(1) :: varname_)

  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\nFailed to read "'//str(c)//'" as an real(8).'//&
            '\n  Variable name: '//str(varname_))
    res = 0.d0
  endif

  deallocate(varname_)
end function dble_char
!==============================================================
!
!==============================================================
logical(4) function log4_char(c,varname) result(res)
  implicit none
  character(*), intent(in) :: c
  character(*), intent(in), optional :: varname

  character(:), allocatable :: varname_
  integer :: ios

  allocate(character(1) :: varname_)

  if( present(varname) )then
    varname_ = trim(varname)
  else
    varname_ = '(no input)'
  endif

  read(c,*,iostat=ios) res

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\nFailed to read "'//str(c)//'" as an logical(4).'//&
            '\n  Variable name: '//str(varname_))
    res = .true.
  endif

  deallocate(varname_)
end function log4_char
!==============================================================
!
!==============================================================
!
!
!
!
!
!==============================================================
!
!==============================================================
subroutine char_to_int1(v, c)
  implicit none
  integer(1)  , intent(out) :: v
  character(*), intent(in) :: c
  integer :: ios

  call echo(code%bgn, 'char_to_val__MP__char_to_int1', '-p')
  !-------------------------------------------------------------
  read(c,*,iostat=ios) v

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\n  Failed to convert "'//str(c)//'" to integer(1)')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine char_to_int1
!==============================================================
!
!==============================================================
subroutine char_to_int2(v, c)
  implicit none
  integer(2)  , intent(out) :: v
  character(*), intent(in) :: c

  integer :: ios

  call echo(code%bgn, 'char_to_val__MP__char_to_int2', '-p')
  !-------------------------------------------------------------
  read(c,*,iostat=ios) v

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\n  Failed to convert "'//str(c)//'" to integer(2)')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine char_to_int2
!==============================================================
!
!==============================================================
subroutine char_to_int4(v, c, ios)
  implicit none
  integer(4)  , intent(out) :: v
  character(*), intent(in) :: c
  integer     , intent(out), optional :: ios

  integer :: ios_

  call echo(code%bgn, 'char_to_val__MP__char_to_int4', '-p')
  !-------------------------------------------------------------
  read(c,*,iostat=ios_) v

  if( ios_ /= 0 )then
    if( present(ios) )then
      ios = ios_
    else
      call eerr(str(msg_io_error())//&
              '\n  Failed to convert "'//str(c)//'" to integer(4)')
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine char_to_int4
!==============================================================
!
!==============================================================
subroutine char_to_int8(v, c)
  implicit none
  integer(8)  , intent(out) :: v
  character(*), intent(in) :: c

  integer :: ios

  call echo(code%bgn, 'char_to_val__MP__char_to_int8', '-p')
  !-------------------------------------------------------------
  read(c,*,iostat=ios) v

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\n  Failed to convert "'//str(c)//'" to integer(8)')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine char_to_int8
!==============================================================
!
!==============================================================
subroutine char_to_real(v, c, rfmt)
  implicit none
  real(4)     , intent(out) :: v
  character(*), intent(in) :: c
  character(*), intent(in), optional :: rfmt

  integer :: ios

  call echo(code%bgn, 'char_to_val__MP__char_to_real', '-p')
  !-------------------------------------------------------------
  if( present(rfmt) )then
    read(c,rfmt,iostat=ios) v
  else
    read(c,*,iostat=ios) v
  endif

  if( ios /= 0 )then
    if( present(rfmt) )then
      call eerr(str(msg_io_error())//&
              '\n  Failed to convert "'//str(c)//'" to real(4)'//&
              '\n  rfmt: '//str(rfmt))
    else
      call eerr(str(msg_io_error())//&
              '\n  Failed to convert "'//str(c)//'" to real(4)')
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine char_to_real
!==============================================================
!
!==============================================================
subroutine char_to_dble(v, c)
  implicit none
  real(8)     , intent(out) :: v
  character(*), intent(in) :: c

  integer :: ios

  call echo(code%bgn, 'char_to_val__MP__char_to_dble', '-p')
  !-------------------------------------------------------------
  read(c,*,iostat=ios) v

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\n  Failed to convert "'//str(c)//'" to real(8)')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine char_to_dble
!==============================================================
!
!==============================================================
subroutine char_to_log4(v, c)
  implicit none
  logical(4)  , intent(out) :: v
  character(*), intent(in) :: c

  integer :: ios

  call echo(code%bgn, 'char_to_val__MP__char_to_log4', '-p')
  !-------------------------------------------------------------
  read(c,*,iostat=ios) v

  if( ios /= 0 )then
    call eerr(str(msg_io_error())//&
            '\n  Failed to convert "'//str(c)//'" to logical(4)')
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine char_to_log4
!===============================================================
!
!===============================================================
!
!
!
!
!
!==============================================================
!
!==============================================================
subroutine remove_quoted(c, quote)
  implicit none
  character(*), intent(inout) :: c
  character(*), intent(in), optional :: quote
  integer :: clen
  integer :: ic0, ic
  integer :: ic0_singleQuote, ic0_doubleQuote
  logical :: removeSingleQuote, removeDoubleQuote
  character(CLEN_VAR), parameter :: proc = 'remove_quoted'

  if( present(quote) )then
    removeSingleQuote = .false.
    removeDoubleQuote = .false.
    selectcase( quote )
    case( QUOTE_BOTH )
      removeSingleQuote = .true.
      removeDoubleQuote = .true.
    case( QUOTE_SINGLE )
      removeSingleQuote = .true.
    case( QUOTE_DOUBLE )
      removeDoubleQuote = .true.
    case( QUOTE_NONE )
    endselect
  else
    removeSingleQuote = .true.
    removeDoubleQuote = .true.
  endif

  clen = len(c)

  ic0 = 1
  do
    ic0_singleQuote = index(c(ic0:),"'") + ic0 - 1
    ic0_doubleQuote = index(c(ic0:),'"') + ic0 - 1

    if( ic0_singleQuote == ic0-1 ) ic0_singleQuote = clen
    if( ic0_doubleQuote == ic0-1 ) ic0_doubleQuote = clen

    if( (.not. removeSingleQuote .or. ic0_singleQuote == clen) .and. &
        (.not. removeDoubleQuote .or. ic0_doubleQuote == clen) ) exit
    !-----------------------------------------------------------
    ! Remove single quote
    !-----------------------------------------------------------
    if( removeSingleQuote .and. ic0_singleQuote < ic0_doubleQuote )then
      ic = index(c(ic0_singleQuote+1:),"'")
      if( ic == 0 )then
        write(STDOUT,"(a)") '*** ERROR @ '//trim(proc)//' ***'
        write(STDOUT,"(a)") trim(msg_unexpected_condition())
        write(STDOUT,"(a)") 'Single quote was not closed. String:'
        write(STDOUT,"(a)") trim(c)
        stop
      endif

      c = c(:ic0_singleQuote-1)//c(ic0_singleQuote+ic+1:)
    !-----------------------------------------------------------
    ! Remove double quote
    !-----------------------------------------------------------
    elseif( removeDoubleQuote .and. ic0_doubleQuote < ic0_singleQuote )then
      ic = index(c(ic0_doubleQuote+1:),'"')
      if( ic == 0 )then
        write(STDOUT,"(a)") '*** ERROR @ '//trim(proc)//' ***'
        write(STDOUT,"(a)") trim(msg_unexpected_condition())
        write(STDOUT,"(a)") 'Double quote was not closed. String:'
        write(STDOUT,"(a)") trim(c)
        stop
      endif

      c = c(:ic0_doubleQuote-1)//c(ic0_doubleQuote+ic+1:)
    endif
  enddo
end subroutine remove_quoted
!==============================================================
!
!==============================================================
subroutine search_word(c, word, loc, quoteIgnored)
  implicit none
  character(*), intent(in)  :: c
  character(*), intent(in)  :: word
  integer     , intent(out) :: loc
  character(*), intent(in), optional :: quoteIgnored
  integer :: clen
  integer :: ic0, iic_word, iic_singleQuote, iic_doubleQuote
  integer :: stat
  logical :: ignoreSingleQuote, ignoreDoubleQuote
  character(CLEN_VAR) :: proc = 'search_word'

  loc = 0

  if( index(c,word) == 0 ) return

  if( present(quoteIgnored) )then
    ignoreSingleQuote = .false.
    ignoreDoubleQuote = .false.
    selectcase( quoteIgnored )
    case( QUOTE_BOTH )
      ignoreSingleQuote = .true.
      ignoreDoubleQuote = .true.
    case( QUOTE_SINGLE )
      ignoreSingleQuote = .true.
    case( QUOTE_DOUBLE )
      ignoreDoubleQuote = .true.
    case( QUOTE_NONE )
    case default
      write(STDOUT,"(a)") '*** ERROR @ '//trim(proc)//' ***'
      write(STDOUT,"(a)") trim(msg_invalid_value())
      write(STDOUT,"(a)") '  quoteIgnored: "'//trim(quoteIgnored)//'"'
      stop
    endselect
  else
    ignoreSingleQuote = .true.
    ignoreDoubleQuote = .true.
  endif

  clen = len(c)

  stat = 0
  ic0 = 1
  do
    iic_word = index(c(ic0:),word)
    iic_singleQuote = index(c(ic0:),"'")
    iic_doubleQuote = index(c(ic0:),'"')
    if( iic_word == 0 )then
      return
    endif

    if( iic_singleQuote == 0 ) iic_singleQuote = clen
    if( iic_doubleQuote == 0 ) iic_doubleQuote = clen

    if( ignoreSingleQuote .and. &
        iic_singleQuote < iic_word .and. &
        iic_singleQuote < iic_doubleQuote )then
      ic0 = ic0 + iic_singleQuote
      iic_singleQuote = index(c(ic0+1:),"'")
      if( iic_doubleQuote == 0 )then
        write(STDOUT,"(a)") '*** ERROR @ '//trim(proc)//' ***'
        write(STDOUT,"(a)") trim(msg_unexpected_condition())
        write(STDOUT,"(a)") 'Single quote was not closed.'
        stop
      endif
      ic0 = ic0 + iic_singleQuote + 1
      if( ic0 > clen ) exit
    elseif( ignoreDoubleQuote .and. &
            iic_doubleQuote < iic_word .and. &
            iic_doubleQuote < iic_singleQuote )then
      ic0 = ic0 + iic_doubleQuote
      iic_doubleQuote = index(c(ic0+1:),'"')
      if( iic_doubleQuote == 0 )then
        write(STDOUT,"(a)") '*** ERROR @ '//trim(proc)//' ***'
        write(STDOUT,"(a)") trim(msg_unexpected_condition())
        write(STDOUT,"(a)") 'Double quote was not closed.'
        stop
      endif
      ic0 = ic0 + iic_doubleQuote + 1
      if( ic0 > clen ) exit
    else
      loc = ic0 + iic_word - 1
      exit
    endif
  enddo
end subroutine search_word
!==============================================================
!
!==============================================================
subroutine count_word(c, word, n, quoteIgnored)
  implicit none
  character(*), intent(in)  :: c
  character(*), intent(in)  :: word
  integer     , intent(out) :: n
  character(*), intent(in), optional :: quoteIgnored
  integer :: clen
  integer :: ic0, iic_word, iic_singleQuote, iic_doubleQuote
  integer :: stat
  logical :: ignoreSingleQuote, ignoreDoubleQuote
  character(CLEN_VAR), parameter :: proc = 'count_word'

  n = 0

  if( index(c,word) == 0 ) return

  if( present(quoteIgnored) )then
    ignoreSingleQuote = .false.
    ignoreDoubleQuote = .false.
    selectcase( quoteIgnored )
    case( QUOTE_BOTH )
      ignoreSingleQuote = .true.
      ignoreDoubleQuote = .true.
    case( QUOTE_SINGLE )
      ignoreSingleQuote = .true.
    case( QUOTE_DOUBLE )
      ignoreDoubleQuote = .true.
    case( QUOTE_NONE )
    endselect
  else
    ignoreSingleQuote = .true.
    ignoreDoubleQuote = .true.
  endif

  clen = len(c)

  stat = 0
  ic0 = 1
  do
    iic_word = index(c(ic0:),word)
    iic_singleQuote = index(c(ic0:),"'")
    iic_doubleQuote = index(c(ic0:),'"')

    if( iic_word == 0 ) return

    if( iic_singleQuote == 0 ) iic_singleQuote = clen
    if( iic_doubleQuote == 0 ) iic_doubleQuote = clen

    if( ignoreSingleQuote .and. &
        iic_singleQuote < iic_word .and. &
        iic_singleQuote < iic_doubleQuote )then
      ic0 = ic0 + iic_singleQuote
      iic_singleQuote = index(c(ic0+1:),"'")
      if( iic_doubleQuote == 0 )then
        write(STDOUT,"(a)") '*** ERROR @ '//trim(proc)//' ***'
        write(STDOUT,"(a)") 'Single quote was not closed.'
        stop
      endif
      ic0 = ic0 + iic_singleQuote + 1
    elseif( ignoreDoubleQuote .and. &
            iic_doubleQuote < iic_word .and. &
            iic_doubleQuote < iic_singleQuote )then
      ic0 = ic0 + iic_doubleQuote
      iic_doubleQuote = index(c(ic0+1:),'"')
      if( iic_doubleQuote == 0 )then
        write(STDOUT,"(a)") '*** ERROR @ '//trim(proc)//' ***'
        write(STDOUT,"(a)") 'Double quote was not closed.'
        stop
      endif
      ic0 = ic0 + iic_doubleQuote + 1
    else
      n = n + 1
      ic0 = ic0 + iic_word
    endif

    if( ic0 > clen ) exit
  enddo
end subroutine count_word
!==============================================================
!
!==============================================================
subroutine remove_word(c, word, n)
  implicit none
  character(*), intent(inout) :: c
  character(*), intent(in)    :: word
  integer     , intent(in), optional :: n

  character(len(c)) :: c_in
  integer :: n_
  integer :: loc
  integer :: cl, wl
  integer :: counter
  character(CLEN_VAR), parameter :: proc = 'remove_word'

  c_in = c

  n_ = 0
  if( present(n) ) n_ = n

  cl = len(c)
  wl = len(word)

  if( n_ == 0 )then
    counter = 0
    do
      loc = index(c(:cl-wl*counter),word)

      if( loc == 0 ) exit

      c = c(:loc-1)//c(loc+cl:)

      counter = counter + 1
    enddo
  else
    counter = 0
    do while( counter < n_ )
      loc = index(c(:cl-wl*counter),word) 

      if( loc == 0 )then
        write(STDOUT, "(a)") '*** ERROR @ '//trim(proc)//' ***'
        write(STDOUT, "(a)") 'Word "'//trim(word)//'" was not found any more.'
        write(STDOUT,"(a)") 'string: '//c_in
        write(STDOUT,"(a)") 'word: '//word
        write(STDOUT,"(a,i0)") 'n: ',n
        stop
      endif

      c = c(:loc-1)//c(loc+cl:)

      counter = counter + 1
    enddo
  endif
end subroutine remove_word
!==============================================================
!
!==============================================================
subroutine replace_word(c, word_old, word_new, n)
  implicit none
  character(*), intent(inout) :: c
  character(*), intent(in)    :: word_old, word_new
  integer     , intent(in), optional    :: n

  character(len(c)) :: c_in
  integer :: n_
  integer :: loc
  integer :: counter
  character(CLEN_VAR), parameter :: proc = 'replace_word'

  c_in = c

  n_ = 0
  if( present(n) ) n_ = n

  if( n_ == 0 )then
    do
      loc = index(c, word_old)
      if( loc == 0 ) exit
      c = c(:loc-1)//word_new//c(loc+len(word_old):)
    enddo
  else
    counter = 0
    do while( counter < n_ )
      counter = counter + 1

      loc = index(c, word_old)
      if( loc == 0 )then
        write(STDOUT,"(a)") '*** ERROR @ '//trim(proc)//' ***'
        write(STDOUT,"(a)") 'Word "'//word_old//'" was not found any more.'
        write(STDOUT,"(a)") 'string: '//trim(c_in)
        write(STDOUT,"(a,i0)") 'n: ',n
        stop
      endif

      c = c(:loc-1)//word_new//c(loc+len(word_old):)
    enddo
  endif
end subroutine replace_word
!==============================================================
!
!==============================================================
subroutine remove_comment(c, sgn, quote)
  implicit none
  character(*), intent(inout)  :: c
  character(*), intent(in), optional :: sgn
  character(*), intent(in), optional :: quote
  character(:), allocatable :: sgn_
  character(CLEN_KEY) :: quote_
  integer :: loc

  if( present(sgn) )then
    allocate(character(len(sgn)) :: sgn_)
    sgn_ = sgn
  else
    allocate(character(1) :: sgn_)
    sgn_ = '#'
  endif

  quote_ = QUOTE_BOTH
  if( present(quote) ) quote_ = quote

  call search_word(c, sgn_, loc, quote_)

  if( loc > 0 )then
    c = c(:loc-1)
  endif
end subroutine remove_comment
!==============================================================
!
!==============================================================
subroutine split(c, dlm, n, quote)
  implicit none
  character(*), intent(inout) :: c
  character(*), intent(in)    :: dlm
  integer     , intent(in)    :: n
  character(*), intent(in), optional :: quote

  character(len(c)) :: c_in
  character(CLEN_KEY) :: quote_
  integer :: ic
  integer :: i
  character(CLEN_VAR), parameter :: proc = 'split'

  quote_ = QUOTE_BOTH
  if( present(quote) ) quote_ = quote

  c_in = c

  do i = 1, n-1
    call search_word(c, dlm, ic, quote_)
    if( ic == 0 )then
      write(STDOUT,"(a)") '*** ERROR @ '//trim(proc)//' ***'
      write(STDOUT,"(a)") 'Delimiter "'//dlm//'" was not found any more.'
      write(STDOUT,"(a)") 'string: '//trim(c_in)
      write(STDOUT,"(a,i0)") 'n: ',n
      write(STDOUT,"(a)") 'quote: '//trim(quote)
      stop
    elseif( ic == len(c) )then
      c = ''
    else
      c = c(ic+len(dlm):)
    endif
  enddo

  call search_word(c, dlm, ic, quote_)

  if( ic > 0 )then
    c = c(:ic-1)
  endif
end subroutine split
!==============================================================
!
!==============================================================
function splitted(c_in, dlm, n, quote) result(c)
  implicit none
  character(*), intent(in) :: c_in
  character(*), intent(in) :: dlm
  integer     , intent(in) :: n
  character(*), intent(in), optional :: quote
  character(len(c_in))     :: c
  character(CLEN_KEY) :: quote_
  integer :: ic
  integer :: i
  character(CLEN_VAR), parameter :: proc = 'splitted'

  quote_ = QUOTE_BOTH
  if( present(quote) ) quote_ = quote

  c = c_in

  do i = 1, n-1
    call search_word(c, dlm, ic, quote_)
    if( ic == 0 )then
      write(STDOUT,"(a)") '*** ERROR @ '//trim(proc)//' ***'
      write(STDOUT,"(a)") 'Delimiter "'//dlm//'" was not found any more.'
      write(STDOUT,"(a)") 'string: '//trim(c_in)
      write(STDOUT,"(a,i0)") 'n: ',n
      stop
    elseif( ic == len(c) )then
      c = ''
    else
      c = c(ic+len(dlm):)
      !---------------------------------------------------------
      ! Remove delimiters if it continues.
      !---------------------------------------------------------
      do while( len(c) >= len(dlm) )
        if( c(:len(dlm)) == dlm )then
          if( len(c) == len(dlm) )then
            c = ''
          else
            c = c(len(dlm)+1:)
          endif
        else
          exit
        endif
      enddo
      !---------------------------------------------------------
    endif
  enddo
  !-------------------------------------------------------------
  ! Slice a string ahead of the next delimiter.
  !-------------------------------------------------------------
  call search_word(c, dlm, ic, quote_)

  if( ic > 0 )then
    c = c(:ic-1)
  endif
end function splitted
!==============================================================
!
!==============================================================
function sliced(c_in, i0, i1) result(c_out)
  implicit none
  character(*), intent(in) :: c_in
  integer     , intent(in) :: i0, i1
  character(len(c_in))     :: c_out
  integer :: i0_, i1_

  i0_ = i0
  if( i0 < 0 ) i0_ = len(c_in) + i0

  i1_ = i1
  if( i1 < 0 ) i1_ = len(c_in) + i1

  c_out = c_in(i0_:i1_)
end function sliced
!==============================================================
!
!==============================================================
end module lib_util_char
