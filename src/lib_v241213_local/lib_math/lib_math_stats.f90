module lib_math_stats
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: get_stats
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface get_stats
    module procedure get_stats_int1_1d
    module procedure get_stats_int1_2d
    module procedure get_stats_int2_1d
    module procedure get_stats_int2_2d
    module procedure get_stats_int4_1d
    module procedure get_stats_int4_2d
    module procedure get_stats_int8_1d
    module procedure get_stats_int8_2d
    module procedure get_stats_real_1d
    module procedure get_stats_real_2d
    module procedure get_stats_dble_1d
    module procedure get_stats_dble_2d
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine get_stats_int1_1d(dat, vmin, vmax, vsum, imin, imax, miss, mask, stat)
  implicit none
  integer, parameter :: byte = 1
  integer(byte), intent(in)            :: dat(:)
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin, imax
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:)
  integer      , intent(out), optional :: stat  ! status
  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_, imax_
  integer(8)    :: nx, ix
  logical       :: found

  call echo(code%bgn, 'get_stats__MP__get_stats_int1_1d', '-p')
  !-------------------------------------------------------------
  if( present(stat) ) stat = 0

  nx = size(dat,kind=8)
  if( present(mask) )then
    if( size(mask,kind=8) /= nx )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  size(mask) /= size(dat)')
    endif
  endif

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_1
    vmax_ = 0_1
  endif

  vsum_ = 0_8

  imin_ = 0_8
  imax_ = 0_8
  !-------------------------------------------------------------
  ! Case: Both missing value and mask were specified.
  if( present(miss) .and. present(mask) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss .and. mask(ix) )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( .not. found )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss .and. mask(ix) )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    endif
  !-------------------------------------------------------------
  ! Case: Only missing value was specified.
  elseif( present(miss) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( .not. found )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    endif
  !-------------------------------------------------------------
  ! Case: Only mask was specified.
  elseif( present(mask) )then
    if( .not. any(mask) )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      vmin_ = minval(dat,mask)
      vmax_ = maxval(dat,mask)
      imin_ = minloc(dat,1,mask)
      imax_ = maxloc(dat,1,mask)

      if( present(vsum) )then
        vsum_ = 0_8
        do ix = 1_8, nx
          if( mask(ix) ) vsum_ = vsum_ + int(dat(ix),kind=8)
        enddo
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Neither missing value or mask was specified.
  else
    vmin_ = minval(dat)
    vmax_ = maxval(dat)
    imin_ = minloc(dat,1)
    imax_ = maxloc(dat,1)

    if( present(vsum) )then
      vsum_ = 0_8
      do ix = 1_8, nx
        vsum_ = vsum_ + int(dat(ix),kind=8)
      enddo
    endif
  endif
  !-------------------------------------------------------------
  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin) ) imin = imin_
  if( present(imax) ) imax = imax_
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix)
  vmax_ = dat(ix)
  vsum_ = int(dat(ix),8)
  imin_ = ix
  imax_ = ix
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix) < vmin_ )then
    vmin_ = dat(ix)
    imin_ = ix
  endif

  if( dat(ix) > vmax_ )then
    vmax_ = dat(ix)
    imax_ = ix
  endif

  vsum_ = vsum_ + int(dat(ix),8)
end subroutine update_values
!---------------------------------------------------------------
end subroutine get_stats_int1_1d
!===============================================================
!
!===============================================================
subroutine get_stats_int2_1d(dat, vmin, vmax, vsum, imin, imax, miss, mask, stat)
  implicit none
  integer, parameter :: byte = 2
  integer(byte), intent(in)            :: dat(:)
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin, imax
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:)
  integer      , intent(out), optional :: stat  ! status
  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_, imax_
  integer(8)    :: nx, ix
  logical       :: found

  call echo(code%bgn, 'get_stats__MP__get_stats_int2_1d', '-p')
  !-------------------------------------------------------------
  if( present(stat) ) stat = 0

  nx = size(dat,kind=8)
  if( present(mask) )then
    if( size(mask,kind=8) /= nx )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  size(mask) /= size(dat)')
    endif
  endif

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_2
    vmax_ = 0_2
  endif

  vsum_ = 0_8

  imin_ = 0_8
  imax_ = 0_8
  !-------------------------------------------------------------
  ! Case: Both missing value and mask were specified.
  if( present(miss) .and. present(mask) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss .and. mask(ix) )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( .not. found )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss .and. mask(ix) )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    endif
  !-------------------------------------------------------------
  ! Case: Only missing value was specified.
  elseif( present(miss) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( .not. found )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    endif
  !-------------------------------------------------------------
  ! Case: Only mask was specified.
  elseif( present(mask) )then
    if( .not. any(mask) )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      vmin_ = minval(dat,mask)
      vmax_ = maxval(dat,mask)
      imin_ = minloc(dat,1,mask)
      imax_ = maxloc(dat,1,mask)

      if( present(vsum) )then
        vsum_ = 0_8
        do ix = 1_8, nx
          if( mask(ix) ) vsum_ = vsum_ + int(dat(ix),8)
        enddo
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Neither missing value or mask was specified.
  else
    vmin_ = minval(dat)
    vmax_ = maxval(dat)
    imin_ = minloc(dat,1)
    imax_ = maxloc(dat,1)

    if( present(vsum) )then
      vsum_ = 0_8
      do ix = 1_8, nx
        vsum_ = vsum_ + int(dat(ix),8)
      enddo
    endif
  endif
  !-------------------------------------------------------------
  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin) ) imin = imin_
  if( present(imax) ) imax = imax_
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix)
  vmax_ = dat(ix)
  vsum_ = int(dat(ix),8)
  imin_ = ix
  imax_ = ix
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix) < vmin_ )then
    vmin_ = dat(ix)
    imin_ = ix
  endif

  if( dat(ix) > vmax_ )then
    vmax_ = dat(ix)
    imax_ = ix
  endif

  vsum_ = vsum_ + int(dat(ix),8)
end subroutine update_values
!---------------------------------------------------------------
end subroutine get_stats_int2_1d
!===============================================================
!
!===============================================================
subroutine get_stats_int4_1d(dat, vmin, vmax, vsum, imin, imax, miss, mask, stat)
  implicit none
  integer, parameter :: byte = 4
  integer(byte), intent(in)            :: dat(:)
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin, imax
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:)
  integer      , intent(out), optional :: stat  ! status
  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_, imax_
  integer(8)    :: nx, ix
  logical       :: found

  call echo(code%bgn, 'get_stats__MP__get_stats_int4_1d', '-p')
  !-------------------------------------------------------------
  if( present(stat) ) stat = 0

  nx = size(dat,kind=8)
  if( present(mask) )then
    if( size(mask,kind=8) /= nx )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  size(mask) /= size(dat)')
    endif
  endif

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_1
    vmax_ = 0_1
  endif

  vsum_ = 0_8

  imin_ = 0_8
  imax_ = 0_8
  !-------------------------------------------------------------
  ! Case: Both missing value and mask were specified.
  if( present(miss) .and. present(mask) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss .and. mask(ix) )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( .not. found )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss .and. mask(ix) )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    endif
  !-------------------------------------------------------------
  ! Case: Only missing value was specified.
  elseif( present(miss) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( .not. found )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    endif
  !-------------------------------------------------------------
  ! Case: Only mask was specified.
  elseif( present(mask) )then
    if( .not. any(mask) )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      vmin_ = minval(dat,mask)
      vmax_ = maxval(dat,mask)
      imin_ = minloc(dat,1,mask)
      imax_ = maxloc(dat,1,mask)

      if( present(vsum) )then
        vsum_ = 0_8
        do ix = 1_8, nx
          if( mask(ix) ) vsum_ = vsum_ + int(dat(ix),8)
        enddo
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Neither missing value or mask was specified.
  else
    vmin_ = minval(dat)
    vmax_ = maxval(dat)
    imin_ = minloc(dat,1)
    imax_ = maxloc(dat,1)

    if( present(vsum) )then
      vsum_ = 0_8
      do ix = 1_8, nx
        vsum_ = vsum_ + int(dat(ix),8)
      enddo
    endif
  endif
  !-------------------------------------------------------------
  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin) ) imin = imin_
  if( present(imax) ) imax = imax_
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix)
  vmax_ = dat(ix)
  vsum_ = int(dat(ix),8)
  imin_ = ix
  imax_ = ix
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix) < vmin_ )then
    vmin_ = dat(ix)
    imin_ = ix
  endif

  if( dat(ix) > vmax_ )then
    vmax_ = dat(ix)
    imax_ = ix
  endif

  vsum_ = vsum_ + int(dat(ix),8)
end subroutine update_values
!---------------------------------------------------------------
end subroutine get_stats_int4_1d
!===============================================================
!
!===============================================================
subroutine get_stats_int8_1d(dat, vmin, vmax, vsum, imin, imax, miss, mask, stat)
  implicit none
  integer, parameter :: byte = 8
  integer(byte), intent(in)            :: dat(:)
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin, imax
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:)
  integer      , intent(out), optional :: stat  ! status
  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_, imax_
  integer(8)    :: nx, ix
  logical       :: found

  call echo(code%bgn, 'get_stats__MP__get_stats_int8_1d', '-p')
  !-------------------------------------------------------------
  if( present(stat) ) stat = 0

  nx = size(dat,kind=8)
  if( present(mask) )then
    if( size(mask,kind=8) /= nx )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  size(mask) /= size(dat)')
    endif
  endif

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_1
    vmax_ = 0_1
  endif

  vsum_ = 0_8

  imin_ = 0_8
  imax_ = 0_8
  !-------------------------------------------------------------
  ! Case: Both missing value and mask were specified.
  if( present(miss) .and. present(mask) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss .and. mask(ix) )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( .not. found )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss .and. mask(ix) )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    endif
  !-------------------------------------------------------------
  ! Case: Only missing value was specified.
  elseif( present(miss) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( .not. found )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    endif
  !-------------------------------------------------------------
  ! Case: Only mask was specified.
  elseif( present(mask) )then
    if( .not. any(mask) )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      vmin_ = minval(dat,mask)
      vmax_ = maxval(dat,mask)
      imin_ = minloc(dat,1,mask)
      imax_ = maxloc(dat,1,mask)

      if( present(vsum) )then
        vsum_ = 0_8
        do ix = 1_8, nx
          if( mask(ix) ) vsum_ = vsum_ + int(dat(ix),8)
        enddo
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Neither missing value or mask was specified.
  else
    vmin_ = minval(dat)
    vmax_ = maxval(dat)
    imin_ = minloc(dat,1)
    imax_ = maxloc(dat,1)

    if( present(vsum) )then
      vsum_ = 0_8
      do ix = 1_8, nx
        vsum_ = vsum_ + int(dat(ix),8)
      enddo
    endif
  endif
  !-------------------------------------------------------------
  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin) ) imin = imin_
  if( present(imax) ) imax = imax_
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix)
  vmax_ = dat(ix)
  vsum_ = int(dat(ix),kind=8)
  imin_ = ix
  imax_ = ix
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix) < vmin_ )then
    vmin_ = dat(ix)
    imin_ = ix
  endif

  if( dat(ix) > vmax_ )then
    vmax_ = dat(ix)
    imax_ = ix
  endif

  vsum_ = vsum_ + int(dat(ix),8)
end subroutine update_values
!---------------------------------------------------------------
end subroutine get_stats_int8_1d
!===============================================================
!
!===============================================================
subroutine get_stats_real_1d(dat, vmin, vmax, vsum, imin, imax, miss, mask, stat)
  implicit none
  integer, parameter :: byte = 4
  real(byte), intent(in)            :: dat(:)
  real(byte), intent(out), optional :: vmin, vmax
  real(8)   , intent(out), optional :: vsum
  integer(8), intent(out), optional :: imin, imax
  real(byte), intent(in) , optional :: miss
  logical   , intent(in) , optional :: mask(:)
  integer   , intent(out), optional :: stat  ! status
  real(byte) :: vmin_, vmax_
  real(8)    :: vsum_
  integer(8) :: imin_, imax_
  integer(8) :: nx, ix
  logical    :: found

  call echo(code%bgn, 'get_stats__MP__get_stats_real_1d', '-p')
  !-------------------------------------------------------------
  if( present(stat) ) stat = 0

  nx = size(dat,kind=8)
  if( present(mask) )then
    if( size(mask,kind=8) /= nx )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  size(mask) /= size(dat)')
    endif
  endif

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0.0
    vmax_ = 0.0
  endif

  vsum_ = 0.d0

  imin_ = 0_8
  imax_ = 0_8
  !-------------------------------------------------------------
  ! Case: Both missing value and mask were specified.
  if( present(miss) .and. present(mask) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss .and. mask(ix) )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( .not. found )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss .and. mask(ix) )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    endif
  !-------------------------------------------------------------
  ! Case: Only missing value was specified.
  elseif( present(miss) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( .not. found )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    endif
  !-------------------------------------------------------------
  ! Case: Only mask was specified.
  elseif( present(mask) )then
    if( .not. any(mask) )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      vmin_ = minval(dat,mask)
      vmax_ = maxval(dat,mask)
      imin_ = minloc(dat,1,mask)
      imax_ = maxloc(dat,1,mask)

      if( present(vsum) )then
        vsum_ = 0.d0
        do ix = 1_8, nx
          if( mask(ix) ) vsum_ = vsum_ + real(dat(ix),8)
        enddo
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Neither missing value or mask was specified.
  else
    vmin_ = minval(dat)
    vmax_ = maxval(dat)
    imin_ = minloc(dat,1)
    imax_ = maxloc(dat,1)
    if( present(vsum) )then
      vsum_ = 0.d0
      do ix = 1_8, nx
        vsum_ = vsum_ + real(dat(ix),8)
      enddo
    endif
  endif
  !-------------------------------------------------------------
  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin) ) imin = imin_
  if( present(imax) ) imax = imax_
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix)
  vmax_ = dat(ix)
  vsum_ = real(dat(ix),8)
  imin_ = ix
  imax_ = ix
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix) < vmin_ )then
    vmin_ = dat(ix)
    imin_ = ix
  endif

  if( dat(ix) > vmax_ )then
    vmax_ = dat(ix)
    imax_ = ix
  endif

  vsum_ = vsum_ + real(dat(ix),8)
end subroutine update_values
!---------------------------------------------------------------
end subroutine get_stats_real_1d
!===============================================================
!
!===============================================================
subroutine get_stats_dble_1d(dat, vmin, vmax, vsum, imin, imax, miss, mask, stat)
  implicit none
  integer, parameter :: byte = 8
  real(byte), intent(in)            :: dat(:)
  real(byte), intent(out), optional :: vmin, vmax
  real(8)   , intent(out), optional :: vsum
  integer(8), intent(out), optional :: imin, imax
  real(byte), intent(in) , optional :: miss
  logical   , intent(in) , optional :: mask(:)
  integer   , intent(out), optional :: stat  ! status
  real(byte) :: vmin_, vmax_
  real(8)    :: vsum_
  integer(8) :: imin_, imax_
  integer(8) :: nx, ix
  logical    :: found

  call echo(code%bgn, 'get_stats__MP__get_stats_dble_1d', '-p')
  !-------------------------------------------------------------
  if( present(stat) ) stat = 0

  nx = size(dat,kind=8)
  if( present(mask) )then
    if( size(mask,kind=8) /= nx )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  size(mask) /= size(dat)')
    endif
  endif

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0.d0
    vmax_ = 0.d0
  endif

  vsum_ = 0.d0

  imin_ = 0_8
  imax_ = 0_8
  !-------------------------------------------------------------
  ! Case: Both missing value and mask were specified.
  if( present(miss) .and. present(mask) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss .and. mask(ix) )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( .not. found )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss .and. mask(ix) )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    endif
  !-------------------------------------------------------------
  ! Case: Only missing value was specified.
  elseif( present(miss) )then
    found = .false.
    ix = 1_8
    do while( ix <= nx )
      if( dat(ix) /= miss )then
        call put_values()
        found = .true.
        exit
      endif
      ix = ix + 1_8
    enddo

    if( .not. found )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      ix = ix + 1_8
      do while( ix <= nx )
        if( dat(ix) /= miss )then
          call update_values()
        endif
        ix = ix + 1_8
      enddo
    endif
  !-------------------------------------------------------------
  ! Case: Only mask was specified.
  elseif( present(mask) )then
    if( .not. any(mask) )then
      if( present(stat) )then
        stat = 1
      else
        call eerr('No valid value was found.')
      endif
    else
      vmin_ = minval(dat,mask)
      vmax_ = maxval(dat,mask)
      imin_ = minloc(dat,1,mask)
      imax_ = maxloc(dat,1,mask)

      if( present(vsum) )then
        vsum_ = sum(dat,mask)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Neither missing value or mask was specified.
  else
    vmin_ = minval(dat)
    vmax_ = maxval(dat)
    imin_ = minloc(dat,1)
    imax_ = maxloc(dat,1)

    if( present(vsum) )then
      vsum_ = sum(dat)
    endif
  endif
  !-------------------------------------------------------------
  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin) ) imin = imin_
  if( present(imax) ) imax = imax_
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix)
  vmax_ = dat(ix)
  vsum_ = real(dat(ix),8)
  imin_ = ix
  imax_ = ix
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix) < vmin_ )then
    vmin_ = dat(ix)
    imin_ = ix
  endif

  if( dat(ix) > vmax_ )then
    vmax_ = dat(ix)
    imax_ = ix
  endif

  vsum_ = vsum_ + real(dat(ix),8)
end subroutine update_values
!---------------------------------------------------------------
end subroutine get_stats_dble_1d
!===============================================================
!
!===============================================================
subroutine get_stats_int1_2d(dat, vmin, vmax, vsum, imin1, imax1, imin2, imax2, miss, mask, stat)
  implicit none
  integer, parameter :: byte = 1
  integer(byte), intent(in)            :: dat(:,:)
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin1, imax1, imin2, imax2
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:,:)
  integer      , intent(out), optional :: stat  ! status
  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_(2), imax_(2)
  integer(8)    :: nx, ix, ny, iy
  logical       :: found

  call echo(code%bgn, 'get_stats__MP__get_stats_int1_2d', '-p')
  !-------------------------------------------------------------
  if( present(stat) ) stat = 0

  nx = size(dat,1,kind=8)
  ny = size(dat,2,kind=8)
  if( present(mask) )then
    if( any(shape(mask) /= shape(dat)) )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  shape(mask) /= shape(dat)')
    endif
  endif

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_1
    vmax_ = 0_1
  endif

  vsum_ = 0_8

  imin_(:) = 0_8
  imax_(:) = 0_8
  !-------------------------------------------------------------
  ! Case 1: Missing value and mask was specified
  if( present(miss) .and. present(mask) )then
    found = .false.
    iy = 1_8
    loop_init_case1: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
          found = .true.
          call put_values()
          exit loop_init_case1
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case1

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      call no_valid_value()
    endif
  !-------------------------------------------------------------
  ! Case 2: Missing value was specified
  elseif( present(miss) )then
    found = .false.
    iy = 1_8
    loop_init_case2: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss )then
          found = .true.
          call put_values()
          exit loop_init_case2
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case2

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      call no_valid_value()
    endif
  !-------------------------------------------------------------
  ! Case 3: Mask was specified
  elseif( present(mask) )then
    if( .not. any(mask) )then
      call no_valid_value()
    else
      vmin_ = minval(dat,mask=mask)
      vmax_ = maxval(dat,mask=mask)
      vsum_ = sum(dat,mask=mask)
      imin_ = minloc(dat,mask=mask)
      imax_ = maxloc(dat,mask=mask)
    endif
  !-------------------------------------------------------------
  ! Case 4: Neither missing value or mask was specified.
  else
    vmin_ = minval(dat)
    vmax_ = maxval(dat)
    vsum_ = sum(dat)
    imin_ = minloc(dat)
    imax_ = maxloc(dat)
  endif
  !-------------------------------------------------------------
  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin1) ) imin1 = imin_(1)
  if( present(imin2) ) imin2 = imin_(2)
  if( present(imax1) ) imax1 = imax_(1)
  if( present(imax2) ) imax2 = imax_(2)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix,iy)
  vmax_ = dat(ix,iy)
  vsum_ = dat(ix,iy)
  imin_(1) = ix
  imin_(2) = iy
  imax_(1) = ix
  imax_(2) = iy
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix,iy) < vmin_ )then
    vmin_ = dat(ix,iy)
    imin_(1) = ix
    imin_(2) = iy
  elseif( dat(ix,iy) > vmax_ )then
    vmax_ = dat(ix,iy)
    imax_(1) = ix
    imax_(2) = iy
  endif

  vsum_ = vsum_ + dat(ix,iy)
end subroutine update_values
!---------------------------------------------------------------
subroutine no_valid_value()
  implicit none

  if( present(stat) )then
    stat = 1
    vmin_ = miss
    vmax_ = miss
  else
    call eerr(str(msg_unexpected_condition())//&
            '\nValid value was not found.')
  endif
end subroutine no_valid_value
!---------------------------------------------------------------
end subroutine get_stats_int1_2d
!===============================================================
!
!===============================================================
subroutine get_stats_int2_2d(dat, vmin, vmax, vsum, imin1, imax1, imin2, imax2, miss, mask, stat)
  implicit none
  integer, parameter :: byte = 2
  integer(byte), intent(in)            :: dat(:,:)
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin1, imax1, imin2, imax2
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:,:)
  integer      , intent(out), optional :: stat  ! status
  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_(2), imax_(2)
  integer(8)    :: nx, ix, ny, iy
  logical       :: found

  call echo(code%bgn, 'get_stats__MP__get_stats_int2_2d', '-p')
  !-------------------------------------------------------------
  if( present(stat) ) stat = 0

  nx = size(dat,1,kind=8)
  ny = size(dat,2,kind=8)
  if( present(mask) )then
    if( any(shape(mask) /= shape(dat)) )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  shape(mask) /= shape(dat)')
    endif
  endif

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_2
    vmax_ = 0_2
  endif

  vsum_ = 0_8

  imin_(:) = 0_8
  imax_(:) = 0_8
  !-------------------------------------------------------------
  ! Case 1: Missing value and mask was specified
  if( present(miss) .and. present(mask) )then
    found = .false.
    iy = 1_8
    loop_init_case1: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
          found = .true.
          call put_values()
          exit loop_init_case1
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case1

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      call no_valid_value()
    endif
  !-------------------------------------------------------------
  ! Case 2: Missing value was specified
  elseif( present(miss) )then
    found = .false.
    iy = 1_8
    loop_init_case2: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss )then
          found = .true.
          call put_values()
          exit loop_init_case2
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case2

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      call no_valid_value()
    endif
  !-------------------------------------------------------------
  ! Case 3: Mask was specified
  elseif( present(mask) )then
    if( .not. any(mask) )then
      call no_valid_value()
    else
      vmin_ = minval(dat,mask=mask)
      vmax_ = maxval(dat,mask=mask)
      vsum_ = sum(dat,mask=mask)
      imin_ = minloc(dat,mask=mask)
      imax_ = maxloc(dat,mask=mask)
    endif
  !-------------------------------------------------------------
  ! Case 4: Neither missing value or mask was specified.
  else
    vmin_ = minval(dat)
    vmax_ = maxval(dat)
    vsum_ = sum(dat)
    imin_ = minloc(dat)
    imax_ = maxloc(dat)
  endif
  !-------------------------------------------------------------
  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin1) ) imin1 = imin_(1)
  if( present(imin2) ) imin2 = imin_(2)
  if( present(imax1) ) imax1 = imax_(1)
  if( present(imax2) ) imax2 = imax_(2)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix,iy)
  vmax_ = dat(ix,iy)
  vsum_ = dat(ix,iy)
  imin_(1) = ix
  imin_(2) = iy
  imax_(1) = ix
  imax_(2) = iy
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix,iy) < vmin_ )then
    vmin_ = dat(ix,iy)
    imin_(1) = ix
    imin_(2) = iy
  elseif( dat(ix,iy) > vmax_ )then
    vmax_ = dat(ix,iy)
    imax_(1) = ix
    imax_(2) = iy
  endif

  vsum_ = vsum_ + dat(ix,iy)
end subroutine update_values
!---------------------------------------------------------------
subroutine no_valid_value()
  implicit none

  if( present(stat) )then
    stat = 1
    vmin_ = miss
    vmax_ = miss
  else
    call eerr(str(msg_unexpected_condition())//&
            '\nValid value was not found.')
  endif
end subroutine no_valid_value
!---------------------------------------------------------------
end subroutine get_stats_int2_2d
!===============================================================
!
!===============================================================
subroutine get_stats_int4_2d(dat, vmin, vmax, vsum, imin1, imax1, imin2, imax2, miss, mask, stat)
  implicit none
  integer, parameter :: byte = 4
  integer(byte), intent(in)            :: dat(:,:)
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin1, imax1, imin2, imax2
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:,:)
  integer      , intent(out), optional :: stat  ! status
  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_(2), imax_(2)
  integer(8)    :: nx, ix, ny, iy
  logical       :: found

  call echo(code%bgn, 'get_stats__MP__get_stats_int4_2d', '-p')
  !-------------------------------------------------------------
  if( present(stat) ) stat = 0

  nx = size(dat,1,kind=8)
  ny = size(dat,2,kind=8)
  if( present(mask) )then
    if( any(shape(mask) /= shape(dat)) )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  shape(mask) /= shape(dat)')
    endif
  endif

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_4
    vmax_ = 0_4
  endif

  vsum_ = 0_8

  imin_(:) = 0_8
  imax_(:) = 0_8
  !-------------------------------------------------------------
  ! Case 1: Missing value and mask was specified
  if( present(miss) .and. present(mask) )then
    found = .false.
    iy = 1_8
    loop_init_case1: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
          found = .true.
          call put_values()
          exit loop_init_case1
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case1

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      call no_valid_value()
    endif
  !-------------------------------------------------------------
  ! Case 2: Missing value was specified
  elseif( present(miss) )then
    found = .false.
    iy = 1_8
    loop_init_case2: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss )then
          found = .true.
          call put_values()
          exit loop_init_case2
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case2

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      call no_valid_value()
    endif
  !-------------------------------------------------------------
  ! Case 3: Mask was specified
  elseif( present(mask) )then
    if( .not. any(mask) )then
      call no_valid_value()
    else
      vmin_ = minval(dat,mask=mask)
      vmax_ = maxval(dat,mask=mask)
      vsum_ = sum(dat,mask=mask)
      imin_ = minloc(dat,mask=mask)
      imax_ = maxloc(dat,mask=mask)
    endif
  !-------------------------------------------------------------
  ! Case 4: Neither missing value or mask was specified.
  else
    vmin_ = minval(dat)
    vmax_ = maxval(dat)
    vsum_ = sum(dat)
    imin_ = minloc(dat)
    imax_ = maxloc(dat)
  endif
  !-------------------------------------------------------------
  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin1) ) imin1 = imin_(1)
  if( present(imin2) ) imin2 = imin_(2)
  if( present(imax1) ) imax1 = imax_(1)
  if( present(imax2) ) imax2 = imax_(2)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix,iy)
  vmax_ = dat(ix,iy)
  vsum_ = dat(ix,iy)
  imin_(1) = ix
  imin_(2) = iy
  imax_(1) = ix
  imax_(2) = iy
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix,iy) < vmin_ )then
    vmin_ = dat(ix,iy)
    imin_(1) = ix
    imin_(2) = iy
  elseif( dat(ix,iy) > vmax_ )then
    vmax_ = dat(ix,iy)
    imax_(1) = ix
    imax_(2) = iy
  endif

  vsum_ = vsum_ + dat(ix,iy)
end subroutine update_values
!---------------------------------------------------------------
subroutine no_valid_value()
  implicit none

  if( present(stat) )then
    stat = 1
    vmin_ = miss
    vmax_ = miss
  else
    call eerr(str(msg_unexpected_condition())//&
            '\nValid value was not found.')
  endif
end subroutine no_valid_value
!---------------------------------------------------------------
end subroutine get_stats_int4_2d
!===============================================================
!
!===============================================================
subroutine get_stats_int8_2d(dat, vmin, vmax, vsum, imin1, imax1, imin2, imax2, miss, mask, stat)
  implicit none
  integer, parameter :: byte = 8
  integer(byte), intent(in)            :: dat(:,:)
  integer(byte), intent(out), optional :: vmin, vmax
  integer(8)   , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin1, imax1, imin2, imax2
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:,:)
  integer      , intent(out), optional :: stat  ! status
  integer(byte) :: vmin_, vmax_
  integer(8)    :: vsum_
  integer(8)    :: imin_(2), imax_(2)
  integer(8)    :: nx, ix, ny, iy
  logical       :: found

  call echo(code%bgn, 'get_stats__MP__get_stats_int8_2d', '-p')
  !-------------------------------------------------------------
  if( present(stat) ) stat = 0

  nx = size(dat,1,kind=8)
  ny = size(dat,2,kind=8)
  if( present(mask) )then
    if( any(shape(mask) /= shape(dat)) )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  shape(mask) /= shape(dat)')
    endif
  endif

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0_8
    vmax_ = 0_8
  endif

  vsum_ = 0_8

  imin_(:) = 0_8
  imax_(:) = 0_8
  !-------------------------------------------------------------
  ! Case 1: Missing value and mask was specified
  if( present(miss) .and. present(mask) )then
    found = .false.
    iy = 1_8
    loop_init_case1: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
          found = .true.
          call put_values()
          exit loop_init_case1
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case1

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      call no_valid_value()
    endif
  !-------------------------------------------------------------
  ! Case 2: Missing value was specified
  elseif( present(miss) )then
    found = .false.
    iy = 1_8
    loop_init_case2: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss )then
          found = .true.
          call put_values()
          exit loop_init_case2
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case2

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      call no_valid_value()
    endif
  !-------------------------------------------------------------
  ! Case 3: Mask was specified
  elseif( present(mask) )then
    if( .not. any(mask) )then
      call no_valid_value()
    else
      vmin_ = minval(dat,mask=mask)
      vmax_ = maxval(dat,mask=mask)
      vsum_ = sum(dat,mask=mask)
      imin_ = minloc(dat,mask=mask)
      imax_ = maxloc(dat,mask=mask)
    endif
  !-------------------------------------------------------------
  ! Case 4: Neither missing value or mask was specified.
  else
    vmin_ = minval(dat)
    vmax_ = maxval(dat)
    vsum_ = sum(dat)
    imin_ = minloc(dat)
    imax_ = maxloc(dat)
  endif
  !-------------------------------------------------------------
  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin1) ) imin1 = imin_(1)
  if( present(imin2) ) imin2 = imin_(2)
  if( present(imax1) ) imax1 = imax_(1)
  if( present(imax2) ) imax2 = imax_(2)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix,iy)
  vmax_ = dat(ix,iy)
  vsum_ = dat(ix,iy)
  imin_(1) = ix
  imin_(2) = iy
  imax_(1) = ix
  imax_(2) = iy
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix,iy) < vmin_ )then
    vmin_ = dat(ix,iy)
    imin_(1) = ix
    imin_(2) = iy
  elseif( dat(ix,iy) > vmax_ )then
    vmax_ = dat(ix,iy)
    imax_(1) = ix
    imax_(2) = iy
  endif

  vsum_ = vsum_ + dat(ix,iy)
end subroutine update_values
!---------------------------------------------------------------
subroutine no_valid_value()
  implicit none

  if( present(stat) )then
    stat = 1
    vmin_ = miss
    vmax_ = miss
  else
    call eerr(str(msg_unexpected_condition())//&
            '\nValid value was not found.')
  endif
end subroutine no_valid_value
!---------------------------------------------------------------
end subroutine get_stats_int8_2d
!===============================================================
!
!===============================================================
subroutine get_stats_real_2d(dat, vmin, vmax, vsum, imin1, imax1, imin2, imax2, miss, mask, stat)
  implicit none
  integer, parameter :: byte = 4
  real(byte)   , intent(in)            :: dat(:,:)
  real(byte)   , intent(out), optional :: vmin, vmax
  real(8)      , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin1, imax1, imin2, imax2
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:,:)
  integer      , intent(out), optional :: stat  ! status
  real(byte) :: vmin_, vmax_
  real(8)    :: vsum_
  integer(8) :: imin_(2), imax_(2)
  integer(8) :: nx, ix, ny, iy
  logical    :: found

  call echo(code%bgn, 'get_stats__MP__get_stats_real_2d', '-p')
  !-------------------------------------------------------------
  if( present(stat) ) stat = 0

  nx = size(dat,1,kind=8)
  ny = size(dat,2,kind=8)
  if( present(mask) )then
    if( any(shape(mask) /= shape(dat)) )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  shape(mask) /= shape(dat)')
    endif
  endif

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0.0
    vmax_ = 0.0
  endif

  vsum_ = 0.d0

  imin_(:) = 0_8
  imax_(:) = 0_8
  !-------------------------------------------------------------
  ! Case 1: Missing value and mask was specified
  if( present(miss) .and. present(mask) )then
    found = .false.
    iy = 1_8
    loop_init_case1: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
          found = .true.
          call put_values()
          exit loop_init_case1
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case1

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      call no_valid_value()
    endif
  !-------------------------------------------------------------
  ! Case 2: Missing value was specified
  elseif( present(miss) )then
    found = .false.
    iy = 1_8
    loop_init_case2: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss )then
          found = .true.
          call put_values()
          exit loop_init_case2
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case2

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      call no_valid_value()
    endif
  !-------------------------------------------------------------
  ! Case 3: Mask was specified
  elseif( present(mask) )then
    if( .not. any(mask) )then
      call no_valid_value()
    else
      vmin_ = minval(dat,mask=mask)
      vmax_ = maxval(dat,mask=mask)
      vsum_ = sum(dat,mask=mask)
      imin_ = minloc(dat,mask=mask)
      imax_ = maxloc(dat,mask=mask)
    endif
  !-------------------------------------------------------------
  ! Case 4: Neither missing value or mask was specified.
  else
    vmin_ = minval(dat)
    vmax_ = maxval(dat)
    vsum_ = sum(dat)
    imin_ = minloc(dat)
    imax_ = maxloc(dat)
  endif
  !-------------------------------------------------------------
  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin1) ) imin1 = imin_(1)
  if( present(imin2) ) imin2 = imin_(2)
  if( present(imax1) ) imax1 = imax_(1)
  if( present(imax2) ) imax2 = imax_(2)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix,iy)
  vmax_ = dat(ix,iy)
  vsum_ = dat(ix,iy)
  imin_(1) = ix
  imin_(2) = iy
  imax_(1) = ix
  imax_(2) = iy
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix,iy) < vmin_ )then
    vmin_ = dat(ix,iy)
    imin_(1) = ix
    imin_(2) = iy
  elseif( dat(ix,iy) > vmax_ )then
    vmax_ = dat(ix,iy)
    imax_(1) = ix
    imax_(2) = iy
  endif

  vsum_ = vsum_ + dat(ix,iy)
end subroutine update_values
!---------------------------------------------------------------
subroutine no_valid_value()
  implicit none

  if( present(stat) )then
    stat = 1
    vmin_ = miss
    vmax_ = miss
  else
    call eerr(str(msg_unexpected_condition())//&
            '\nValid value was not found.')
  endif
end subroutine no_valid_value
!---------------------------------------------------------------
end subroutine get_stats_real_2d
!===============================================================
!
!===============================================================
subroutine get_stats_dble_2d(dat, vmin, vmax, vsum, imin1, imax1, imin2, imax2, miss, mask, stat)
  implicit none
  integer, parameter :: byte = 8
  real(byte)   , intent(in)            :: dat(:,:)
  real(byte)   , intent(out), optional :: vmin, vmax
  real(8)      , intent(out), optional :: vsum
  integer(8)   , intent(out), optional :: imin1, imax1, imin2, imax2
  integer(byte), intent(in) , optional :: miss
  logical      , intent(in) , optional :: mask(:,:)
  integer      , intent(out), optional :: stat  ! status
  real(byte) :: vmin_, vmax_
  real(8)    :: vsum_
  integer(8) :: imin_(2), imax_(2)
  integer(8) :: nx, ix, ny, iy
  logical    :: found

  call echo(code%bgn, 'get_stats__MP__get_stats_dble_2d', '-p')
  !-------------------------------------------------------------
  if( present(stat) ) stat = 0

  nx = size(dat,1,kind=8)
  ny = size(dat,2,kind=8)
  if( present(mask) )then
    if( any(shape(mask) /= shape(dat)) )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  shape(mask) /= shape(dat)')
    endif
  endif

  if( present(miss) )then
    vmin_ = miss
    vmax_ = miss
  else
    vmin_ = 0.d0
    vmax_ = 0.d0
  endif

  vsum_ = 0.d0

  imin_(:) = 0_8
  imax_(:) = 0_8
  !-------------------------------------------------------------
  ! Case 1: Missing value and mask was specified
  if( present(miss) .and. present(mask) )then
    found = .false.
    iy = 1_8
    loop_init_case1: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
          found = .true.
          call put_values()
          exit loop_init_case1
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case1

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss .and. mask(ix,iy) )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      call no_valid_value()
    endif
  !-------------------------------------------------------------
  ! Case 2: Missing value was specified
  elseif( present(miss) )then
    found = .false.
    iy = 1_8
    loop_init_case2: &
    do while( iy <= ny )
      ix = 1_8
      do while( ix <= nx )
        if( dat(ix,iy) /= miss )then
          found = .true.
          call put_values()
          exit loop_init_case2
        endif
        ix = ix + 1_8
      enddo
      iy = iy + 1_8
    enddo loop_init_case2

    if( found )then
      do while( iy <= ny )
        ix = 1_8
        do while( ix <= nx )
          if( dat(ix,iy) /= miss )then
            call update_values()
          endif
          ix = ix + 1_8
        enddo
        iy = iy + 1_8
      enddo
    else
      call no_valid_value()
    endif
  !-------------------------------------------------------------
  ! Case 3: Mask was specified
  elseif( present(mask) )then
    if( .not. any(mask) )then
      call no_valid_value()
    else
      vmin_ = minval(dat,mask=mask)
      vmax_ = maxval(dat,mask=mask)
      vsum_ = sum(dat,mask=mask)
      imin_ = minloc(dat,mask=mask)
      imax_ = maxloc(dat,mask=mask)
    endif
  !-------------------------------------------------------------
  ! Case 4: Neither missing value or mask was specified.
  else
    vmin_ = minval(dat)
    vmax_ = maxval(dat)
    vsum_ = sum(dat)
    imin_ = minloc(dat)
    imax_ = maxloc(dat)
  endif
  !-------------------------------------------------------------
  if( present(vmin) ) vmin = vmin_
  if( present(vmax) ) vmax = vmax_
  if( present(vsum) ) vsum = vsum_
  if( present(imin1) ) imin1 = imin_(1)
  if( present(imin2) ) imin2 = imin_(2)
  if( present(imax1) ) imax1 = imax_(1)
  if( present(imax2) ) imax2 = imax_(2)
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine put_values()
  implicit none

  vmin_ = dat(ix,iy)
  vmax_ = dat(ix,iy)
  vsum_ = dat(ix,iy)
  imin_(1) = ix
  imin_(2) = iy
  imax_(1) = ix
  imax_(2) = iy
end subroutine put_values
!---------------------------------------------------------------
subroutine update_values()
  implicit none

  if( dat(ix,iy) < vmin_ )then
    vmin_ = dat(ix,iy)
    imin_(1) = ix
    imin_(2) = iy
  elseif( dat(ix,iy) > vmax_ )then
    vmax_ = dat(ix,iy)
    imax_(1) = ix
    imax_(2) = iy
  endif

  vsum_ = vsum_ + dat(ix,iy)
end subroutine update_values
!---------------------------------------------------------------
subroutine no_valid_value()
  implicit none

  if( present(stat) )then
    stat = 1
    vmin_ = miss
    vmax_ = miss
  else
    call eerr(str(msg_unexpected_condition())//&
            '\nValid value was not found.')
  endif
end subroutine no_valid_value
!---------------------------------------------------------------
end subroutine get_stats_dble_2d
!===============================================================
!
!===============================================================
end module lib_math_stats
