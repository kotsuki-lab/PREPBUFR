module lib_stats_basic
  use lib_const
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: irange

  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  interface irange
    module procedure irange__int1_1d_loc4
    module procedure irange__int2_1d_loc4
    module procedure irange__int4_1d_loc4
    module procedure irange__int8_1d_loc4
    module procedure irange__real_1d_loc4
    module procedure irange__dble_1d_loc4
    module procedure irange__int1_1d_loc8
    module procedure irange__int2_1d_loc8
    module procedure irange__int4_1d_loc8
    module procedure irange__int8_1d_loc8
    module procedure irange__real_1d_loc8
    module procedure irange__dble_1d_loc8
  end interface
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  integer, parameter :: CODE_IRANGE_OK   = 0
  integer, parameter :: CODE_IRANGE_NONE = 1
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
integer function irange__int1_1d_loc4(&
    x, vmin, vmax, imin, imax, &
    mask, miss) result(iret)
  integer(1), intent(in) :: x(:)
  integer(1), intent(out) :: vmin, vmax
  integer(4), intent(out), optional :: imin, imax
  logical   , intent(in) , optional :: mask(:)
  integer(1), intent(in) , optional :: miss

  integer(8) :: imin8, imax8

  iret = CODE_IRANGE_OK

  if( present(mask) .and. present(miss) )then
    iret = irange__int1_1d_loc8(x, vmin, vmax, imin8, imax8, mask, miss)
  elseif( present(mask) )then
    iret = irange__int1_1d_loc8(x, vmin, vmax, imin8, imax8, mask)
  elseif( present(miss) )then
    iret = irange__int1_1d_loc8(x, vmin, vmax, imin8, imax8, miss=miss)
  else
    iret = irange__int1_1d_loc8(x, vmin, vmax, imin8, imax8)
  endif

  if( present(imin) ) imin = int(imin8,4)
  if( present(imax) ) imax = int(imax8,4)
end function irange__int1_1d_loc4
!===============================================================
!
!===============================================================
integer function irange__int2_1d_loc4(&
    x, vmin, vmax, imin, imax, &
    mask, miss) result(iret)
  integer(2), intent(in) :: x(:)
  integer(2), intent(out) :: vmin, vmax
  integer(4), intent(out), optional :: imin, imax
  logical   , intent(in) , optional :: mask(:)
  integer(2), intent(in) , optional :: miss

  integer(8) :: imin8, imax8

  iret = CODE_IRANGE_OK

  if( present(mask) .and. present(miss) )then
    iret = irange__int2_1d_loc8(x, vmin, vmax, imin8, imax8, mask, miss)
  elseif( present(mask) )then
    iret = irange__int2_1d_loc8(x, vmin, vmax, imin8, imax8, mask)
  elseif( present(miss) )then
    iret = irange__int2_1d_loc8(x, vmin, vmax, imin8, imax8, miss=miss)
  else
    iret = irange__int2_1d_loc8(x, vmin, vmax, imin8, imax8)
  endif

  if( present(imin) ) imin = int(imin8,4)
  if( present(imax) ) imax = int(imax8,4)
end function irange__int2_1d_loc4
!===============================================================
!
!===============================================================
integer function irange__int4_1d_loc4(&
    x, vmin, vmax, imin, imax, &
    mask, miss) result(iret)
  integer(4), intent(in) :: x(:)
  integer(4), intent(out) :: vmin, vmax
  integer(4), intent(out), optional :: imin, imax
  logical   , intent(in) , optional :: mask(:)
  integer(4), intent(in) , optional :: miss

  integer(8) :: imin8, imax8

  iret = CODE_IRANGE_OK

  if( present(mask) .and. present(miss) )then
    iret = irange__int4_1d_loc8(x, vmin, vmax, imin8, imax8, mask, miss)
  elseif( present(mask) )then
    iret = irange__int4_1d_loc8(x, vmin, vmax, imin8, imax8, mask)
  elseif( present(miss) )then
    iret = irange__int4_1d_loc8(x, vmin, vmax, imin8, imax8, miss=miss)
  else
    iret = irange__int4_1d_loc8(x, vmin, vmax, imin8, imax8)
  endif

  if( present(imin) ) imin = int(imin8,4)
  if( present(imax) ) imax = int(imax8,4)
end function irange__int4_1d_loc4
!===============================================================
!
!===============================================================
integer function irange__int8_1d_loc4(&
    x, vmin, vmax, imin, imax, &
    mask, miss) result(iret)
  integer(8), intent(in) :: x(:)
  integer(8), intent(out) :: vmin, vmax
  integer(4), intent(out), optional :: imin, imax
  logical   , intent(in) , optional :: mask(:)
  integer(8), intent(in) , optional :: miss

  integer(8) :: imin8, imax8

  iret = CODE_IRANGE_OK

  if( present(mask) .and. present(miss) )then
    iret = irange__int8_1d_loc8(x, vmin, vmax, imin8, imax8, mask, miss)
  elseif( present(mask) )then
    iret = irange__int8_1d_loc8(x, vmin, vmax, imin8, imax8, mask)
  elseif( present(miss) )then
    iret = irange__int8_1d_loc8(x, vmin, vmax, imin8, imax8, miss=miss)
  else
    iret = irange__int8_1d_loc8(x, vmin, vmax, imin8, imax8)
  endif

  if( present(imin) ) imin = int(imin8,4)
  if( present(imax) ) imax = int(imax8,4)
end function irange__int8_1d_loc4
!===============================================================
!
!===============================================================
integer function irange__real_1d_loc4(&
    x, vmin, vmax, imin, imax, &
    mask, miss) result(iret)
  real(4)   , intent(in) :: x(:)
  real(4)   , intent(out) :: vmin, vmax
  integer(4), intent(out), optional :: imin, imax
  logical   , intent(in) , optional :: mask(:)
  real(4)   , intent(in) , optional :: miss

  integer(8) :: imin8, imax8

  iret = CODE_IRANGE_OK

  if( present(mask) .and. present(miss) )then
    iret = irange__real_1d_loc8(x, vmin, vmax, imin8, imax8, mask, miss)
  elseif( present(mask) )then
    iret = irange__real_1d_loc8(x, vmin, vmax, imin8, imax8, mask)
  elseif( present(miss) )then
    iret = irange__real_1d_loc8(x, vmin, vmax, imin8, imax8, miss=miss)
  else
    iret = irange__real_1d_loc8(x, vmin, vmax, imin8, imax8)
  endif

  if( present(imin) ) imin = int(imin8,4)
  if( present(imax) ) imax = int(imax8,4)
end function irange__real_1d_loc4
!===============================================================
!
!===============================================================
integer function irange__dble_1d_loc4(&
    x, vmin, vmax, imin, imax, &
    mask, miss) result(iret)
  real(8)   , intent(in) :: x(:)
  real(8)   , intent(out) :: vmin, vmax
  integer(4), intent(out), optional :: imin, imax
  logical   , intent(in) , optional :: mask(:)
  real(8)   , intent(in) , optional :: miss

  integer(8) :: imin8, imax8

  iret = CODE_IRANGE_OK

  if( present(mask) .and. present(miss) )then
    iret = irange__dble_1d_loc8(x, vmin, vmax, imin8, imax8, mask, miss)
  elseif( present(mask) )then
    iret = irange__dble_1d_loc8(x, vmin, vmax, imin8, imax8, mask)
  elseif( present(miss) )then
    iret = irange__dble_1d_loc8(x, vmin, vmax, imin8, imax8, miss=miss)
  else
    iret = irange__dble_1d_loc8(x, vmin, vmax, imin8, imax8)
  endif

  if( present(imin) ) imin = int(imin8,4)
  if( present(imax) ) imax = int(imax8,4)
end function irange__dble_1d_loc4
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
integer function irange__int1_1d_loc8(&
    x, vmin, vmax, imin, imax, &
    mask, miss) result(iret)
  integer(1), intent(in)  :: x(:)
  integer(1), intent(out) :: vmin, vmax
  integer(8), intent(out) :: imin, imax
  logical   , intent(in), optional :: mask(:)
  integer(1), intent(in), optional :: miss

  iret = CODE_IRANGE_OK

  if( present(mask) .and. present(miss) )then
    if( .not. any(mask) .or. all(x==miss) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, mask.and.x/=miss, kind=8)
      imax = maxloc(x, 1, mask.and.x/=miss, kind=8)
    endif
  elseif( present(mask) )then
    if( .not. any(mask) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, mask, kind=8)
      imax = maxloc(x, 1, mask, kind=8)
    endif
  elseif( present(miss) )then
    if( all(x == miss) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, x/=miss, kind=8)
      imax = maxloc(x, 1, x/=miss, kind=8)
    endif
  else
    imin = minloc(x, 1, kind=8)
    imax = maxloc(x, 1, kind=8)
  endif

  if( iret == CODE_IRANGE_OK )then
    vmin = x(imin)
    vmax = x(imax)
  else
    vmin = 0_1
    vmax = 0_1
  endif
end function irange__int1_1d_loc8
!===============================================================
!
!===============================================================
integer function irange__int2_1d_loc8(&
    x, vmin, vmax, imin, imax, &
    mask, miss) result(iret)
  integer(2), intent(in)  :: x(:)
  integer(2), intent(out) :: vmin, vmax
  integer(8), intent(out) :: imin, imax
  logical   , intent(in), optional :: mask(:)
  integer(2), intent(in), optional :: miss

  iret = CODE_IRANGE_OK

  if( present(mask) .and. present(miss) )then
    if( .not. any(mask) .or. all(x==miss) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, mask.and.x/=miss, kind=8)
      imax = maxloc(x, 1, mask.and.x/=miss, kind=8)
    endif
  elseif( present(mask) )then
    if( .not. any(mask) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, mask, kind=8)
      imax = maxloc(x, 1, mask, kind=8)
    endif
  elseif( present(miss) )then
    if( all(x == miss) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, x/=miss, kind=8)
      imax = maxloc(x, 1, x/=miss, kind=8)
    endif
  else
    imin = minloc(x, 1, kind=8)
    imax = maxloc(x, 1, kind=8)
  endif

  if( iret == CODE_IRANGE_OK )then
    vmin = x(imin)
    vmax = x(imax)
  else
    vmin = 0_2
    vmax = 0_2
  endif
end function irange__int2_1d_loc8
!===============================================================
!
!===============================================================
integer function irange__int4_1d_loc8(&
    x, vmin, vmax, imin, imax, &
    mask, miss) result(iret)
  integer(4), intent(in)  :: x(:)
  integer(4), intent(out) :: vmin, vmax
  integer(8), intent(out) :: imin, imax
  logical   , intent(in), optional :: mask(:)
  integer(4), intent(in), optional :: miss

  iret = CODE_IRANGE_OK

  if( present(mask) .and. present(miss) )then
    if( .not. any(mask) .or. all(x==miss) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, mask.and.x/=miss, kind=8)
      imax = maxloc(x, 1, mask.and.x/=miss, kind=8)
    endif
  elseif( present(mask) )then
    if( .not. any(mask) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, mask, kind=8)
      imax = maxloc(x, 1, mask, kind=8)
    endif
  elseif( present(miss) )then
    if( all(x == miss) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, x/=miss, kind=8)
      imax = maxloc(x, 1, x/=miss, kind=8)
    endif
  else
    imin = minloc(x, 1, kind=8)
    imax = maxloc(x, 1, kind=8)
  endif

  if( iret == CODE_IRANGE_OK )then
    vmin = x(imin)
    vmax = x(imax)
  else
    vmin = 0_4
    vmax = 0_4
  endif
end function irange__int4_1d_loc8
!===============================================================
!
!===============================================================
integer function irange__int8_1d_loc8(&
    x, vmin, vmax, imin, imax, &
    mask, miss) result(iret)
  integer(8), intent(in)  :: x(:)
  integer(8), intent(out) :: vmin, vmax
  integer(8), intent(out) :: imin, imax
  logical   , intent(in), optional :: mask(:)
  integer(8), intent(in), optional :: miss

  iret = CODE_IRANGE_OK

  if( present(mask) .and. present(miss) )then
    if( .not. any(mask) .or. all(x==miss) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, mask.and.x/=miss, kind=8)
      imax = maxloc(x, 1, mask.and.x/=miss, kind=8)
    endif
  elseif( present(mask) )then
    if( .not. any(mask) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, mask, kind=8)
      imax = maxloc(x, 1, mask, kind=8)
    endif
  elseif( present(miss) )then
    if( all(x == miss) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, x/=miss, kind=8)
      imax = maxloc(x, 1, x/=miss, kind=8)
    endif
  else
    imin = minloc(x, 1, kind=8)
    imax = maxloc(x, 1, kind=8)
  endif

  if( iret == CODE_IRANGE_OK )then
    vmin = x(imin)
    vmax = x(imax)
  else
    vmin = 0_8
    vmax = 0_8
  endif
end function irange__int8_1d_loc8
!===============================================================
!
!===============================================================
integer function irange__real_1d_loc8(&
    x, vmin, vmax, imin, imax, &
    mask, miss) result(iret)
  real(4)   , intent(in)  :: x(:)
  real(4)   , intent(out) :: vmin, vmax
  integer(8), intent(out) :: imin, imax
  logical   , intent(in), optional :: mask(:)
  real(4)   , intent(in), optional :: miss

  iret = CODE_IRANGE_OK

  if( present(mask) .and. present(miss) )then
    if( .not. any(mask) .or. all(x==miss) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, mask.and.x/=miss, kind=8)
      imax = maxloc(x, 1, mask.and.x/=miss, kind=8)
    endif
  elseif( present(mask) )then
    if( .not. any(mask) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, mask, kind=8)
      imax = maxloc(x, 1, mask, kind=8)
    endif
  elseif( present(miss) )then
    if( all(x == miss) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, x/=miss, kind=8)
      imax = maxloc(x, 1, x/=miss, kind=8)
    endif
  else
    imin = minloc(x, 1, kind=8)
    imax = maxloc(x, 1, kind=8)
  endif

  if( iret == CODE_IRANGE_OK )then
    vmin = x(imin)
    vmax = x(imax)
  else
    vmin = 0.0
    vmax = 0.0
  endif
end function irange__real_1d_loc8
!===============================================================
!
!===============================================================
integer function irange__dble_1d_loc8(&
    x, vmin, vmax, imin, imax, &
    mask, miss) result(iret)
  real(8)   , intent(in)  :: x(:)
  real(8)   , intent(out) :: vmin, vmax
  integer(8), intent(out) :: imin, imax
  logical   , intent(in), optional :: mask(:)
  real(8)   , intent(in), optional :: miss

  iret = CODE_IRANGE_OK

  if( present(mask) .and. present(miss) )then
    if( .not. any(mask) .or. all(x==miss) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, mask.and.x/=miss, kind=8)
      imax = maxloc(x, 1, mask.and.x/=miss, kind=8)
    endif
  elseif( present(mask) )then
    if( .not. any(mask) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, mask, kind=8)
      imax = maxloc(x, 1, mask, kind=8)
    endif
  elseif( present(miss) )then
    if( all(x == miss) )then
      iret = CODE_IRANGE_NONE
    else
      imin = minloc(x, 1, x/=miss, kind=8)
      imax = maxloc(x, 1, x/=miss, kind=8)
    endif
  else
    imin = minloc(x, 1, kind=8)
    imax = maxloc(x, 1, kind=8)
  endif

  if( iret == CODE_IRANGE_OK )then
    vmin = x(imin)
    vmax = x(imax)
  else
    vmin = 0.d0
    vmax = 0.d0
  endif
end function irange__dble_1d_loc8
!===============================================================
!
!===============================================================
end module lib_stats_basic
