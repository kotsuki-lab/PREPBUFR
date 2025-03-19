module lib_time_base
  use lib_const, only: &
    CLEN_VAR, &
    SEC_DAY
  implicit none
  private
  !------------------------------------------------------------
  ! Public Procedures
  !------------------------------------------------------------
  public :: days
  public :: dayOfYear
  public :: second
  public :: timediff
  public :: date_and_time_values
  public :: str_datetime
  public :: float_datetime
  !------------------------------------------------------------
  ! Interfaces
  !------------------------------------------------------------
  interface days
    module procedure days_year
    module procedure days_month
  end interface

  interface second
    module procedure second_baseDefault
    module procedure second_baseSpecified
  end interface

  interface str_datetime
    module procedure str_datetime__all
    module procedure str_datetime__fmt
  end interface

  interface float_datetime
    module procedure float_datetime__all
    module procedure float_datetime__fmt
  end interface
  !------------------------------------------------------------
  ! Module Variables
  !------------------------------------------------------------
  integer, parameter :: year_base = 1900

  character(CLEN_VAR), parameter :: modname = 'lib_time_base'
!--------------------------------------------------------------
contains
!==============================================================
!
!==============================================================
integer function days_year(yr) result(days)
  implicit none
  integer, intent(in) :: yr

  days = 365
  if( mod(yr,4) == 0 )then
    days = 366
    if( mod(yr,100) == 0 )then
      days = 365
      if( mod(yr,400) == 0 )then
        days = 366
      endif
    endif
  endif
end function days_year
!==============================================================
!
!==============================================================
integer function days_month(yr, mn) result(days)
  implicit none
  integer, intent(in) :: yr, mn

  selectcase( mn )
  case( 2 )
    days = 28 + (days_year(yr) - 365)
  case( 4, 6, 9, 11 )
    days = 30
  case( 1, 3, 5, 7, 8, 10, 12 )
    days = 31
  case default
    days = 0
  endselect
end function days_month
!==============================================================
!
!==============================================================
integer function dayOfYear(yr, mn, dy)
  implicit none
  integer, intent(in) :: yr, mn, dy
  integer :: imn

  dayOfYear = dy
  do imn = 1, mn-1
    dayOfYear = dayOfYear + days(yr, imn)
  enddo
end function dayOfYear
!==============================================================
!
!==============================================================
real(8) function second_baseDefault(time) result(t)
  implicit none
  integer, intent(in) :: time(:)  !(yr,mn,dy,hr,mm,sc,ms)

  t = second_baseSpecified(time, YEAR_BASE)
end function second_baseDefault
!==============================================================
!
!==============================================================
real(8) function second_baseSpecified(time, yr0) result(t)
  implicit none
  integer, intent(in) :: time(:)  !(yr,mn,dy,timediff_from_UTC,hr,mm,sc,ms)
  integer, intent(in) :: yr0
  integer :: time_(8)
  integer :: d, yr

  time_(:) = (/0, 1, 1, 0, 0, 0, 0, 0/)  ! 0000/01/01 00:00:00.000 UTC
  time_(:size(time)) = time(:)

  d = 0
  do yr = yr0, time_(1)-1
    d = d + days(yr)
  enddo

  t = SEC_DAY * (d + dayOfYear(time_(1),time_(2),time_(3))) &
        + 60 * ((60 * time_(5)) + time_(6)) + time_(7) + time_(8)*1d-3
end function second_baseSpecified
!==============================================================
!
!==============================================================
real(8) function timediff(t0, t1)
  implicit none
  integer, intent(in) :: t0(:), t1(:)  !(yr,mn,dy,timediff_from_UTC,hr,mm,sc,ms)
  integer :: yr0

  yr0 = min(t0(1), t1(1))
  timediff = second(t1,yr0) - second(t0,yr0)
end function timediff
!==============================================================
!
!==============================================================
function date_and_time_values() result(values)
  implicit none
  character(8)  :: date
  character(10) :: time
  character(5)  :: zone
  integer       :: values(8)

  call date_and_time(date, time, zone, values)
end function date_and_time_values
!==============================================================
!
!==============================================================
function str_datetime__all() result(s)
  implicit none
  character(18) :: s

  character(8)  :: date
  character(10) :: time
  character(5)  :: zone
  integer       :: values(8)
  
  call date_and_time(date, time, zone, values)

  s = date//time
end function str_datetime__all
!==============================================================
!
!==============================================================
function str_datetime__fmt(fmt) result(s)
  implicit none
  character(*), intent(in) :: fmt
  character(len_trim(fmt)) :: s

  character(8)  :: date
  character(10) :: time
  character(5)  :: zone
  integer       :: values(8)
  integer :: loc

  call date_and_time(date, time, zone, values)

  s = trim(fmt)

  if( index(fmt,'YYYY') /= 0 )then
    loc = index(fmt,'YYYY')
    s(loc:loc+3) = date(1:4)
  elseif( index(fmt,'YY') /= 0 )then
    loc = index(fmt,'YY')
    s(loc:loc+1) = date(3:4)
  endif

  if( index(fmt,'MM') /= 0 )then
    loc = index(fmt,'MM')
    s(loc:loc+1) = date(5:6)
  endif

  if( index(fmt,'DD') /= 0 )then
    loc = index(fmt,'DD')
    s(loc:loc+1) = date(7:8)
  endif

  if( index(fmt,'hh') /= 0 )then
    loc = index(fmt,'hh')
    s(loc:loc+1) = time(1:2)
  endif

  if( index(fmt,'mm') /= 0 )then
    loc = index(fmt,'mm')
    s(loc:loc+1) = time(3:4)
  endif

  if( index(fmt,'ss.sss') /= 0 )then
    loc = index(fmt,'ss.sss')
    s(loc:loc+5) = time(5:10)
  elseif( index(fmt,'ss') /= 0 )then
    loc = index(fmt,'ss')
    s(loc:loc+1) = time(5:6)
  endif
end function str_datetime__fmt
!==============================================================
!
!==============================================================
real(8) function float_datetime__all() result(res)
  implicit none
  character(18) :: s

  s = str_datetime__all()
  read(s,*) res
end function float_datetime__all
!==============================================================
!
!==============================================================
real(8) function float_datetime__fmt(fmt) result(res)
  implicit none
  character(*), intent(in) :: fmt

  character(len_trim(fmt)) :: s
  integer :: ios

  s = str_datetime__fmt(fmt)
  read(s,*,iostat=ios) res
  if( ios /= 0 )then
    write(0,*) '*** Error @ float_datetime_fmt__MOD__'//trim(modname)
    write(0,*) 'Failed to convert formatted datetime to float.'
    stop
  endif
end function float_datetime__fmt
!==============================================================
!
!==============================================================
end module lib_time_base
