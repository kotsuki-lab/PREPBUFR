module lib_math_exception
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: is_nan
  public :: is_inf
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface is_nan
    module procedure is_nan__dble_0d
    module procedure is_nan__dble_1d
    module procedure is_nan__dble_2d
    module procedure is_nan__condition_dble_1d
    module procedure is_nan__condition_dble_2d
  end interface

  interface is_inf
    module procedure is_inf__real
    module procedure is_inf__dble
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
logical function is_nan__dble_0d(x) result(res)
  implicit none
  real(8), intent(in) :: x

  integer (8) :: n
  integer :: i

  n = transfer(x, 0_8)

  ! Exponent part
  res = .true.
  do i = 52, 62
    if( .not. btest(n,i) )then
      res = .false.
      return
    endif
  enddo

  ! Fraction part
  res = .false.
  do i = 0, 51
    if( btest(n,i) )then
      res = .true.
      return
    endif
  enddo
end function is_nan__dble_0d
!===============================================================
!
!===============================================================
function is_nan__dble_1d(x) result(res)
  implicit none
  real(8), intent(in) :: x(:)
  logical :: res(size(x))

  integer :: i

  do i = 1, size(x)
    res(i) = is_nan__dble_0d(x(i))
  enddo
end function is_nan__dble_1d
!===============================================================
!
!===============================================================
function is_nan__dble_2d(x) result(res)
  implicit none
  real(8), intent(in) :: x(:,:)
  logical :: res(size(x,1),size(x,2))

  integer :: i, j

  do j = 1, size(x,2)
    do i = 1, size(x,1)
      res(i,j) = is_nan__dble_0d(x(i,j))
    enddo
  enddo
end function is_nan__dble_2d
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
logical function is_nan__condition_dble_1d(x, condition) result(res)
  implicit none
  real(8), intent(in) :: x(:)
  character(*), intent(in) :: condition

  integer :: i

  call echo(code%bgn, 'is_nan_condition_dble_1d', '-p')
  !-------------------------------------------------------------
  selectcase( condition )
  case( 'all' )
    res = .true.
    do i = 1, size(x)
      if( .not. is_nan__dble_0d(x(i)) )then
        res = .false.
        exit
      endif
    enddo
  case( 'any' )
    res = .false.
    do i = 1, size(x)
      if( is_nan__dble_0d(x(i)) )then
        res = .true. 
        exit
      endif
    enddo
  case default
    call eerr('Invalid value in $condition: '//str(condition))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function is_nan__condition_dble_1d
!===============================================================
!
!===============================================================
logical function is_nan__condition_dble_2d(x, condition) result(res)
  implicit none
  real(8), intent(in) :: x(:,:)
  character(*), intent(in) :: condition

  integer :: i, j

  call echo(code%bgn, 'is_nan__condition_dble_2d', '-p')
  !-------------------------------------------------------------
  selectcase( condition )
  case( 'all' )
    res = .true.
    loop_all:&
    do j = 1, size(x,2)
      do i = 1, size(x,1)
        if( .not. is_nan__dble_0d(x(i,j)) )then
          res = .false.
          exit loop_all
        endif
      enddo
    enddo&
    loop_all
  case( 'any' )
    res = .false.
    loop_any:&
    do j = 1, size(x,2)
      do i = 1, size(x,1)
        if( is_nan__dble_0d(x(i,j)) )then
          res = .true. 
          exit loop_any
        endif
      enddo
    enddo&
    loop_any
  case default
    call eerr('Invalid value in $condition: '//str(condition))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function is_nan__condition_dble_2d
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
logical function is_inf__real(x) result(is_inf)
  implicit none
  real(4), intent(in) :: x
  integer(4) :: n
  integer :: i

  n = transfer(x, 0_4)

  ! Exponent part
  is_inf = .true.
  do i = 23, 30
    if( .not. btest(n,i) )then
      is_inf = .false.
      return
    endif
  enddo

  ! Fraction part
  is_inf = .true.
  do i = 0, 22
    if( btest(n,i) )then
      is_inf = .false.
      return
    endif
  enddo
end function is_inf__real
!===============================================================
!
!===============================================================
logical function is_inf__dble(x) result(is_inf)
  implicit none
  real(8), intent(in) :: x
  integer(8) :: n
  integer :: i

  n = transfer(x, 0_8)

  ! Exponent part
  is_inf = .true.
  do i = 52, 62
    if( .not. btest(n,i) )then
      is_inf = .false.
      return
    endif
  enddo

  ! Fraction part
  is_inf = .true.
  do i = 0, 51
    if( btest(n,i) )then
      is_inf = .false.
      return
    endif
  enddo
end function is_inf__dble
!===============================================================
!
!===============================================================
end module lib_math_exception
