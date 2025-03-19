module lib_base_specialnumber
  implicit none
  private
  !-------------------------------------------------------------
  ! Procedures
  !-------------------------------------------------------------
  public :: is_nan
  public :: is_inf
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface is_nan
    module procedure is_nan4_0d
    module procedure is_nan8_0d
    module procedure is_nan4_1d
    module procedure is_nan8_1d
    module procedure is_nan4_2d
    module procedure is_nan8_2d
  end interface

  interface is_inf
    module procedure is_inf4
    module procedure is_inf8
  end interface
!---------------------------------------------------------------
contains
!=======================================================================
!
!=======================================================================
logical function is_nan4_0d(x) result(res)
  implicit none
  real(4), intent(in) :: x
  integer(4) :: n
  integer :: i

  n = transfer(x, 0_4)

  ! Exponent part
  res = .true.
  do i = 23, 30
    if( .not. btest(n,i) )then
      res = .false.
      return
    endif
  enddo

  ! Fraction part
  res = .false.
  do i = 0, 22
    if( btest(n,i) )then
      res = .true.
      return
    endif
  enddo
end function is_nan4_0d
!=======================================================================
!
!=======================================================================
logical function is_nan8_0d(x) result(res)
  implicit none
  real(8), intent(in) :: x
  integer(8) :: n
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
end function is_nan8_0d
!=======================================================================
!
!=======================================================================
function is_nan4_1d(x) result(res)
  implicit none
  real(4), intent(in) :: x(:)
  logical             :: res(size(x))
  integer(8) :: i

  do i = 1_8, size(x,kind=8)
    res(i) = is_nan(x(i))
  enddo
end function is_nan4_1d
!=======================================================================
!
!=======================================================================
function is_nan8_1d(x) result(res)
  implicit none
  real(8), intent(in) :: x(:)
  logical             :: res(size(x))
  integer(8) :: i

  do i = 1_8, size(x,kind=8)
    res(i) = is_nan(x(i))
  enddo
end function is_nan8_1d
!=======================================================================
!
!=======================================================================
function is_nan4_2d(x) result(res)
  implicit none
  real(4), intent(in) :: x(:,:)
  logical             :: res(size(x,1),size(x,2))
  integer(8) :: i, j

  do j = 1_8, size(x,2,kind=8)
    do i = 1_8, size(x,1,kind=8)
      res(i,j) = is_nan(x(i,j))
    enddo
  enddo
end function is_nan4_2d
!=======================================================================
!
!=======================================================================
function is_nan8_2d(x) result(res)
  implicit none
  real(8), intent(in) :: x(:,:)
  logical             :: res(size(x,1),size(x,2))
  integer(8) :: i, j

  do j = 1_8, size(x,2,kind=8)
    do i = 1_8, size(x,1,kind=8)
      res(i,j) = is_nan(x(i,j))
    enddo
  enddo
end function is_nan8_2d
!=======================================================================
!
!=======================================================================
!
!
!
!
!
!=======================================================================
!
!=======================================================================
logical function is_inf4(x) result(is_inf)
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
end function is_inf4
!=======================================================================
!
!=======================================================================
logical function is_inf8(x) result(is_inf)
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
end function is_inf8
!=======================================================================
!
!=======================================================================
end module lib_base_specialnumber
