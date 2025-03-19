module lib_time_timer
  use lib_time_base
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: timer_

  public :: init_timer
  public :: start_timer
  public :: stop_timer
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  type timer_
    integer :: t0(8)
    integer :: t1(8)
    real(8) :: t_acc
  end type
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  interface init_timer
    module procedure init_timer_0d
    module procedure init_timer_1d
    module procedure init_timer_2d
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine init_timer_0d(timer)
  implicit none
  type(timer_), intent(out) :: timer

  timer%t_acc = 0.d0
end subroutine init_timer_0d
!===============================================================
!
!===============================================================
subroutine init_timer_1d(timer)
  implicit none
  type(timer_), intent(out) :: timer(:)

  timer(:)%t_acc = 0.d0
end subroutine init_timer_1d
!===============================================================
!
!===============================================================
subroutine init_timer_2d(timer)
  implicit none
  type(timer_), intent(out) :: timer(:,:)

  timer(:,:)%t_acc = 0.d0
end subroutine init_timer_2d
!===============================================================
!
!===============================================================
subroutine start_timer(timer)
  implicit none
  type(timer_), intent(inout) :: timer

  timer%t0 = date_and_time_values()
end subroutine start_timer
!===============================================================
!
!===============================================================
subroutine stop_timer(timer)
  implicit none
  type(timer_), intent(inout) :: timer

  timer%t1 = date_and_time_values()
  timer%t_acc = timer%t_acc + timediff(timer%t0, timer%t1)
end subroutine stop_timer
!===============================================================
!
!===============================================================
end module lib_time_timer
