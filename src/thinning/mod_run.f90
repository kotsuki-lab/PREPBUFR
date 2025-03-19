module mod_run
  use lib_const
  use lib_log
  use def_const
  use def_type
  implicit none
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: run_get_stats
  public :: run_make_obs
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine run_get_stats(f_conf)
  use common_XXXXXX, only: &
        set_common_XXXXXX
  use def_type_pb
  use mod_pb, only: &
        read_prepbufr_dumped
  use mod_set, only: &
        read_conf
  use mod_stats, only: &
        get_stats_pb_org
  implicit none
  character(*), intent(in) :: f_conf

  character(CLEN_PATH) :: f_data, f_obs
  type(time_) :: time
  real(8) :: earth_r
  type(model_) :: geo
  real(8), pointer :: obserr(:)
  type(order_), pointer :: lst_order(:)
  type(monit_) :: monit

  call echo(code%bgn, 'run_get_stats')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(obserr)
  nullify(lst_order)

  call set_common_XXXXXX

  call read_conf(&
         f_conf, &
         f_data, f_obs, &
         time, earth_r, geo, obserr, lst_order, monit)

  call get_stats_pb_org(f_data, time)

  if( associated(obserr) ) deallocate(obserr)
  if( associated(lst_order) ) deallocate(lst_order)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine run_get_stats
!===============================================================
!
!===============================================================
subroutine run_make_obs(f_conf)
  use common_XXXXXX, only: &
        set_common_XXXXXX
  use mod_set, only: &
        read_conf
  use mod_make_obs, only: &
        make_obs
  implicit none
  character(*), intent(in) :: f_conf

  character(CLEN_PATH) :: f_data, f_obs
  type(time_) :: time
  real(8) :: earth_r
  type(model_) :: geo
  real(8), pointer :: obserr(:)
  type(order_), pointer :: lst_order(:)
  type(monit_) :: monit

  nullify(obserr)
  nullify(lst_order)

  call set_common_XXXXXX

  call read_conf(&
         f_conf, &
         f_data, f_obs, &
         time, earth_r, geo, obserr, lst_order, monit)

  call make_obs(&
         f_data, f_obs, &
         time, earth_r, geo, obserr, lst_order, monit)

  ! TODO
  ! free geo

  if( associated(obserr) ) deallocate(obserr)
  if( associated(lst_order) ) deallocate(lst_order)
end subroutine run_make_obs
!===============================================================
!
!===============================================================
end module mod_run
