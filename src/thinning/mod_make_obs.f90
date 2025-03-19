module mod_make_obs
  use lib_const
  use lib_time
  use lib_log
  use lib_math, only: &
        add, &
        div
  use lib_io
  use common_XXXXXX, only: &
        set_common_XXXXXX
  use def_const_pb
  use def_type_pb
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: make_obs
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine make_obs(&
    f_data_template, f_obs_template, &
    time, earth_r, geo, obserr, lst_order, &
    monit)
  use mod_pb, only: &
        read_prepbufr_dumped, &
        free_prepbufr       , &
        set_dgt_nmsg        , &
        set_dgt_nsub        , &
        set_dgt_nrec
  use mod_base, only: &
        get_f_obs     , &
        idx_to_var_obs, &
        var_to_idx_obs
  use mod_share, only: &
        nlay_all      , &
        set_pb        , &
        set_vidx      , &
        set_model_grid, &
        set_logplv
  use mod_stats, only: &
        init_stats , &
        clear_stats, &
        report_stats__order
  use mod_set, only: &
        show_order
  use mod_common, only: &
        find_records_same_id        , &
        free_records_same_id        , &
        find_grids_around           , &
        free_grid                   , &
        find_subsets_in_grids       , &
        free_subsets_in_grids       , &
        init_records_status         , &
        find_local_records_of_layers, &
        make_lst_lay_all            , &
        make_lst_rec_valid          , &
        free_local_records_of_layers, &
        copy_pb_org                 , &
        free_pb                     , &
        output_obs                  , &
        write_monit_summary
  use mod__all, only: &
        select_records_all => select_records
  use mod__wgt, only: &
        select_records_wgt => select_records
  use mod__dnst, only: &
        select_records_dnst => select_records
  implicit none
  character(*), intent(in) :: f_data_template
  character(*), intent(in) :: f_obs_template
  type(time_) , intent(in) :: time
  real(8)     , intent(in) :: earth_r
  type(model_), intent(in) :: geo
  real(8)     , intent(in) :: obserr(:)
  type(order_), intent(in), target :: lst_order(:)
  type(monit_), intent(in) :: monit

  type(prepbufr_) :: pb0
  type(pb_) :: pb
  type(lay_all_)  , allocatable :: lst_lay_all(:)
  type(rec_valid_), allocatable :: lst_rec_valid(:)
  type(order_), pointer :: order
  character(CLEN_PATH) :: f_obs
  integer :: norder, iorder
  integer :: itime
  integer :: ivar
  integer :: vidx
  character(CLEN_MNNC) :: var
  integer :: info
  integer :: ncol_bar = 56
  type(timer_), allocatable :: ptime(:)
  type(timer_), allocatable :: ptime_order(:)

  call echo(code%bgn, 'make_obs')
  !-------------------------------------------------------------
  ! Set module variables that are constants in practice
  !-------------------------------------------------------------
  call set_pb()
  call set_vidx()
  call set_model_grid()
  call set_logplv()
  !-------------------------------------------------------------
  ! Remove old output files
  !-------------------------------------------------------------
  do itime = time%tout_bhd, time%tout_ahd
    call get_f_obs(f_obs_template, f_obs, itime, init=.true.)
  enddo
  !-------------------------------------------------------------
  ! Set module variables of MODULE common_XXXXXX
  !-------------------------------------------------------------
  call set_common_XXXXXX()
  !-------------------------------------------------------------
  ! Init. stats.
  !-------------------------------------------------------------
  call init_stats('valid', time)
  call init_stats('selected', time)
  !-------------------------------------------------------------
  ! Make output directory for monitoring data
  !-------------------------------------------------------------
  if( monit%save_monit )then
    call mkdir(monit%dir)
  endif
  !--------------------------------------------------------------
  !
  !--------------------------------------------------------------
  norder = size(lst_order)
  !--------------------------------------------------------------
  ! Select obs. (thinning)
  !--------------------------------------------------------------
  allocate(ptime(6))
  call init_timer(ptime)

  allocate(ptime_order(norder))
  call init_timer(ptime_order(:))

  do iorder = 1, norder
    order => lst_order(iorder)

    call find_grids_around(order%dist_h_thresh, earth_r)

    call echo(code%ent, 'order '//str(iorder), '-p -x2')
    call edbg(str('',ncol_bar,'-'))
    call show_order(order, iorder, norder)
    call edbg(str('',ncol_bar,'-'))
    call start_timer(ptime_order(iorder))
    !-----------------------------------------------------------
    ! Read data
    !-----------------------------------------------------------
    call read_prepbufr_dumped(f_data_template, order%msgtyp, pb0)
    if( pb0%nmsg == 0 )then
      call edbg('No data')
      call echo(code%ext)
      cycle
    endif

    call set_dgt_nmsg(pb0)
    call set_dgt_nsub(pb0)
    call set_dgt_nrec(pb0)

    call copy_pb_org(pb0, pb, order, geo, monit)
    call free_prepbufr(pb0)

    call find_records_same_id(pb, order%code_same_id)
    !-----------------------------------------------------------
    ! Make data for localization
    !-----------------------------------------------------------
    selectcase( order%method )
    case( METHOD_ALL  , &
          METHOD_WGT1 , &
          METHOD_DNST1, METHOD_DNST2 )
      call find_subsets_in_grids(pb)

    case( METHOD_NONE )
      continue

    case default
      call eerr('Invalid value in $order%method: '//str(order%method))
    endselect
    !-----------------------------------------------------------
    ! Select obs.
    !-----------------------------------------------------------
    do itime = time%tout_bhd, time%tout_ahd
      call edbg(str('',ncol_bar,'-'))
      call echo(code%ent, 'msgtyp='//str(order%msgtyp)//&
                ' time='//str(itime))
      call edbg(str('',ncol_bar,'-'), '-x2')

      call get_f_obs(f_obs_template, f_obs, itime, .false.)

      do ivar = 1, order%nvar
        vidx = order%vidx(ivar)
        var  = idx_to_var_obs(vidx)

        call edbg(str('',ncol_bar-2,'-'))
        call echo(code%ent, 'msgtyp='//str(order%msgtyp)//&
                  ' time='//str(itime)//' var='//str(var))
        call edbg(str('',ncol_bar-2,'-'), '-x2')
        !-------------------------------------------------------
        !
        !-------------------------------------------------------
        call start_timer(ptime(1))
        call init_records_status(pb, itime, vidx, order%wgt_qlt, info)
        call stop_timer(ptime(1))
        if( info == -1 )then
          call echo(code%ext)
          cycle
        endif
        !-------------------------------------------------------
        !
        !-------------------------------------------------------
        selectcase( order%method )
        !-------------------------------------------------------
        ! Method: all, wgt, dnst
        case( METHOD_ALL  , &
              METHOD_WGT1 , &
              METHOD_DNST1, METHOD_DNST2 )
          !-----------------------------------------------------
          !
          !-----------------------------------------------------
          call start_timer(ptime(2))
          call find_local_records_of_layers(&
                 pb, vidx, &
                 earth_r, &
                 order%dist_h_thresh, order%dist_v_thresh, order%rho_h, order%rho_v, &
                 order%wgt_qlt, info)
          call stop_timer(ptime(2))
          if( info /= 0 )then
            call echo(code%ext)
            cycle
          endif

          call start_timer(ptime(3))
          allocate(lst_lay_all(nlay_all))
          call make_lst_lay_all(lst_lay_all)
          call stop_timer(ptime(3))

          call start_timer(ptime(4))
          allocate(lst_rec_valid(pb%nrec_valid))
          call make_lst_rec_valid(pb, lst_lay_all, lst_rec_valid)
          call stop_timer(ptime(4))
          !-----------------------------------------------------
          ! Main process
          !-----------------------------------------------------
          call start_timer(ptime(5))
          selectcase( order%method )
          case( METHOD_ALL )
            call select_records_all(&
                   pb, lst_lay_all, lst_rec_valid, &
                   order)
          case( METHOD_WGT1 )
            call select_records_wgt(&
                   pb, lst_lay_all, lst_rec_valid, &
                   order)
          case( METHOD_DNST1, METHOD_DNST2 )
            call select_records_dnst(&
                   pb, lst_lay_all, lst_rec_valid, &
                   order, earth_r, monit)
          case default
            call eerr('Invalid value in $order%method: '//str(order%method))
          endselect
          call stop_timer(ptime(5))
          !-----------------------------------------------------
          if( monit%save_monit )then
            call write_monit_summary(monit, pb, order, var, itime)
          endif
        !-------------------------------------------------------
        ! Method: none
        case( METHOD_NONE )

          if( monit%save_monit )then
            call write_monit_summary(monit, pb, order, var, itime)
          endif
        !-------------------------------------------------------
        case default
          call eerr('Invalid value in $order%method: '//str(order%method))
        endselect
        !-------------------------------------------------------
        ! Output selected obs.
        !-------------------------------------------------------
        call start_timer(ptime(6))
        call output_obs(f_obs, pb, itime, order%msgtyp, var, &
                        order%obserr_scale, obserr)
        call stop_timer(ptime(6))
        !-------------------------------------------------------
        ! Free datasets
        !-------------------------------------------------------
        selectcase( order%method )
        case( METHOD_ALL  , &
              METHOD_WGT1 , &
              METHOD_DNST1, METHOD_DNST2 )
          !call free_lst_rec_valid()
          !call free_lst_lay_all()
          deallocate(lst_lay_all)
          deallocate(lst_rec_valid)
          call free_local_records_of_layers()
        case( METHOD_NONE )
          continue
        case default
          call eerr('Invalid value in $order%method: '//str(order%method))
        endselect

        call echo(code%ext)
      enddo  ! ivar/

      call echo(code%ext)
    enddo  ! time/
    !-----------------------------------------------------------
    ! Free datasets
    !-----------------------------------------------------------
    selectcase( order%method )
    case( METHOD_ALL  , &
          METHOD_WGT1 , &
          METHOD_DNST1, METHOD_DNST2 )
      call free_subsets_in_grids()
    case( METHOD_NONE )
      continue
    case default
      call eerr('Invalid value in $order%method: '//str(order%method))
    endselect

    call free_records_same_id(order%code_same_id)

    call free_pb(pb)
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    call stop_timer(ptime_order(iorder))
    call edbg('Process Time: '//str(ptime_order(iorder)%t_acc,'es9.2'))
    !-----------------------------------------------------------
    call echo(code%ext)
  enddo  ! iorder/

  call free_grid()

  deallocate(ptime_order)
  !-------------------------------------------------------------
  ! Finalize
  !-------------------------------------------------------------
  call edbg(str('',ncol_bar,'-'))

  call report_stats__order(lst_order, time)
  call clear_stats('valid')
  call clear_stats('selected')

  call edbg('Process Time (sec)')
  call edbg('  Initializing status of records : '//str(ptime(1)%t_acc,'es9.2'))
  call edbg('  Finding local records of layers: '//str(ptime(2)%t_acc,'es9.2'))
  call edbg('  Making a list of layers        : '//str(ptime(3)%t_acc,'es9.2'))
  call edbg('  Making a list of records       : '//str(ptime(4)%t_acc,'es9.2'))
  call edbg('  Selecting records              : '//str(ptime(5)%t_acc,'es9.2'))
  call edbg('  Outputting                     : '//str(ptime(6)%t_acc,'es9.2'))
  deallocate(ptime)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_obs
!===============================================================
!
!===============================================================
end module mod_make_obs
