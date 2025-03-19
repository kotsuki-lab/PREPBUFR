module mod_stats
  use common_obs_XXXXXX, only: &
        ID_U_OBS, ID_V_OBS , ID_T_OBS , &
        ID_Q_OBS, ID_RH_OBS, ID_PS_OBS, &
        ID_RAIN_OBS, &
        ityp_ADPUPA_obs, ityp_AIRCAR_obs, ityp_AIRCFT_obs, &
        ityp_SATWND_obs, ityp_PROFLR_obs, ityp_VADWND_obs, &
        ityp_SATEMP_obs, ityp_ADPSFC_obs, ityp_SFCSHP_obs, &
        ityp_SFCBOG_obs, ityp_SPSSMI_obs, ityp_SYNDAT_obs, &
        ityp_ERS1DA_obs, ityp_GOESND_obs, ityp_QKSWND_obs, &
        ityp_MSONET_obs, ityp_GPSIPW_obs, ityp_RASSDA_obs, &
        ityp_WDSATR_obs, ityp_ASCATW_obs
  use lib_const
  use lib_base
  use lib_log
  use lib_math
  use lib_io
  use lib_array
  use lib_math
  use def_const_pb
  use def_type_pb
  use def_const
  use def_type
  use mod_share
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: get_stats_pb_org
  public :: get_stats_output
  public :: report_stats__all
  public :: report_stats__order
  public :: init_stats
  public :: clear_stats
  !-------------------------------------------------------------
  ! 
  !-------------------------------------------------------------
  type stvar_
    integer, pointer :: nrec(:)
    real(8) :: obsmin, obsmax, obsmean
  end type

  type sttyp_
    type(stvar_), pointer :: var(:)
    integer :: nrec
  end type

  type stats_
    type(sttyp_), pointer :: typ(:)
  end type

  type(stats_), allocatable, target :: stats_all(:)
  type(stats_), allocatable, target :: stats_valid(:)
  type(stats_), allocatable, target :: stats_selected(:)
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine get_stats_pb_org(fin, time)
  use def_type_pb
  use mod_pb, only: &
        read_prepbufr_dumped, &
        free_prepbufr, &
        typ_to_idx_msgtyp
  implicit none
  character(*), intent(in) :: fin
  type(time_), intent(in) :: time

  type(sttyp_), pointer :: sttyp
  type(stvar_), pointer :: stvar
  type(prepbufr_), target :: pb0
  type(message_), pointer :: msg
  type(subset_) , pointer :: sub
  type(record_) , pointer :: rec
  integer :: imsg, isub, irec, ivar
  integer :: imsgtyp
  integer :: itime
  integer :: iqmk

  call echo(code%bgn, 'get_stats_pb_org')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call read_prepbufr_dumped(fin, 'all', pb0)

  call init_stats('all', time)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do itime = time%tout_bhd, time%tout_ahd
  do imsg = 1, pb0%nmsg
    msg => pb0%msg(imsg)
    imsgtyp = typ_to_idx_msgtyp(msg%typ)
    sttyp => stats_all(itime)%typ(imsgtyp)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)
      do irec = 1, sub%nrec
        rec => sub%rec(irec)
        do ivar = 1, nvar
          stvar => sttyp%var(ivar)
          if( rec%obs(ivar) == PREPBUFR_MISS )then
            call add(stvar%nrec(-2))
          else
            selectcase( nint(rec%qmk(ivar)) )
            case( 0:3 )
              iqmk = nint(rec%qmk(ivar))
            case( 4: )
              iqmk = 4
            case default
              call eerr('Unexpected condition:'//&
                       '\n  init(rec%qmk(ivar)) < 0')
            endselect
            call add(stvar%nrec(iqmk))
            stvar%obsmin = min(stvar%obsmin, rec%obs(ivar))
            stvar%obsmax = max(stvar%obsmax, rec%obs(ivar))
            call add(stvar%obsmean, rec%obs(ivar))
          endif
        enddo  ! ivar/
      enddo  ! irec/
    enddo  ! isub/
  enddo  ! imsg/
  enddo

  do itime = time%tout_bhd, time%tout_ahd
  do imsgtyp = 1, nmsgtyp
    sttyp => stats_all(itime)%typ(imsgtyp)
    sttyp%nrec = sum(sttyp%var(1)%nrec(:))
  enddo
  enddo

  do itime = time%tout_bhd, time%tout_ahd
  do imsgtyp = 1, nmsgtyp
    do ivar = 1, nvar
      stvar => stats_all(itime)%typ(imsgtyp)%var(ivar)
      if( sum(stvar%nrec(-1:)) > 0 )then
        call div(stvar%obsmean, sum(stvar%nrec(-1:)))
      else
        stvar%obsmean = PREPBUFR_MISS
      endif
    enddo
  enddo
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call report_stats__all(time)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call clear_stats('all')
  call free_prepbufr(pb0)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_stats_pb_org
!===============================================================
!
!===============================================================
subroutine get_stats_output(varname, pb, itime, imsgtyp, vidx)
  implicit none
  character(*), intent(in)         :: varname
  type(pb_)   , intent(in), target :: pb
  integer     , intent(in)         :: itime
  integer     , intent(in)         :: imsgtyp
  integer     , intent(in)         :: vidx

  type(sttyp_), pointer :: sttyp
  type(stvar_), pointer :: stvar
  type(msg_), pointer :: msg
  type(sub_), pointer :: sub
  type(rec_), pointer :: rec
  integer :: imsg, isub, irec

  call echo(code%bgn, 'get_stats_output', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( varname )
  case( 'valid' )
    sttyp => stats_valid(itime)%typ(imsgtyp)
    stvar => sttyp%var(vidx)

    do imsg = 1, pb%nmsg
      msg => pb%msg(imsg)
      do isub = 1, msg%nsub
        sub => msg%sub(isub)
        do irec = 1, sub%nrec
          rec => sub%rec(irec)
          if( rec%is_valid )then
            call add(stvar%nrec(rec%iqmk(vidx)))
            stvar%obsmin = min(stvar%obsmin, rec%obs(vidx))
            stvar%obsmax = max(stvar%obsmax, rec%obs(vidx))
            call add(stvar%obsmean, rec%obs(vidx))
          endif
        enddo
      enddo
    enddo

    if( sum(stvar%nrec(-1:)) /= pb%nrec_valid )then
      call eerr('Unexpected condition:'//&
              '\n  sum(stvar%nrec(-1:)) /= pb%nrec_valid'//&
              '\n  stvar%nrec(-1:): '//str(stvar%nrec(-1:))//&
              '\n  pb%nrec_valid: '//str(pb%nrec_valid))
    endif

    if( pb%nrec_valid > 0 )then
      call div(stvar%obsmean, pb%nrec_valid)
    endif

    call add(sttyp%nrec, pb%nrec_valid)
  case( 'selected' )
    sttyp => stats_selected(itime)%typ(imsgtyp)
    stvar => sttyp%var(vidx)

    do imsg = 1, pb%nmsg
      msg => pb%msg(imsg)
      do isub = 1, msg%nsub
        sub => msg%sub(isub)
        do irec = 1, sub%nrec
          rec => sub%rec(irec)
          if( rec%is_selected )then
            call add(stvar%nrec(rec%iqmk(vidx)))
            stvar%obsmin = min(stvar%obsmin, rec%obs(vidx))
            stvar%obsmax = max(stvar%obsmax, rec%obs(vidx))
            call add(stvar%obsmean, rec%obs(vidx))
          endif
        enddo
      enddo
    enddo

    if( sum(stvar%nrec(-1:)) /= pb%nrec_selected )then
      call eerr('Unexpected condition:'//&
              '\n  sum(stvar%nrec(-1:)) /= pb%nrec_selected'//&
              '\n  stvar%nrec(-1:): '//str(stvar%nrec(-1:))//&
              '\n  pb%nrec_selected: '//str(pb%nrec_selected))
    endif

    if( pb%nrec_selected > 0 )then
      call div(stvar%obsmean, pb%nrec_selected)
    endif

    call add(sttyp%nrec, pb%nrec_selected)
  case default
    call eerr('Invalid value in $varname: '//str(varname))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_stats_output
!===============================================================
!
!===============================================================
subroutine report_stats__all(time)
  use mod_pb, only: &
        idx_to_typ_msgtyp
  use mod_base, only: &
        idx_to_var_obs
  implicit none
  type(time_), intent(in) :: time

  type(sttyp_), pointer :: sttyp
  integer :: itime
  integer :: imsgtyp
  integer :: ivar
  character(clen_var) :: var
  integer :: dgt_var, dgt_nrec

  call echo(code%bgn, 'report_stats__all')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_var = 0
  do ivar = 1, nvar
    var = idx_to_var_obs(ivar)
    dgt_var = max(dgt_var, len_trim(var))
  enddo

  do itime = time%tout_bhd, time%tout_ahd
    dgt_nrec = max(dgt(maxval(stats_all(itime)%typ(:)%nrec)), 5)
    do imsgtyp = 1, NMSGTYP
      sttyp => stats_all(itime)%typ(imsgtyp)

      call edbg(str(idx_to_typ_msgtyp(imsgtyp),CLEN_MNNC)//' '//&
                str(sttyp%nrec,dgt_nrec)//' records in total')

      if( sttyp%nrec == 0 ) cycle

      call print_stats_head(dgt_var, dgt_nrec)
      do ivar = 1, nvar
        call print_stats_stvar('all', itime, imsgtyp, ivar, dgt_var, dgt_nrec)
      enddo  ! ivar/
    enddo  ! imsgtyp/
  enddo  ! itime/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine report_stats__all
!===============================================================
!
!===============================================================
subroutine report_stats__order(lst_order, time)
  use mod_pb, only: &
        idx_to_typ_msgtyp, &
        typ_to_idx_msgtyp
  use mod_base, only: &
        idx_to_var_obs
  implicit none
  type(order_), intent(in), target :: lst_order(:)
  type(time_), intent(in) :: time

  type(order_), pointer :: order
  integer :: itime
  integer :: norder, iorder
  integer :: ivar
  integer :: imsgtyp
  character(CLEN_MNNC) :: var
  integer :: nrec_selected_all, nrec_valid_all
  integer :: dgt_var, dgt_nrec
  integer :: clen_bar = 56

  call echo(code%bgn, 'report_stats__order')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_var = 0
  do ivar = 1, nvar
    var = idx_to_var_obs(ivar)
    dgt_var = max(dgt_var, len_trim(var))
  enddo

  norder = size(lst_order)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do itime = time%tout_bhd, time%tout_ahd
    call edbg(str('',clen_bar,'-'))
    call edbg('Report of time='//str(itime))
    call edbg(str('',clen_bar,'-'))

    nrec_valid_all = 0
    nrec_selected_all = 0
    do iorder = 1, norder
      order => lst_order(iorder)
      imsgtyp = typ_to_idx_msgtyp(order%msgtyp)
      call add(nrec_valid_all, stats_valid(itime)%typ(imsgtyp)%nrec)
      call add(nrec_selected_all, stats_selected(itime)%typ(imsgtyp)%nrec)
    enddo
    call edbg('TOTAL: '//str(nrec_selected_all)//' in '//str(nrec_valid_all)//&
              ' valid observations are selected')

    dgt_nrec = 5
    do iorder = 1, norder
      order => lst_order(iorder)
      imsgtyp = typ_to_idx_msgtyp(order%msgtyp)
      do ivar = 1, nvar
        dgt_nrec = max(dgt_nrec,&
          dgt(maxval(stats_valid(itime)%typ(imsgtyp)%var(ivar)%nrec(:))),&
          dgt(maxval(stats_selected(itime)%typ(imsgtyp)%var(ivar)%nrec(:))))
      enddo
    enddo

    do iorder = 1, norder
      order => lst_order(iorder)

      imsgtyp = typ_to_idx_msgtyp(order%msgtyp)

      call edbg(str(order%msgtyp,CLEN_MNNC)//': '//&
                str(stats_selected(itime)%typ(imsgtyp)%nrec)//&
                ' in '//str(stats_valid(itime)%typ(imsgtyp)%nrec)//&
                ' valid observations are selected')

      if( stats_selected(itime)%typ(imsgtyp)%nrec > 0 )then
        call print_stats_head(dgt_var, dgt_nrec)
        do ivar = 1, nvar
          if( .not. any(order%vidx(:) == ivar) ) cycle
          call print_stats_stvar('selected', itime, imsgtyp, ivar, dgt_var, dgt_nrec)
        enddo  ! ivar/
      endif
      if( stats_valid(itime)%typ(imsgtyp)%nrec > 0 )then
        call print_stats_head(dgt_var, dgt_nrec)
        do ivar = 1, nvar
          if( .not. any(order%vidx(:) == ivar) ) cycle
          call print_stats_stvar('valid', itime, imsgtyp, ivar, dgt_var, dgt_nrec)
        enddo
      endif
    enddo  ! iorder/
  enddo  ! itime/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine report_stats__order
!===============================================================
!
!===============================================================
subroutine print_stats_head(dgt_var, dgt_nrec)
  implicit none
  integer, intent(in) :: dgt_var, dgt_nrec

  call edbg(str('',dgt_var)//' '//&
            str((/'noObs', 'noQmk', 'Q0   ', 'Q1   ', &
                  'Q2   ', 'Q3   ', 'Q4-  '/),dgt_nrec)//&
            ' '//str((/'min ', 'max ', 'mean'/),9), &
            '+x4')
end subroutine print_stats_head
!===============================================================
!
!===============================================================
subroutine print_stats_stvar(varname, itime, imsgtyp, ivar, dgt_var, dgt_nrec)
  use mod_base, only: &
        idx_to_var_obs
  implicit none
  character(*), intent(in) :: varname
  integer, intent(in) :: itime, imsgtyp, ivar
  integer, intent(in) :: dgt_var, dgt_nrec

  type(stvar_), pointer :: stvar
  character(64) :: s_obs

  selectcase( varname )
  case( 'all' )
    stvar => stats_all(itime)%typ(imsgtyp)%var(ivar)
  case( 'valid' )
    stvar => stats_valid(itime)%typ(imsgtyp)%var(ivar)
  case( 'selected' )
    stvar => stats_selected(itime)%typ(imsgtyp)%var(ivar)
  case default
    call eerr('Invalid value in $varname: '//str(varname))
  endselect

  if( sum(stvar%nrec(-1:)) == 0 )then
    s_obs = str((/'', '', ''/),-9,' ',fill='-')
  else
    s_obs = str((/stvar%obsmin,stvar%obsmax,stvar%obsmean/),'es9.2',' ')
  endif

  call edbg(str(idx_to_var_obs(ivar),dgt_var)//' '//&
            str(stvar%nrec,dgt_nrec,' ')//&
            ' '//s_obs, &
            '+x4')
end subroutine print_stats_stvar
!===============================================================
!
!===============================================================
subroutine init_stats(varname, time)
  implicit none
  character(*), intent(in) :: varname
  type(time_) , intent(in) :: time

  type(stats_), pointer :: stats(:)
  type(sttyp_), pointer :: sttyp
  type(stvar_), pointer :: stvar
  integer :: imsgtyp, ivar
  integer :: itime

  call echo(code%bgn, 'init_stats', '-p -x2')
  !-------------------------------------------------------------
  selectcase( varname )
  case( 'all' )
    allocate(stats_all(time%tout_bhd:time%tout_ahd))
    stats => stats_all
  case( 'valid' )
    allocate(stats_valid(time%tout_bhd:time%tout_ahd))
    stats => stats_valid
  case( 'selected' )
    allocate(stats_selected(time%tout_bhd:time%tout_ahd))
    stats => stats_selected
  case default
    call eerr('Invalid value in $varname: '//str(varname))
  endselect

  do itime = time%tout_bhd, time%tout_ahd
    allocate(stats(itime)%typ(NMSGTYP))
    do imsgtyp = 1, NMSGTYP
      sttyp => stats(itime)%typ(imsgtyp)
      sttyp%nrec = 0
      allocate(sttyp%var(nvar))
      do ivar = 1, nvar
        stvar => sttyp%var(ivar)
        allocate(stvar%nrec(-2:4))
        stvar%nrec(:) = 0
        stvar%obsmin  =  1d20
        stvar%obsmax  = -1d20
        stvar%obsmean = 0.d0
      enddo  ! ivar/
    enddo  ! imsgtyp/
  enddo  ! itime/

  nullify(stats)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_stats
!===============================================================
!
!===============================================================
subroutine clear_stats(varname)
  implicit none
  character(*), intent(in) :: varname

  call echo(code%bgn, 'clear_stats')
  !-------------------------------------------------------------
  selectcase( varname )
  case( 'valid' )
    deallocate(stats_valid)
  case( 'selected' )
    deallocate(stats_selected)
  case default
    call eerr('Invalid value in $varname: '//str(varname))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine clear_stats
!===============================================================
!
!===============================================================
end module mod_stats
