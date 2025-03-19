module mod_set
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
  public :: read_nml_file
  public :: read_conf

  public :: show_order
  !-------------------------------------------------------------
  ! Private Module Variables (Parameters)
  !-------------------------------------------------------------
  integer, parameter :: TOUT_BHD_DEFAULT = -3
  integer, parameter :: TOUT_AHD_DEFAULT =  3

  real(8), parameter :: TINC_BHD_DEFAULT = 0.5d0
  real(8), parameter :: TINC_AHD_DEFAULT = 0.49d0

  integer, parameter :: NMAX_DEFAULT = 0

  real(8), parameter :: RHO_H_DEFAULT = 500.d0
  real(8), parameter :: RHO_V_DEFAULT = 0.1d0

  real(8), parameter :: DNST_THRESH_DEFAULT = 1.d0

  logical, parameter :: USE_ALL_Q0_DEFAULT = .false.

  real(8), parameter :: WGT_QLT_DEFAULT(-1:UB_WGT_QLT-1)&
    = (/0.4d0, &  ! -1: qmk is missing
        1.0d0, &  !  0
        0.8d0, &  !  1
        0.4d0, &  !  2
        0.1d0, &  !  3
        0.0d0 /)  !  4

  real(8), parameter :: WGT_SAME_SID_DEFAULT = 1.d0
  integer, parameter :: NMAX_SAME_SID_DEFAULT = 0
  real(8), parameter :: WGT_SAME_SAID_DEFAULT = 1.d0
  integer, parameter :: NMAX_SAME_SAID_DEFAULT = 0

  integer, parameter :: OPT_SFC_DEFAULT  = OPT_SFC__NOT_MODIFY
  integer, parameter :: OPT_TV_DEFAULT   = OPT_TV__DO_NOT_USE
  integer, parameter :: OPT_QERR_DEFAULT = OPT_QERR__CONV_RHERR

  real(8), parameter :: LAPSE_RATE_DEFAULT = 0.55d-2

  real(8), parameter :: OBSERR_U_DEFAULT  = 1.d0
  real(8), parameter :: OBSERR_V_DEFAULT  = 1.d0
  real(8), parameter :: OBSERR_T_DEFAULT  = 1.d0
  real(8), parameter :: OBSERR_Q_DEFAULT  = 1.d-3
  real(8), parameter :: OBSERR_PS_DEFAULT = 1.d0

  character(CLEN_PATH), parameter :: DIR_MONIT_DEFAULT = 'tmp/monit'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine read_nml_file(&
    un, f_pb_, f_table_, f_data_, f_obs_, replace_old_dumped_file)
  implicit none
  integer, intent(in) :: un
  character(*), intent(out) :: f_pb_
  character(*), intent(out) :: f_table_
  character(*), intent(out) :: f_data_
  character(*), intent(out) :: f_obs_
  logical     , intent(out) :: replace_old_dumped_file

  character(CLEN_PATH) :: f_pb
  character(CLEN_PATH) :: f_table
  character(CLEN_PATH) :: f_data
  character(CLEN_PATH) :: f_obs

  namelist/nml_file/&
    f_pb, f_table, f_data, f_obs, replace_old_dumped_file

  f_pb = ''
  f_table = ''
  f_data = ''
  f_obs = ''
  replace_old_dumped_file = .false.

  rewind(un)
  read(un, nml=nml_file)

  f_pb_    = f_pb
  f_table_ = f_table
  f_data_  = f_data
  f_obs_   = f_obs
end subroutine read_nml_file
!===============================================================
!
!===============================================================
subroutine read_conf(&
    f_conf, &
    f_data, f_obs, &
    time, earth_r, geo, obserr, lst_order, monit)
  use mod_pb, only: &
        set_mod_pb__opt_log_proc, &
        typ_to_idx_msgtyp
  use mod_base, only: &
        id_to_var_obs, &
        var_to_id_obs, &
        idx_to_var_obs, &
        var_to_idx_obs, &
        var_to_idx_obserr, &
        idx_obserr_to_var, &
        str_opt_Tv  , &
        str_opt_Qerr, &
        str_opt_sfc
  implicit none
  character(*), intent(in)  :: f_conf
  character(*), intent(out) :: f_data
  character(*), intent(out) :: f_obs
  type(time_) , intent(out) :: time
  real(8)     , intent(out) :: earth_r
  type(model_), intent(out) :: geo
  real(8)     , pointer     :: obserr(:)
  type(order_), pointer     :: lst_order(:)
  type(monit_), intent(out) :: monit

  ! /nml_time/
  integer :: datetime
  integer :: tout_bhd, tout_ahd

  ! /nml_geo/
  integer :: nlon, nlat
  real(8) :: west, east, south, north
  logical :: is_south_to_north
  character(CLEN_PATH) :: f_elv
  real(8) :: elv_miss

  ! /nml_common/
  real(8) :: ticld_bhd, ticld_ahd
  integer :: nmax
  real(8) :: rho_h, rho_v
  real(8) :: dnst_thresh
  logical :: use_all_q0
  real(8), allocatable :: wgt_qlt(:)
  real(8) :: wgt_same_sid
  integer :: nmax_same_sid
  real(8) :: wgt_same_said
  integer :: nmax_same_said
  integer :: opt_sfc
  integer :: opt_Tv
  integer :: opt_Qerr
  real(8) :: lapse_rate
  real(8) :: obserr_scale
  character(CLEN_PATH) :: f_obserr

  ! /nml_constant/
  !real(8) :: earth_r

  ! /nml_msg/
  character(CLEN_MNNC) :: typ
  character(CLEN_VAR)  :: method
  character(CLEN_LINE) :: vars
  !real(8) :: ticld_bhd, ticld_fnt
  !real(8) :: rho_h, rho_v
  !real(8) :: dnst_thresh
  !integer :: nmax
  !logical :: use_all_q0
  !real(8) :: wgt_qlt(:)

  ! /nml_monitor/
  logical :: monit_proc
  logical :: monit_itr_sct_rec
  logical :: monit_itr_add_rec
  logical :: save_modif
  logical :: save_monit
  character(CLEN_PATH) :: dir_monit

  ! common params.
  real(8) :: ticld_bhd_common, ticld_ahd_common
  integer :: nmax_common
  real(8) :: rho_h_common
  real(8) :: rho_v_common
  real(8) :: dnst_thresh_common
  logical :: use_all_q0_common
  real(8), allocatable :: wgt_qlt_common(:)
  real(8) :: wgt_same_sid_common
  integer :: nmax_same_sid_common
  real(8) :: wgt_same_said_common
  integer :: nmax_same_said_common
  integer :: opt_sfc_common
  integer :: opt_Tv_common
  integer :: opt_Qerr_common
  real(8) :: lapse_rate_common
  real(8) :: obserr_scale_common

  ! 
  type(order_), pointer :: order
  integer :: norder, iorder
  integer :: ivar
  integer :: vidx, vidx_obserr
  integer :: id_var
  character(CLEN_MNNC), allocatable :: lst_var(:)
  character(CLEN_MNNC) :: var
  integer :: i
  logical :: is_space
  logical :: ex
  real(8) :: r8_

  character(CLEN_PATH) :: f_pb
  character(CLEN_PATH) :: f_table
  logical              :: replace_old_dumped_file


  integer :: un_conf
  integer :: un_obserr
  integer :: ios

  integer :: dgt_var, dgt_id_var

  namelist/nml_time/ &
    datetime, &
    tout_bhd, tout_ahd
  namelist/nml_geo/ &
    nlon, nlat, &
    west, east, south, north, &
    is_south_to_north, &
    f_elv, elv_miss
  namelist/nml_common/ &
    ticld_bhd, ticld_ahd, &
    nmax, &
    rho_h, rho_v, &
    dnst_thresh, &
    use_all_q0, &
    wgt_qlt, &
    wgt_same_sid, nmax_same_sid, &
    wgt_same_said, nmax_same_said, &
    opt_sfc, opt_Tv, opt_Qerr, &
    lapse_rate, &
    obserr_scale, &
    f_obserr
  namelist/nml_constant/ &
    earth_r
  namelist/nml_msg/ &
    typ, method, vars, &
    ticld_bhd, ticld_ahd, &
    nmax, &
    rho_h, rho_v, wgt_qlt, &
    dnst_thresh, &
    use_all_q0, &
    wgt_qlt, &
    wgt_same_sid, nmax_same_sid, &
    wgt_same_said, nmax_same_said, &
    opt_sfc, opt_Tv, opt_Qerr, &
    lapse_rate
  namelist/nml_monitor/ &
    monit_proc, &
    monit_itr_sct_rec, &
    monit_itr_add_rec, &
    save_modif, &
    save_monit, &
    dir_monit

  call echo(code%bgn, 'read_conf')
  !-------------------------------------------------------------
  ! Read namelists
  !-------------------------------------------------------------
  call echo(code%ent, 'Reading namelists')

  un_conf = unit_number()
  open(un_conf, file=f_conf, status='old')
  !!!-----------------------------------------------------------
  !!! nml_file
  !!!-----------------------------------------------------------
  call echo(code%ent, 'Reading nml_file')

  call read_nml_file(&
         un_conf, &
         f_pb, f_table, f_data, f_obs, replace_old_dumped_file)

  call echo(code%ext)
  !!!-----------------------------------------------------------
  !!! nml_time
  !!!-----------------------------------------------------------
  call echo(code%ent, 'Reading nml_time')

  datetime = 0_8
  tout_bhd = TOUT_BHD_DEFAULT
  tout_ahd = TOUT_AHD_DEFAULT

  rewind(un_conf)
  read(un_conf, nml=nml_time)

  time%datetime = datetime
  time%tout_bhd = tout_bhd
  time%tout_ahd = tout_ahd

  call edbg('datetime: '//str(datetime))
  call edbg('tout_bhd: '//str(tout_bhd)//', tout_ahd: '//str(tout_ahd))

  call echo(code%ext)
  !!!-----------------------------------------------------------
  !!! nml_geo
  !!!-----------------------------------------------------------
  call echo(code%ent, 'Reading nml_geo')

  nlon = 0
  nlat = 0
  west = 0.d0
  east = 3.6d2
  south = -9.d1
  north = 9.d1
  is_south_to_north = .true.
  f_elv = ''
  elv_miss = -9999.d0

  rewind(un_conf)
  read(un_conf, nml=nml_geo, iostat=ios)
  selectcase( ios )
  case( 0 )
    continue
  case( -1 )
    continue
  case default
    rewind(un_conf)
    read(un_conf,nml=nml_geo)
  endselect

  geo%nlon = nlon
  geo%nlat = nlat
  geo%west = west
  geo%east = east
  geo%south = south
  geo%north = north
  geo%is_south_to_north = is_south_to_north
  geo%f_elv = file(f_elv, DTYPE_REAL, ENDIAN_LITTLE, 1)
  geo%elv_miss = elv_miss

  call set_geo(geo)

  call echo(code%ext)
  !!!-----------------------------------------------------------
  !!! nml_parameter_common
  !!!-----------------------------------------------------------
  call echo(code%ent, 'Reading nml_common')

  ticld_bhd = TINC_BHD_DEFAULT
  ticld_ahd = TINC_AHD_DEFAULT
  nmax  = NMAX_DEFAULT
  rho_h = RHO_H_DEFAULT
  rho_v = RHO_V_DEFAULT
  dnst_thresh = DNST_THRESH_DEFAULT
  use_all_q0 = USE_ALL_Q0_DEFAULT
  allocate(wgt_qlt(-1:UB_WGT_QLT-1))
  wgt_qlt(:)     = WGT_QLT_DEFAULT(:)
  wgt_same_sid   = WGT_SAME_SID_DEFAULT
  nmax_same_sid  = NMAX_SAME_SID_DEFAULT
  wgt_same_said  = WGT_SAME_SAID_DEFAULT
  nmax_same_said = NMAX_SAME_SAID_DEFAULT
  opt_sfc  = OPT_SFC_DEFAULT
  opt_Tv   = OPT_TV_DEFAULT
  opt_Qerr = OPT_QERR_DEFAULT
  lapse_rate = LAPSE_RATE_DEFAULT
  obserr_scale = 1.d0
  f_obserr = ''

  rewind(un_conf)
  read(un_conf, nml=nml_common)

  ticld_bhd_common = ticld_bhd
  ticld_ahd_common = ticld_ahd
  nmax_common = nmax
  rho_h_common = rho_h
  rho_v_common = rho_v
  dnst_thresh_common = dnst_thresh
  use_all_q0_common = use_all_q0
  allocate(wgt_qlt_common(-1:UB_WGT_QLT-1))
  wgt_qlt_common(:) = wgt_qlt(:)
  wgt_same_sid_common   = wgt_same_sid
  nmax_same_sid_common  = nmax_same_sid
  wgt_same_said_common  = wgt_same_said
  nmax_same_said_common = nmax_same_said
  opt_sfc_common  = opt_sfc
  opt_Tv_common   = opt_Tv
  opt_Qerr_common = opt_Qerr
  lapse_rate_common = lapse_rate
  obserr_scale_common = obserr_scale

  call edbg('ticld_bhd: '//str(ticld_bhd)//', ticld_ahd: '//str(ticld_ahd))
  call edbg('nmax: '//str(nmax))
  call edbg('rho_h: '//str(rho_h)//', rho_v: '//str(rho_v))
  call edbg('dnst_thresh: '//str(dnst_thresh,'es9.2'))
  call edbg('use_all_q0: '//str(use_all_q0))
  call edbg('wgt_qlt(-1:'//str(UB_WGT_QLT-1)//'): '//str(wgt_qlt,'es9.2'))
  call edbg('wgt_same_sid : '//str(wgt_same_sid)//&
            ', nmax_same_sid : '//str(nmax_same_sid))
  call edbg('wgt_same_said: '//str(wgt_same_said)//&
            ', nmax_same_said: '//str(nmax_same_said))
  call edbg('opt_sfc : '//str(opt_sfc)//' ('//str(str_opt_sfc(opt_sfc))//')')
  call edbg('opt_Tv  : '//str(opt_Tv)//' ('//str(str_opt_Tv(opt_Tv))//')')
  call edbg('opt_Qerr: '//str(opt_Qerr)//' ('//str(str_opt_Qerr(opt_Qerr))//')')
  call edbg('lapse_rate: '//str(lapse_rate,'es9.2'))
  call edbg('obserr_scale: '//str(obserr_scale,'es9.2'))
  call edbg('f_obserr: '//str(f_obserr))

  allocate(obserr(0:NOBSERR))
  obserr(0) = 0.d0
  obserr(VIDX_OBSERR_U) = OBSERR_U_DEFAULT
  obserr(VIDX_OBSERR_V) = OBSERR_V_DEFAULT
  obserr(VIDX_OBSERR_T) = OBSERR_T_DEFAULT
  obserr(VIDX_OBSERR_Q) = OBSERR_Q_DEFAULT
  obserr(VIDX_OBSERR_PS) = OBSERR_PS_DEFAULT

  if( f_obserr /= '' )then
    un_obserr = unit_number()
    open(un_obserr, file=f_obserr, status='old')

    do
      read(un_obserr,*,iostat=ios) ex
      selectcase( ios )
      case( 0 )
        continue
      case( -1 )
        exit
      case default
        call eerr('An error occured while reading '//str(f_obserr))
      endselect

      if( .not. ex ) cycle

      backspace(un_obserr)
      read(un_obserr,*) ex, id_var, r8_

      var = id_to_var_obs(id_var, allow_not_found=.true.)
      if( var == '' ) cycle

      vidx = var_to_idx_obserr(var, allow_not_found=.true.)
      if( vidx == 0 ) cycle

      obserr(vidx) = r8_
    enddo
  endif

  dgt_var = max(dgt(VAR_U), dgt(VAR_V), dgt(VAR_T), dgt(VAR_Q), dgt(VAR_PS))
  dgt_id_var = dgt(max(ID_U_OBS, ID_V_OBS, ID_T_OBS, ID_Q_OBS, &
                       ID_PS_OBS, ID_RAIN_OBS))

  do vidx_obserr = 1, NOBSERR
    var = idx_obserr_to_var(vidx_obserr)
    id_var = var_to_id_obs(var, allow_not_found=.false.)
    call edbg('  '//str(var,dgt_var)//&
              ' ('//str(id_var,dgt_id_var)//'): '//&
              str(obserr(vidx_obserr),'es9.2'))
  enddo

  call echo(code%ext)
  !!!-----------------------------------------------------------
  !!! nml_constant
  !!!-----------------------------------------------------------
  call echo(code%ent, 'Reading nml_constant')

  earth_r = EARTH_WGS84ELLIPS_R_AUTHALIC * 1d-3  ! km

  rewind(un_conf)
  read(un_conf, nml=nml_constant)
  call edbg('earth_r: '//str(earth_r,'es15.8'))

  call echo(code%ext)
  !!!-----------------------------------------------------------
  !!! nml_msg
  !!!-----------------------------------------------------------
  call echo(code%ent, 'Reading nml_msg')

  rewind(un_conf)
  norder = 0
  do
    read(un_conf, nml=nml_msg, iostat=ios)
    selectcase( ios )
    case( 0 )
      norder = norder + 1
    case( -1 )
      exit
    case default
      call eerr('An error occured while reading NML_ORDER # '//str(norder+1)//&
                '. Please check the name of variables')
    endselect
  enddo

  if( norder > 0 )then
    allocate(lst_order(norder))

    rewind(un_conf)
    do iorder = 1, norder
      order => lst_order(iorder)
      order%nvar = 0
      order%use_all_q0 = .false.

      typ    = ''
      method = ''
      vars   = ''
      ticld_bhd = ticld_bhd_common
      ticld_ahd = ticld_ahd_common
      nmax = nmax_common
      rho_h = rho_h_common
      rho_v = rho_v_common
      dnst_thresh = dnst_thresh_common
      use_all_q0 = use_all_q0_common
      wgt_qlt(:) = wgt_qlt_common(:)
      nmax_same_sid  = nmax_same_sid_common
      wgt_same_sid   = wgt_same_sid_common
      nmax_same_said = nmax_same_said_common
      wgt_same_said  = wgt_same_said_common
      opt_sfc  = opt_sfc_common
      opt_Tv   = opt_Tv_common
      opt_Qerr = opt_Qerr_common
      lapse_rate = lapse_rate_common
      obserr_scale = obserr_scale_common

      read(un_conf, nml=nml_msg)

      ! msgtyp
      !---------------------------------------------------------
      if( typ_to_idx_msgtyp(typ) == 0 )then
        call eerr('Value of MSGTYP in NML_ORDER # '//str(iorder)//&
                  ' is invalid: '//str(typ))
      endif
      order%msgtyp = typ

      ! is_surface
      !---------------------------------------------------------
      order%is_surface = is_surface(order%msgtyp)

      ! method
      !---------------------------------------------------------
      selectcase( lower(method) )
      case( '' )
        call eerr('METHOD was not specified in NML_ORDER # '//str(iorder))
      case( METHOD_ALL , &
            METHOD_WGT1, &
            METHOD_DNST1, METHOD_DNST2, &
            METHOD_NONE )
        continue
      case default
        call eerr('Value of METHOD in NML_ORDER # '//str(iorder)//&
                  ' is invalid: '//str(method))
      endselect
      order%method = lower(method)

      ! vars.
      !---------------------------------------------------------
      if( vars /= '' )then
        vars = adjustl(vars)

        order%nvar = 0
        is_space    = .true.
        do i = 1, len_trim(vars)
          if( vars(i:i) == '' )then
            is_space = .true.
          else
            if( is_space )then
              order%nvar = order%nvar + 1
              is_space = .false.
            endif
          endif
        enddo

        allocate(order%vidx(order%nvar))

        if( allocated(lst_var) ) deallocate(lst_var)
        allocate(lst_var(order%nvar))
        read(vars,*) lst_var

        do ivar = 1, order%nvar
          order%vidx(ivar) = var_to_idx_obs(lst_var(ivar), nobs, order%is_surface)
        enddo
      endif

      ! treatment of same ids
      !---------------------------------------------------------
      order%code_same_id = CODE_SAME_ID__NONE

      if( wgt_same_sid < 1.d0 )then
        order%code_same_id = CODE_SAME_ID__SID_REDUCE
      elseif( nmax_same_sid > 0 )then
        order%code_same_id = CODE_SAME_ID__SID_LIMIT
      elseif( wgt_same_said < 1.d0 )then
        order%code_same_id = CODE_SAME_ID__SAID_REDUCE
      elseif( nmax_same_said > 0 )then
        order%code_same_id = CODE_SAME_ID__SAID_LIMIT
      endif
      !---------------------------------------------------------
      ! Set values
      !---------------------------------------------------------
      order%ticld_bhd = ticld_bhd
      order%ticld_ahd = ticld_ahd
      order%nmax = nmax
      order%rho_h = rho_h
      order%rho_v = rho_v
      order%dist_h_thresh = order%rho_h * (2.d0*sqrt(10.d0/3))
      order%dist_v_thresh = order%rho_v * (2.d0*sqrt(10.d0/3))
      order%dnst_thresh = dnst_thresh
      order%use_all_q0 = use_all_q0
      allocate(order%wgt_qlt(-1:UB_WGT_QLT))
      order%wgt_qlt(:UB_WGT_QLT-1) = wgt_qlt(:)
      order%wgt_qlt(UB_WGT_QLT) = 0.d0
      order%wgt_same_sid   = wgt_same_sid
      order%nmax_same_sid  = nmax_same_sid
      order%wgt_same_said  = wgt_same_said
      order%nmax_same_said = nmax_same_said
      order%opt_sfc  = opt_sfc
      order%opt_Tv   = opt_Tv
      order%opt_Qerr = opt_Qerr
      order%lapse_rate = lapse_rate
      order%obserr_scale = obserr_scale
    enddo  ! iorder/

    if( allocated(lst_var) ) deallocate(lst_var)

    do iorder = 1, norder
      order => lst_order(iorder)
      vars = ''
      do ivar = 1, order%nvar
        vars = trim(vars)//' '//idx_to_var_obs(order%vidx(ivar))
      enddo
      vars = adjustl(vars)

      call show_order(order, iorder, norder)
    enddo
  endif

  call echo(code%ext)
  !!!-----------------------------------------------------------
  !!! nml_monitor
  !!!-----------------------------------------------------------
  call echo(code%ent, 'Reading nml_monitor')

  monit_proc = .false.
  monit_itr_sct_rec = .false.
  monit_itr_add_rec = .false.
  save_modif = .false.
  save_monit = .false.
  dir_monit = DIR_MONIT_DEFAULT

  rewind(un_conf)
  read(un_conf, nml=nml_monitor)

  monit%monit_proc = monit_proc
  monit%monit_itr_sct_rec = monit_itr_sct_rec
  monit%monit_itr_add_rec = monit_itr_add_rec
  monit%save_modif = save_modif
  monit%save_monit = save_monit
  monit%dir = dir_monit

  call edbg('monit_proc       : '//str(monit_proc))
  call edbg('monit_itr_sct_rec: '//str(monit_itr_sct_rec))
  call edbg('monit_itr_add_rec: '//str(monit_itr_add_rec))
  call edbg('save_modif       : '//str(save_modif))
  call edbg('save_monit       : '//str(save_monit))
  call edbg('dir_monit        : '//str(dir_monit))

  monit%opt_log_proc = ''
  if( .not. monit%monit_proc )then
    monit%opt_log_proc = '-p -x2'
    call set_mod_pb__opt_log_proc(monit%opt_log_proc)
  endif

  call echo(code%ext)
  !-------------------------------------------------------------
  close(un_conf)

  call echo(code%ext)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine read_conf
!===============================================================
!
!===============================================================
logical function is_surface(msgtyp)
  use mod_base, only: &
        typ_to_id_msgtyp
  implicit none
  character(*), intent(in) :: msgtyp

  call echo(code%bgn, 'is_surface', '-p')
  !-------------------------------------------------------------
  if( typ_to_id_msgtyp(msgtyp) == 0 )then
    call eerr('Invalid value in $msgtyp: '//str(msgtyp))
  endif

  selectcase( msgtyp )
  case( MSGTYP_ADPSFC, MSGTYP_SFCSHP, MSGTYP_SFCBOG )
    is_surface = .true.
  case default
    is_surface = .false.
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function is_surface
!===============================================================
!
!===============================================================
subroutine show_order(order, iorder, norder)
  use mod_base, only: &
        idx_to_var_obs, &
        str_opt_Tv    , &
        str_opt_Qerr  , &
        str_opt_sfc
  implicit none
  type(order_), intent(in) :: order
  integer, intent(in) :: iorder
  integer, intent(in) :: norder

  character(:), allocatable :: vars
  character(CLEN_MNNC) :: var
  integer :: loc0
  integer :: ivar

  allocate(character(CLEN_MNNC*order%nvar) :: vars)
  loc0 = 0
  do ivar = 1, order%nvar
    var = idx_to_var_obs(order%vidx(ivar), swap_p_ps=order%is_surface)
    vars(loc0+1:loc0+len_trim(var)) = trim(var)
    loc0 = loc0 + len_trim(var) + 1
    vars(loc0:loc0) = ' '
  enddo
  if( loc0 < len(vars) ) vars(loc0:) = ''

  call edbg('order('//str(iorder,dgt(norder))//')'//&
          '\n  typ        : '//str(order%msgtyp)//&
          '\n  method     : '//str(order%method)//&
          '\n  vars       : '//str(trim(vars))//&
          '\n  nmax       : '//str(order%nmax)//&
          '\n  use_all_q0 : '//str(order%use_all_q0)//&
          '\n  rho_h      : '//str(order%rho_h,'es9.2')//&
          '\n  rho_v      : '//str(order%rho_v,'es9.2')//&
          '\n  wgt_qlt    : '//str(order%wgt_qlt(:UB_WGT_QLT-1),'f6.3')//&
          '\n  dnst_thresh: '//str(order%dnst_thresh,'es9.2')//&
          '\n  wgt_same_sid  : '//str(order%wgt_same_sid,'es9.2')//&
          '\n  nmax_same_sid : '//str(order%nmax_same_sid)//&
          '\n  wgt_same_said : '//str(order%wgt_same_said,'es9.2')//&
          '\n  nmax_same_said: '//str(order%nmax_same_said)//&
          '\n  opt_sfc : '//str(order%opt_sfc)//&
             ' ('//str(str_opt_sfc(order%opt_sfc))//')'//&
          '\n  opt_Tv  : '//str(order%opt_Tv)//&
             ' ('//str(str_opt_Tv(order%opt_Tv))//')'//&
          '\n  opt_Qerr: '//str(order%opt_Qerr)//&
             ' ('//str(str_opt_Qerr(order%opt_Qerr))//')'//&
          '\n  lapse_rate: '//str(order%lapse_rate,'es9.2')//&
          '\n  obserr_scale: '//str(order%obserr_scale,'es9.2'))
end subroutine show_order
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
subroutine set_geo(geo)
  implicit none
  type(model_), intent(inout) :: geo

  real(8) :: lonrange, latrange
  integer :: ilon, ilat

  call echo(code%bgn, 'set_geo', '-p -x2')
  !-------------------------------------------------------------
  if( geo%west == geo%east .or. abs(geo%east - geo%west) == 3.6d2 )then
    lonrange = 3.6d2
  else
    lonrange = londiff_deg(geo%west, geo%east)
  endif
  latrange = geo%north - geo%south
  !-------------------------------------------------------------
  ! Grid size
  !-------------------------------------------------------------
  geo%lonsize = lonrange / geo%nlon
  geo%latsize = latrange / geo%nlat
  !-------------------------------------------------------------
  ! Grid lines
  !-------------------------------------------------------------
  allocate(geo%lonb(0:geo%nlon))
  allocate(geo%latb(0:geo%nlat))

  do ilon = 0, geo%nlon-1
    geo%lonb(ilon) = geo%west + lonrange*ilon/geo%nlon
  enddo
  geo%lonb(geo%nlon) = geo%east

  do ilon = 0, geo%nlon
    if( geo%lonb(ilon) < 0.d0 )then
      call add(geo%lonb(ilon), 3.6d2)
    elseif( geo%lonb(ilon) >= 3.6d2 )then
      call add(geo%lonb(ilon), -3.6d2)
    endif
  enddo

  do ilat = 0, geo%nlat-1
    geo%latb(ilat) = geo%south + latrange*ilat/geo%nlat
  enddo
  geo%latb(geo%nlat) = geo%north
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_geo
!===============================================================
!
!===============================================================
end module mod_set
