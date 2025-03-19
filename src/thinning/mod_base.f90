module mod_base
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
  use lib_io
  use def_const_pb
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: get_s_time
  public :: get_f_obs

  public :: id_to_var_obs
  public :: var_to_id_obs
  public :: mnc_to_var_obs
  public :: var_to_mnc_obs
  public :: idx_to_var_obs
  public :: var_to_idx_obs
  public :: var_to_idx_obserr
  public :: idx_obserr_to_var
  public :: id_to_typ_msgtyp
  public :: typ_to_id_msgtyp

  public :: str_opt_sfc
  public :: str_opt_Tv
  public :: str_opt_Qerr
  !-------------------------------------------------------------
  ! Private Module Variables
  !-------------------------------------------------------------
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
character(6) function get_s_time(time) result(s)
  implicit none
  integer, intent(in) :: time

  if( time == 0 )then
    s = '00'
  elseif( time < 0 )then
    s = 'prev'//str(-time,-2)
  else
    s = 'next'//str(time,-2)
  endif
end function get_s_time
!===============================================================
!
!===============================================================
subroutine get_f_obs(f_obs_template, f_obs, itime, init)
  implicit none
  character(*), intent(in)  :: f_obs_template
  character(*), intent(out) :: f_obs
  integer     , intent(in)  :: itime
  logical     , intent(in)  :: init

  integer :: access

  call echo(code%bgn, 'get_f_obs', '-p -x2')
  !-------------------------------------------------------------
  f_obs = trim(f_obs_template)//'.'//trim(get_s_time(itime))

  if( init )then
    call mkdir(dirname(f_obs))
    call remove(f_obs)
  else
    if( access(f_obs,' ') /= 0 )then
      call make_empty_file(f_obs)
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine get_f_obs
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
character(CLEN_MNNC) function id_to_var_obs(&
    id, swap_p_ps, allow_not_found) result(var)
  implicit none
  integer, intent(in) :: id
  logical, intent(in), optional :: swap_p_ps
  logical, intent(in), optional :: allow_not_found

  logical :: swap_p_ps_
  logical :: allow_not_found_

  call echo(code%bgn, 'id_to_var_obs', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  swap_p_ps_ = .false.
  allow_not_found_ = .false.
  if( present(swap_p_ps) ) swap_p_ps_ = swap_p_ps
  if( present(allow_not_found) ) allow_not_found_ = allow_not_found
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( id )
  case( ID_U_OBS    ); var = VAR_U
  case( ID_V_OBS    ); var = VAR_V
  case( ID_T_OBS    ); var = VAR_T
  case( ID_Q_OBS    ); var = VAR_Q
  case( ID_RH_OBS   )
    if( .not. allow_not_found_ )then
      call eerr('$id_rh_obs was input. '//&
                'Variable name for relative humidity not exist')
    else
      var = ''
    endif
  case( ID_PS_OBS   )
    if( swap_p_ps_ )then
      var = VAR_P
    else
      var = VAR_PS
    endif
  case( ID_RAIN_OBS ); var = VAR_RAIN
  case default
    call eerr('Invalid value in $id: '//str(id))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function id_to_var_obs
!===============================================================
!
!===============================================================
integer function var_to_id_obs(&
    var, swap_p_ps, allow_not_found) result(id)
  implicit none
  character(*), intent(in) :: var
  logical, intent(in), optional :: swap_p_ps
  logical, intent(in), optional :: allow_not_found

  logical :: swap_p_ps_
  logical :: allow_not_found_

  call echo(code%bgn, 'var_to_id_obs', '-p')
  !-------------------------------------------------------------
  swap_p_ps_ = .false.
  allow_not_found_ = .false.
  if( present(swap_p_ps) ) swap_p_ps_ = swap_p_ps
  if( present(allow_not_found) ) allow_not_found_ = allow_not_found

  selectcase( var )
  case( VAR_P )
    if( swap_p_ps_ )then
      id = ID_PS_OBS
    else
      if( .not. allow_not_found_ )then
        call eerr('Variable "'//str(var)//'" is invalid when'//&
                  ' $swap_p_ps_ is False.')
      else
        id = 0
      endif
    endif
  case( VAR_Q )
    id = ID_Q_OBS
  case( VAR_T )
    id = ID_T_OBS
  case( VAR_U )
    id = ID_U_OBS
  case( VAR_V )
    id = ID_V_OBS
  case( VAR_PS )
    if( swap_p_ps_ )then
      if( .not. allow_not_found_ )then
        call eerr('Variable "'//str(var)//'" is invalid when'//&
                  ' $swap_p_ps_ is True.')
      else
        id = 0
      endif
    else
      id = ID_PS_OBS
    endif
  case default
    if( .not. allow_not_found_ )then
      call eerr('Invalid value in $var: '//str(var))
    else
      id = 0
    endif
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function var_to_id_obs
!===============================================================
!
!===============================================================
character(CLEN_MNNC) function mnc_to_var_obs(mnc, swap_p_ps) result(var)
  implicit none
  character(*), intent(in) :: mnc
  logical, intent(in), optional :: swap_p_ps

  logical :: swap_p_ps_

  call echo(code%bgn, 'mnc_to_var_obs', '-p')
  !-------------------------------------------------------------
  swap_p_ps_ = .false.
  if( present(swap_p_ps) ) swap_p_ps_ = swap_p_ps

  selectcase( mnc )
  case( VMNC_U_OBS    ); var = VAR_U
  case( VMNC_V_OBS    ); var = VAR_V
  case( VMNC_T_OBS    ); var = VAR_T
  case( VMNC_Q_OBS    ); var = VAR_Q
  case( VMNC_P_OBS    ); var = VAR_P
  case( VMNC_Z_OBS    ); var = VAR_Z
  case( VMNC_PW_OBS   ); var = VAR_PW
  case( VMNC_PS_OBS   ); var = VAR_PS
  case( VMNC_RAIN_OBS ); var = VAR_RAIN
  case default
    call eerr('Invalid value in $mnc: '//str(mnc))
  endselect

  if( swap_p_ps_ )then
    if( var == VAR_P )then
      var = VAR_PS
    elseif( var == VAR_PS )then
      var = VAR_P
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end function mnc_to_var_obs
!===============================================================
!
!===============================================================
character(CLEN_MNNC) function var_to_mnc_obs(&
    var, swap_p_ps, allow_not_found) result(mnc)
  implicit none
  character(*), intent(in) :: var
  logical, intent(in), optional :: swap_p_ps
  logical, intent(in), optional :: allow_not_found

  logical :: allow_not_found_
  logical :: swap_p_ps_

  call echo(code%bgn, 'var_to_mnc_obs', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  swap_p_ps_ = .false.
  allow_not_found_ = .false.
  if( present(swap_p_ps) ) swap_p_ps_ = swap_p_ps
  if( present(allow_not_found) ) allow_not_found_ = allow_not_found
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( var )
  case( VAR_U    ); mnc = VMNC_U_OBS
  case( VAR_V    ); mnc = VMNC_V_OBS
  case( VAR_T    ); mnc = VMNC_T_OBS
  case( VAR_Q    ); mnc = VMNC_Q_OBS
  case( VAR_Z    ); mnc = VMNC_Z_OBS
  case( VAR_P    ); mnc = VMNC_P_OBS
  case( VAR_PS   ); mnc = VMNC_PS_OBS
  case( VAR_PW   ); mnc = VMNC_PW_OBS
  case( VAR_RAIN ); mnc = VMNC_RAIN_OBS
  case default
    call eerr('Invalid value in $var: '//trim(var))
  endselect

  if( swap_p_ps_ )then
    if( mnc == VMNC_P_OBS )then
      mnc = VMNC_PS_OBS
    elseif( mnc == VMNC_PS_OBS )then
      mnc = VMNC_P_OBS
    endif
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end function var_to_mnc_obs
!===============================================================
!
!===============================================================
character(CLEN_MNNC) function idx_to_var_obs(idx, swap_p_ps) result(var)
  use mod_pb, only: &
        idx_to_mnc_obs
  implicit none
  integer, intent(in) :: idx
  logical, intent(in), optional :: swap_p_ps

  logical :: swap_p_ps_
  character(CLEN_MNNC) :: mnc

  call echo(code%bgn, 'idx_to_var_obs', '-p')
  !-------------------------------------------------------------
  swap_p_ps_ = .false.
  if( present(swap_p_ps) ) swap_p_ps_ = swap_p_ps

  mnc = idx_to_mnc_obs(idx)
  var = mnc_to_var_obs(mnc, swap_p_ps=swap_p_ps_)
  !-------------------------------------------------------------
  call echo(code%ret)
end function idx_to_var_obs
!===============================================================
!
!===============================================================
integer function var_to_idx_obs(&
    var, nobs, swap_p_ps, allow_not_found) result(idx)
  use mod_pb, only: &
        mnc_to_idx_obs
  implicit none
  character(*), intent(in) :: var
  integer, intent(in), optional :: nobs
  logical, intent(in), optional :: swap_p_ps
  logical, intent(in), optional :: allow_not_found

  logical :: swap_p_ps_
  logical :: allow_not_found_

  character(CLEN_MNNC) :: mnc

  call echo(code%bgn, 'var_to_idx_obs', '-p')
  !-------------------------------------------------------------
  swap_p_ps_ = .false.
  allow_not_found_ = .false.
  if( present(swap_p_ps) ) swap_p_ps_ = swap_p_ps
  if( present(allow_not_found) ) allow_not_found_ = allow_not_found

  selectcase( var )
  case( VAR_P, VAR_T, VAR_Q, VAR_Z, VAR_U, VAR_V, &
        VAR_PS, VAR_PW, VAR_RAIN )
    mnc = var_to_mnc_obs(&
            var, swap_p_ps=swap_p_ps_, allow_not_found=allow_not_found_)
    idx = mnc_to_idx_obs(&
            mnc, allow_not_found=allow_not_found_)
  case( VAR_TV )
    if( .not. present(nobs) )then
      call eerr('Input of argument $nobs is required when $var is "'//str(var)//'".')
    endif
    idx = nobs + 1
  case default
    call eerr('Invalid value in $var: '//str(var))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function var_to_idx_obs
!===============================================================
!
!===============================================================
integer function var_to_idx_obserr(&
    var, swap_p_ps, allow_not_found) result(idx)
  implicit none
  character(*), intent(in) :: var
  logical, intent(in), optional :: swap_p_ps
  logical, intent(in), optional :: allow_not_found

  logical :: swap_p_ps_
  logical :: allow_not_found_

  call echo(code%bgn, 'var_to_idx_obserr', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  swap_p_ps_ = .false.
  allow_not_found_ = .false.
  if( present(swap_p_ps) ) swap_p_ps_ = swap_p_ps
  if( present(allow_not_found) ) allow_not_found_ = allow_not_found
  !-------------------------------------------------------------
  selectcase( var )
  case( VAR_U  ); idx = VIDX_OBSERR_U
  case( VAR_V  ); idx = VIDX_OBSERR_V
  case( VAR_T  ); idx = VIDX_OBSERR_T
  case( VAR_Q  ); idx = VIDX_OBSERR_Q
  case( VAR_PS )
    if( swap_p_ps_ )then
      if( allow_not_found_ )then
        idx = 0
      else
        call eerr('Obserr is not defined for variable "'//str(VAR_P)//'"'//&
                '\n  var: '//str(var)//&
                '\n  swap_p_ps: '//str(swap_p_ps_))
      endif
    else
      idx = VIDX_OBSERR_PS
    endif
  case( VAR_P )
    if( swap_p_ps_ )then
      idx = VIDX_OBSERR_PS
    else
      if( allow_not_found_ )then
        idx = 0
      else
        call eerr('Obserr is not defined for variable "'//str(VAR_P)//'"'//&
                '\n  var: '//str(var)//&
                '\n  swap_p_ps: '//str(swap_p_ps_))
      endif
    endif
  case( VAR_Z, VAR_PW, VAR_RAIN )
    if( allow_not_found_ )then
      idx = 0
    else
      call eerr('Obserr is not defined for variable "'//str(var)//'"')
    endif
  case default
    call eerr('Invalid value in $var: '//str(var))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function var_to_idx_obserr
!===============================================================
!
!===============================================================
character(CLEN_MNNC) function idx_obserr_to_var(idx) result(var)
  implicit none
  integer, intent(in) :: idx

  call echo(code%bgn, 'idx_obserr_to_var', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( idx )
  case( VIDX_OBSERR_U  ); var = VAR_U
  case( VIDX_OBSERR_V  ); var = VAR_V
  case( VIDX_OBSERR_T  ); var = VAR_T
  case( VIDX_OBSERR_Q  ); var = VAR_Q
  case( VIDX_OBSERR_PS ); var = VAR_PS
  case default
    call eerr('Invalid value in $idx: '//str(idx))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function idx_obserr_to_var
!===============================================================
!
!===============================================================
integer function typ_to_id_msgtyp(msgtyp) result(id)
  implicit none
  character(*), intent(in) :: msgtyp

  selectcase( msgtyp )
  case( MSGTYP_ADPUPA ); id = ityp_ADPUPA_obs
  case( MSGTYP_AIRCAR ); id = ityp_AIRCAR_obs
  case( MSGTYP_AIRCFT ); id = ityp_AIRCFT_obs
  case( MSGTYP_SATWND ); id = ityp_SATWND_obs
  case( MSGTYP_PROFLR ); id = ityp_PROFLR_obs
  case( MSGTYP_VADWND ); id = ityp_VADWND_obs
  case( MSGTYP_SATEMP ); id = ityp_SATEMP_obs
  case( MSGTYP_ADPSFC ); id = ityp_ADPSFC_obs
  case( MSGTYP_SFCSHP ); id = ityp_SFCSHP_obs
  case( MSGTYP_SFCBOG ); id = ityp_SFCBOG_obs
  case( MSGTYP_SPSSMI ); id = ityp_SPSSMI_obs
  case( MSGTYP_SYNDAT ); id = ityp_SYNDAT_obs
  case( MSGTYP_ERS1DA ); id = ityp_ERS1DA_obs
  case( MSGTYP_GOESND ); id = ityp_GOESND_obs
  case( MSGTYP_QKSWND ); id = ityp_QKSWND_obs
  case( MSGTYP_MSONET ); id = ityp_MSONET_obs
  case( MSGTYP_GPSIPW ); id = ityp_GPSIPW_obs
  case( MSGTYP_RASSDA ); id = ityp_RASSDA_obs
  case( MSGTYP_WDSATR ); id = ityp_WDSATR_obs
  case( MSGTYP_ASCATW ); id = ityp_ASCATW_obs
  case default    ; id = 0
  endselect
end function typ_to_id_msgtyp
!===============================================================
!
!===============================================================
character(CLEN_MNNC) function id_to_typ_msgtyp(id) result(typ)
  implicit none
  integer, intent(in) :: id

  selectcase( id )
  case( ityp_ADPUPA_obs ); typ = MSGTYP_ADPUPA
  case( ityp_AIRCAR_obs ); typ = MSGTYP_AIRCAR
  case( ityp_AIRCFT_obs ); typ = MSGTYP_AIRCFT
  case( ityp_SATWND_obs ); typ = MSGTYP_SATWND
  case( ityp_PROFLR_obs ); typ = MSGTYP_PROFLR
  case( ityp_VADWND_obs ); typ = MSGTYP_VADWND
  case( ityp_SATEMP_obs ); typ = MSGTYP_SATEMP
  case( ityp_ADPSFC_obs ); typ = MSGTYP_ADPSFC
  case( ityp_SFCSHP_obs ); typ = MSGTYP_SFCSHP
  case( ityp_SFCBOG_obs ); typ = MSGTYP_SFCBOG
  case( ityp_SPSSMI_obs ); typ = MSGTYP_SPSSMI
  case( ityp_SYNDAT_obs ); typ = MSGTYP_SYNDAT
  case( ityp_ERS1DA_obs ); typ = MSGTYP_ERS1DA
  case( ityp_GOESND_obs ); typ = MSGTYP_GOESND
  case( ityp_QKSWND_obs ); typ = MSGTYP_QKSWND
  case( ityp_MSONET_obs ); typ = MSGTYP_MSONET
  case( ityp_GPSIPW_obs ); typ = MSGTYP_GPSIPW
  case( ityp_RASSDA_obs ); typ = MSGTYP_RASSDA
  case( ityp_WDSATR_obs ); typ = MSGTYP_WDSATR
  case( ityp_ASCATW_obs ); typ = MSGTYP_ASCATW
  case default; typ = ''
  endselect
end function id_to_typ_msgtyp
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
character(64) function str_opt_sfc(opt_sfc) result(s)
  implicit none
  integer, intent(in) :: opt_sfc

  call echo(code%bgn, 'str_opt_sfc', '-p -x2')
  !-------------------------------------------------------------
  selectcase( opt_sfc )
  case( OPT_SFC__NOT_MODIFY )
    s = 'not modify'
  case( OPT_SFC__MODIFY )
    s = 'modify'
  case default
    call eerr('Invalid value in $opt_sfc: '//str(opt_sfc))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_opt_sfc
!===============================================================
!
!===============================================================
character(64) function str_opt_Tv(opt) result(s)
  implicit none
  integer, intent(in) :: opt

  call echo(code%bgn, 'str_opt_Tv', '-p -x2')
  !-------------------------------------------------------------
  selectcase( opt )
  case( OPT_TV__DO_NOT_USE )
    s = 'do not use'
  case( OPT_TV__CONV_TO_T_Q0 )
    s = 'conv to T and use it as the value of quality 0'
  case( OPT_TV__CONV_TO_T_Q1 )
    s = 'conv to T and use it as the value of quality 1'
  case( OPT_TV__CONV_TO_T_Q2 )
    s = 'conv to T and use it as the value of quality 2'
  case( OPT_TV__CONV_TO_T_Q3 )
    s = 'conv to T and use it as the value of quality 3'
  case( OPT_TV__CONV_TO_T_Q4 )
    s = 'conv to T and use it as the value of quality 4'
  case( OPT_TV__USE_AS_TV )
    s = 'use Tv as Tv'
  case default
    call eerr('Invalid value in $opt: '//str(opt))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_opt_Tv
!===============================================================
!
!===============================================================
character(64) function str_opt_Qerr(opt_Qerr) result(s)
  implicit none
  integer, intent(in) :: opt_Qerr

  call echo(code%bgn, 'str_opt_Qerr', '-p -x2')
  !-------------------------------------------------------------
  selectcase( opt_Qerr )
  case( OPT_QERR__CONV_RHERR )
    s = 'conv to RHerr'
  case( OPT_QERR__DO_NOT_USE )
    s = 'do not use'
  case default
    call eerr('Invalid value in $opt_Qerr: '//str(opt_Qerr))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_opt_Qerr
!===============================================================
!
!===============================================================
end module mod_base
