module mod_main
  use lib_const
  use lib_log
  ! speedy
  use common_XXXXXX, only: &
        nlev, &
        plv
  use common_obs_XXXXXX, only: &
        ntyp_obs
  ! prepbufr
  use def_const_pb
  use def_type_pb
  ! thinning
  use def_const, only: &
        VAR_U, VAR_V, VAR_T, VAR_Q, VAR_P, VAR_Z, &
        VAR_PW, VAR_PS, VAR_RAIN, VAR_TV
  implicit none
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------

  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  real(8), parameter :: RHO_V = 1d-1
  real(8), parameter :: THRESH_DIFF_LOGP = RHO_V * (2.d0/sqrt(10.d0/3))

  integer, parameter :: klon = 3600
  integer, parameter :: klat = 1800
  real(8), parameter :: klonsize = 3.6d2 / klon
  real(8), parameter :: klatsize = 1.8d2 / klat

  real(8), allocatable :: logp(:)  ! [log(hPa)]

  integer :: nhdr, nobs, noer, nqmk, npcd, nrcd
  integer :: nvar
  integer :: vidx_P, vidx_Z, vidx_T, vidx_Q, vidx_U, vidx_V, vidx_Tv

  character(16) :: opt_log = '-p -x2'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine run()
  use lib_io
  use common_XXXXXX, only: &
        set_common_XXXXXX
  ! prepbufr
  use mod_pb, only: &
        read_prepbufr_dumped, &
        set_mod_pb__opt_log_proc
  ! thinning
  use mod_base, only: &
        idx_to_var_obs, &
        id_to_typ_msgtyp
  implicit none
  type(prepbufr_) :: pb
  character(CLEN_MNNC) :: msgtyp
  character(CLEN_PATH) :: fin_data_template
  character(CLEN_PATH) :: dirout_obs
  character(CLEN_PATH) :: fout_point, fout_grid
  character(CLEN_MNNC) :: var
  integer :: ilev
  integer :: vidx

  call echo(code%bgn, 'run', opt_log)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fin_data_template = argument(1)
  dirout_obs = argument(2)
  msgtyp = argument(3)

  call edbg(str(dirout_obs))
  call mkdir(dirout_obs)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call set_common_XXXXXX
  allocate(logp(nlev))
  logp = log(plv*1d-2)

  call set_consts()

  call set_mod_pb__opt_log_proc('-a')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pb%nmsg = 0
  call read_prepbufr_dumped(fin_data_template, msgtyp, pb)

  do vidx = 1, nvar
    var = idx_to_var_obs(vidx, swap_p_ps=.true.)
    selectcase( var )
    case( VAR_U, VAR_V, VAR_T, VAR_Q, VAR_PS )
      continue
    case( VAR_Z, VAR_PW, VAR_RAIN )
      cycle
    case default
      call eerr('Invalid value in $var: '//str(var))
    endselect

    !--- DEBUG
    !if( var /= VAR_U ) cycle
    !--- DEBUG/

    selectcase( msgtyp )
    case( MSGTYP_ADPSFC, MSGTYP_SFCSHP )
      selectcase( var )
      case( VAR_U, VAR_V, VAR_T, VAR_Q, VAR_PS )
      case default
        call eerr('Invalid value in $var: '//str(var))
      endselect
    case( MSGTYP_ADPUPA, MSGTYP_AIRCAR, MSGTYP_AIRCFT, &
          MSGTYP_PROFLR, MSGTYP_SATEMP )
      selectcase( var )
      case( VAR_U, VAR_V, VAR_T, VAR_Q )
      case( VAR_PS )
        cycle
      case default
        call eerr('Invalid value in $var: '//str(var))
      endselect
    case( MSGTYP_SATWND, MSGTYP_VADWND, MSGTYP_ASCATW )
      selectcase( var )
      case( VAR_U, VAR_V )
      case( VAR_T, VAR_Q, VAR_PS )
        cycle
      case default
        call eerr('Invalid value in $var: '//str(var))
      endselect
    case( MSGTYP_SFCBOG, MSGTYP_SPSSMI, MSGTYP_SYNDAT, &
          MSGTYP_ERS1DA, MSGTYP_GOESND, MSGTYP_QKSWND, &
          MSGTYP_MSONET, MSGTYP_GPSIPW, MSGTYP_RASSDA, &
          MSGTYP_WDSATR )
      cycle
    case default
      call eerr('Invalid value in $msgtyp: '//str(msgtyp))
    endselect
    !-----------------------------------------------------------
    ! Make data
    !-----------------------------------------------------------
    selectcase( msgtyp )
    !-----------------------------------------------------------
    ! Case: Airplane
    case( MSGTYP_AIRCAR, MSGTYP_AIRCFT )
      fout_point = joined(dirout_obs, str(msgtyp)//'_'//str(var)//'0_p.bin')
      fout_grid  = joined(dirout_obs, str(msgtyp)//'_'//str(var)//'0_g.bin')

      call makedata_uair(pb, vidx, 0.d0, fout_point, fout_grid)
    !-----------------------------------------------------------
    ! Case: Upper air
    case( MSGTYP_ADPUPA, MSGTYP_PROFLR, MSGTYP_VADWND, &
          MSGTYP_SATEMP, MSGTYP_SATWND, MSGTYP_ASCATW )
      do ilev = 1, nlev
        !--- DEBUG
        !if( ilev /= 1 .and. ilev /= 5 ) cycle
        !if( ilev /= 5 ) cycle
        !--- DEBUG/
        fout_point = joined(dirout_obs, str(msgtyp)//'_'//str(var)//str(ilev)//'_p.bin')
        fout_grid  = joined(dirout_obs, str(msgtyp)//'_'//str(var)//str(ilev)//'_g.bin')

        !call edbg('lev: '//str(ilev)//' plv: '//str(plv(ilev))//' logp: '//str(logp(ilev)))
        call makedata_uair(pb, vidx, logp(ilev), fout_point, fout_grid)
      enddo
    !-----------------------------------------------------------
    ! Case: Surface
    case( MSGTYP_ADPSFC, MSGTYP_SFCSHP )
      fout_point = joined(dirout_obs, str(msgtyp)//'_'//str(var)//str(1)//'_p.bin')
      fout_grid  = joined(dirout_obs, str(msgtyp)//'_'//str(var)//str(1)//'_g.bin')
      call makedata_surf(pb, vidx, fout_point, fout_grid)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
    endselect
  enddo  ! vidx/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine run
!===============================================================
!
!===============================================================
subroutine makedata_uair(pb, vidx, logp, fout_point, fout_grid)
  use lib_array
  use lib_io
  use lib_math
  implicit none
  type(prepbufr_), intent(in), target :: pb
  integer        , intent(in) :: vidx
  real(8)        , intent(in) :: logp
  character(*)   , intent(in) :: fout_point, fout_grid

  type(message_), pointer :: msg
  type(subset_), pointer :: sub
  type(record_), pointer :: rec

  integer :: imsg, isub, irec
  integer :: nrec_tmp
  integer :: nrec_out
  real(8) :: diff_logp
  real(8) :: wgt
  real(8), allocatable :: plon_tmp(:), plat_tmp(:), pobs_tmp(:), pper_tmp(:)
  real(8), pointer     :: plon_out(:), plat_out(:), pobs_out(:), pwgt_out(:)
  integer, pointer     :: ilon1d(:), ilat1d(:)
  real(8), pointer     :: glon1d(:), glat1d(:), gobs1d(:), gwgt1d(:)
  integer, allocatable :: arg(:)
  integer :: jrec
  integer :: js1, je1, js2, je2
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nrec_tmp = 0
  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)
      do irec = 1, sub%nrec
        rec => sub%rec(irec)

        if( rec%obs(vidx) == PREPBUFR_MISS ) cycle

        if( rec%qmk(vidx) >= 4.d0 ) cycle

        if( logp > 0.d0 )then
          diff_logp = abs(log(rec%obs(vidx_P)) - logp)
          if( diff_logp > THRESH_DIFF_LOGP ) cycle
        endif

        call add(nrec_tmp)
      enddo  ! irec/
    enddo  ! isub/
  enddo  ! imsg/

  call edbg('nrec_tmp: '//str(nrec_tmp))
  if( nrec_tmp == 0 ) return

  allocate(plon_tmp(nrec_tmp))
  allocate(plat_tmp(nrec_tmp))
  allocate(pobs_tmp(nrec_tmp))
  allocate(pper_tmp(nrec_tmp))

  jrec = 0
  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)
      do irec = 1, sub%nrec
        rec => sub%rec(irec)

        if( rec%obs(vidx) == PREPBUFR_MISS ) cycle

        if( rec%qmk(vidx) >= 4.d0 ) cycle

        if( logp > 0.d0 )then
          diff_logp = abs(log(rec%obs(vidx_P)) - logp)
          if( diff_logp > THRESH_DIFF_LOGP ) cycle
        else
          diff_logp = 0.d0
        endif

        call add(jrec)
        plon_tmp(jrec) = adjust_lon(sub%lon)
        plat_tmp(jrec) = sub%lat
        pobs_tmp(jrec) = rec%obs(vidx)
        pper_tmp(jrec) = diff_logp
      enddo  ! irec/
    enddo  ! isub/
  enddo  ! imsg/
  !-------------------------------------------------------------
  !  
  !-------------------------------------------------------------
  allocate(plon_out(nrec_tmp))
  allocate(plat_out(nrec_tmp))
  allocate(pobs_out(nrec_tmp))
  allocate(pwgt_out(nrec_tmp))
  allocate(arg(nrec_tmp))

  plon_out(:) = 0.d0
  plat_out(:) = 0.d0
  pobs_out(:) = 0.d0
  pwgt_out(:) = 0.d0

  nrec_out = 0
  je1 = 0
  do while( je1 < nrec_tmp )
    js1 = je1 + 1
    je1 = js1
    do while( je1 < nrec_tmp )
      if( plon_tmp(je1+1) /= plon_tmp(js1) ) exit
      je1 = je1 + 1
    enddo
    if( .not. all(plon_tmp(js1:je1) == plon_tmp(js1)) )then
      call eerr('!!! INTERNAL ERROR !!! lon does not match')
    endif

    if( je1 == js1 )then
      call add(nrec_out)
      wgt = calc_weight_vertical(pper_tmp(js1))
      plon_out(nrec_out) = plon_tmp(js1)*wgt
      plat_out(nrec_out) = plat_tmp(js1)*wgt
      pobs_out(nrec_out) = pobs_tmp(js1)*wgt
      pwgt_out(nrec_out) = calc_weight_vertical(pper_tmp(js1))
    else
      call argsort(plat_tmp(js1:je1), arg(js1:je1))
      call sort(plat_tmp(js1:je1), arg(js1:je1))
      call sort(pobs_tmp(js1:je1), arg(js1:je1))

      je2 = js1 - 1
      do while( je2 < je1 )
        js2 = je2 + 1
        je2 = js2
        do while( je2 < je1 )
          if( plat_tmp(je2+1) /= plat_tmp(js2) ) exit
          je2 = je2 + 1
        enddo
        if( .not. (all(plon_tmp(js2:je2) == plon_tmp(js2)) .and. &
                   all(plat_tmp(js2:je2) == plat_tmp(js2))) )then
          call eerr('!!! INTERNAL ERROR !!! lon or lat does not match')
        endif
      enddo  ! while( je2 < je1 )/

      call add(nrec_out)
      pwgt_out(nrec_out) = 0.d0
      do jrec = js2, je2
        wgt = calc_weight_vertical(pper_tmp(jrec))
        call add(plon_out(nrec_out), plon_tmp(jrec)*wgt)
        call add(plat_out(nrec_out), plat_tmp(jrec)*wgt)
        call add(pobs_out(nrec_out), pobs_tmp(jrec)*wgt)
        call add(pwgt_out(nrec_out), wgt)
      enddo
    endif
  enddo  ! while( je1 < nrec_tmp )/

  call edbg('nrec_out: '//str(nrec_out))
  if( nrec_out == 0 ) return

  call realloc(plon_out, nrec_out, clear=.false.)
  call realloc(plat_out, nrec_out, clear=.false.)
  call realloc(pobs_out, nrec_out, clear=.false.)
  call realloc(pwgt_out, nrec_out, clear=.false.)

  plon_out = plon_out / pwgt_out
  plat_out = plat_out / pwgt_out
  pobs_out = pobs_out / pwgt_out

  deallocate(plon_tmp)
  deallocate(plat_tmp)
  deallocate(pobs_tmp)
  deallocate(pper_tmp)
  deallocate(arg)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_grid1d(&
         plon_out, plat_out, pobs_out, pwgt_out, &
         ilon1d, ilat1d, glon1d, glat1d, gobs1d, gwgt1d)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Writing '//str(fout_point))
  call wbin(plon_out, fout_point, dtype=DTYPE_REAL, rec=1, replace=.true.)
  call wbin(plat_out, fout_point, dtype=DTYPE_REAL, rec=2)
  call wbin(pobs_out, fout_point, dtype=DTYPE_REAL, rec=3)
  call wbin(pwgt_out, fout_point, dtype=DTYPE_REAL, rec=4)

  call edbg('Writing '//str(fout_grid))
  call wbin(ilon1d, fout_grid, dtype=DTYPE_REAL, rec=1, replace=.true.)
  call wbin(ilat1d, fout_grid, dtype=DTYPE_REAL, rec=2)
  call wbin(glon1d, fout_grid, dtype=DTYPE_REAL, rec=3)
  call wbin(glat1d, fout_grid, dtype=DTYPE_REAL, rec=4)
  call wbin(gobs1d, fout_grid, dtype=DTYPE_REAL, rec=5)
  call wbin(gwgt1d, fout_grid, dtype=DTYPE_REAL, rec=6)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(plon_out)
  deallocate(plat_out)
  deallocate(pobs_out)
  deallocate(pwgt_out)

  deallocate(ilon1d)
  deallocate(ilat1d)
  deallocate(glon1d)
  deallocate(glat1d)
  deallocate(gobs1d)
  deallocate(gwgt1d)
  !-------------------------------------------------------------
end subroutine makedata_uair
!===============================================================
!
!===============================================================
subroutine makedata_surf(pb, vidx, fout_point, fout_grid)
  use lib_array
  use lib_io
  use lib_math
  implicit none
  type(prepbufr_), intent(in), target :: pb
  integer        , intent(in) :: vidx
  character(*)   , intent(in) :: fout_point, fout_grid

  type(message_), pointer :: msg
  type(subset_), pointer :: sub
  type(record_), pointer :: rec

  integer :: imsg, isub
  integer :: nrec_tmp
  integer :: nrec_out
  real(8), allocatable :: plon_tmp(:), plat_tmp(:), pobs_tmp(:)
  real(8), pointer     :: plon_out(:), plat_out(:), pobs_out(:), pwgt_out(:)
  integer, pointer     :: ilon1d(:), ilat1d(:)
  real(8), pointer     :: glon1d(:), glat1d(:), gobs1d(:), gwgt1d(:)
  integer, allocatable :: arg(:)
  integer :: jrec
  integer :: js1, je1, js2, je2
  !-------------------------------------------------------------
  ! Count # of valid data
  !-------------------------------------------------------------
  nrec_tmp = 0
  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)
      if( sub%nrec > 1 )then
        call eerr('Unexpected condition: sub%nrec > 1')
      endif
      rec => sub%rec(1)

      if( rec%obs(vidx) == PREPBUFR_MISS ) cycle

      if( rec%qmk(vidx) >= 4.d0 ) cycle

      call add(nrec_tmp)
    enddo  ! isub/
  enddo  ! imsg/

  call edbg('nrec_tmp: '//str(nrec_tmp))
  if( nrec_tmp == 0 ) return
  !-------------------------------------------------------------
  ! Store the data
  !-------------------------------------------------------------
  allocate(plon_tmp(nrec_tmp))
  allocate(plat_tmp(nrec_tmp))
  allocate(pobs_tmp(nrec_tmp))

  jrec = 0
  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)
      rec => sub%rec(1)

      if( rec%obs(vidx) == PREPBUFR_MISS ) cycle

      if( rec%qmk(vidx) >= 4.d0 ) cycle

      call add(jrec)

      plon_tmp(jrec) = adjust_lon(sub%lon)
      plat_tmp(jrec) = sub%lat
      pobs_tmp(jrec) = rec%obs(vidx)
    enddo  ! isub/
  enddo  ! imsg/
  !-------------------------------------------------------------
  ! Merge point data which have same coordinates
  !-------------------------------------------------------------
  allocate(plon_out(nrec_tmp))
  allocate(plat_out(nrec_tmp))
  allocate(pobs_out(nrec_tmp))
  allocate(pwgt_out(nrec_tmp))
  allocate(arg(nrec_tmp))

  plon_out(:) = 0.d0
  plat_out(:) = 0.d0
  pobs_out(:) = 0.d0
  pwgt_out(:) = 0.d0

  nrec_out = 0
  je1 = 0
  do while( je1 < nrec_tmp )
    js1 = je1 + 1
    je1 = js1
    do while( je1 < nrec_tmp )
      if( plon_tmp(je1+1) /= plon_tmp(js1) ) exit
      je1 = je1 + 1
    enddo
    if( .not. all(plon_tmp(js1:je1) == plon_tmp(js1)) )then
      call eerr('!!! INTERNAL ERROR !!! lon does not match')
    endif

    if( je1 == js1 )then
      call add(nrec_out)
      plon_out(nrec_out) = plon_tmp(js1)
      plat_out(nrec_out) = plat_tmp(js1)
      pobs_out(nrec_out) = pobs_tmp(js1)
      pwgt_out(nrec_out) = 1
    else
      call argsort(plat_tmp(js1:je1), arg(js1:je1))
      call sort(plat_tmp(js1:je1), arg(js1:je1))
      call sort(pobs_tmp(js1:je1), arg(js1:je1))

      je2 = js1 - 1
      do while( je2 < je1 )
        js2 = je2 + 1
        je2 = js2
        do while( je2 < je1 )
          if( plat_tmp(je2+1) /= plat_tmp(js2) ) exit
          je2 = je2 + 1
        enddo
        if( .not. (all(plon_tmp(js2:je2) == plon_tmp(js2)) .and. &
                   all(plat_tmp(js2:je2) == plat_tmp(js2))) )then
          call eerr('!!! INTERNAL ERROR !!! lon or lat does not match')
        endif

        call add(nrec_out)
        plon_out(nrec_out) = plon_tmp(js2)
        plat_out(nrec_out) = plat_tmp(js2)
        pobs_out(nrec_out) = sum(pobs_tmp(js2:je2)) / (je2-js2+1)
        pwgt_out(nrec_out) = je2 - js2 + 1
      enddo  ! while( je2 < je1 )/
    endif
  enddo  ! while( je1 < nrec )/

  call edbg('nrec_out: '//str(nrec_out))
  if( nrec_out == 0 ) return

  call realloc(plon_out, nrec_out, clear=.false.)
  call realloc(plat_out, nrec_out, clear=.false.)
  call realloc(pobs_out, nrec_out, clear=.false.)
  call realloc(pwgt_out, nrec_out, clear=.false.)

  deallocate(plon_tmp)
  deallocate(plat_tmp)
  deallocate(pobs_tmp)
  deallocate(arg)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call make_grid1d(&
         plon_out, plat_out, pobs_out, pwgt_out, &
         ilon1d, ilat1d, glon1d, glat1d, gobs1d, gwgt1d)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Writing '//str(fout_point))
  call wbin(plon_out, fout_point, dtype=DTYPE_REAL, rec=1, replace=.true.)
  call wbin(plat_out, fout_point, dtype=DTYPE_REAL, rec=2)
  call wbin(pobs_out, fout_point, dtype=DTYPE_REAL, rec=3)
  call wbin(pwgt_out, fout_point, dtype=DTYPE_REAL, rec=4)

  call edbg('Writing '//str(fout_grid))
  call wbin(ilon1d, fout_grid, rec=1, dtype=DTYPE_REAL, replace=.true.)
  call wbin(ilat1d, fout_grid, rec=2, dtype=DTYPE_REAL)
  call wbin(glon1d, fout_grid, rec=3, dtype=DTYPE_REAL)
  call wbin(glat1d, fout_grid, rec=4, dtype=DTYPE_REAL)
  call wbin(gobs1d, fout_grid, rec=5, dtype=DTYPE_REAL)
  call wbin(gwgt1d, fout_grid, rec=6, dtype=DTYPE_REAL)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(plon_out)
  deallocate(plat_out)
  deallocate(pobs_out)
  deallocate(pwgt_out)

  deallocate(ilon1d)
  deallocate(ilat1d)
  deallocate(glon1d)
  deallocate(glat1d)
  deallocate(gobs1d)
  deallocate(gwgt1d)
  !-------------------------------------------------------------
end subroutine makedata_surf
!===============================================================
!
!===============================================================
subroutine make_grid1d(&
    plon, plat, pobs, pwgt, &
    ilon, ilat, glon, glat, gobs, gwgt)
  use lib_math
  implicit none
  real(8), intent(in) :: plon(:), plat(:), pobs(:), pwgt(:)
  integer, pointer    :: ilon(:), ilat(:)
  real(8), pointer    :: glon(:), glat(:), gobs(:), gwgt(:)

  integer, allocatable :: idx1(:,:)
  integer :: nrec, jrec
  integer :: ngrid, jgrid
  integer :: jlon, jlat
  integer :: info
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nrec = size(plon)

  allocate(idx1(klon,klat))
  idx1(:,:) = 0
  ngrid = 0
  do jrec = 1, nrec
    info = get_grid_index(plon(jrec), plat(jrec), jlon, jlat)
    if( idx1(jlon,jlat) == 0 )then
      call add(ngrid)
      idx1(jlon,jlat) = ngrid
    endif
  enddo

  call edbg('ngrid: '//str(ngrid)//' ('//str(real(ngrid)/(klon*klat)*100,'f5.1')//' %)')

  allocate(ilon(ngrid))
  allocate(ilat(ngrid))
  allocate(glon(ngrid))
  allocate(glat(ngrid))
  allocate(gobs(ngrid))
  allocate(gwgt(ngrid))

  glon(:) = 0.d0
  glat(:) = 0.d0
  gobs(:) = 0.d0
  gwgt(:) = 0.d0
  do jrec = 1, nrec
    info = get_grid_index(plon(jrec), plat(jrec), jlon, jlat)
    jgrid = idx1(jlon,jlat)
    ilon(jgrid) = jlon
    ilat(jgrid) = jlat
    call add(glon(jgrid), plon(jrec)*pwgt(jrec))
    call add(glat(jgrid), plat(jrec)*pwgt(jrec))
    call add(gobs(jgrid), pobs(jrec)*pwgt(jrec))
    call add(gwgt(jgrid), pwgt(jrec))
  enddo

  glon = glon / gwgt
  glat = glat / gwgt
  gobs = gobs / gwgt

  deallocate(idx1)
  !-------------------------------------------------------------
end subroutine make_grid1d
!===============================================================
!
!===============================================================
subroutine set_consts()
  use mod_pb, only: &
        get_num_key_all, &
        get_nvar
  use mod_base, only: &
        var_to_idx_obs
  implicit none

  call echo(code%bgn, 'set_consts', opt_log)
  !-------------------------------------------------------------
  call get_num_key_all(nhdr, nobs, noer, nqmk, npcd, nrcd)
  nvar = get_nvar(nobs)

  vidx_P = var_to_idx_obs(VAR_P, nobs)
  vidx_Z = var_to_idx_obs(VAR_Z, nobs)
  vidx_T = var_to_idx_obs(VAR_T, nobs)
  vidx_Q = var_to_idx_obs(VAR_Q, nobs)
  vidx_U = var_to_idx_obs(VAR_U, nobs)
  vidx_V = var_to_idx_obs(VAR_V, nobs)

  vidx_Tv = var_to_idx_obs(VAR_TV, nobs)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_consts
!===============================================================
!
!===============================================================
integer function qmk_to_qlv(qmk) result(qlv)
  implicit none
  real(8), intent(in) :: qmk

  qlv = min(int(qmk), 5)
end function qmk_to_qlv
!===============================================================
!
!===============================================================
real(8) function adjust_lon(lonin) result(lon)
  implicit none
  real(8), intent(in) :: lonin

  lon = lonin
  if( lon > 1.8d2 ) lon = lonin - 3.6d2
end function adjust_lon
!===============================================================
!
!===============================================================
integer function get_grid_index(lon, lat, ilon, ilat) result(info)
  implicit none
  real(8), intent(in) :: lon, lat
  integer, intent(out) :: ilon, ilat

  ilon = max(min(int((lon+1.8d2) / klonsize + 1), klon), 1)
  ilat = max(min(int((lat+9.d1) / klatsize + 1), klat), 1)

  info = 0
  if( ilon < 1 .or. ilat < 1 ) info = 1
end function get_grid_index
!===============================================================
!
!===============================================================
real(8) function calc_weight_vertical(dv) result(wgt)
  implicit none
  real(8), intent(in) :: dv

  wgt = exp(-(dv / rho_v)**2)
end function calc_weight_vertical
!===============================================================
!
!===============================================================
end module mod_main
