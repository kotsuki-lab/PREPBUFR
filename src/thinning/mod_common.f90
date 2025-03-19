module mod_common
  use lib_const
  use lib_time
  use lib_util
  use lib_log
  use lib_io
  use lib_array
  use lib_math
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
  use mod_share
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: find_records_same_id
  public :: free_records_same_id
  public :: pnt_lst_idx_same_id

  public :: find_grids_around
  public :: free_grid
  public :: find_subsets_in_grids
  public :: free_subsets_in_grids
  public :: init_records_status

  public :: find_local_records_of_layers
  public :: free_local_records_of_layers

  public :: make_lst_lay_all
  public :: make_lst_rec_valid

  public :: copy_pb_org
  public :: output_obs

  public :: free_pb

  public :: write_monit_summary
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  type(grid_), pointer :: grid(:,:)
  logical :: is_associated_grid = .false.

  type(lst_idx_same_id_), pointer :: lst_idx_same_sid(:)
  type(lst_idx_same_id_), pointer :: lst_idx_same_said(:)
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine find_records_same_id(pb, code_same_id)
  implicit none
  type(pb_), intent(in), target :: pb
  integer, intent(in) :: code_same_id

  integer :: idtype
  integer(8), allocatable :: lst_sub_all_id(:)
  integer, allocatable :: lst_imsg(:)
  integer, allocatable :: lst_isub(:)
  integer, allocatable :: arg(:)
  integer :: nsub_all
  integer :: imsg, isub, irec
  integer :: imsg2, isub2
  integer :: nid
  integer :: is, ie, i
  type(msg_), pointer :: msg
  type(sub_), pointer :: sub, sub2
  type(rec_), pointer :: rec
  type(lst_idx_same_id_), pointer :: lst
  type(lst_idx_same_id_), pointer :: lst_idx_same_id(:)

  call echo(code%bgn, 'find_records_same_id')
  !-------------------------------------------------------------
  ! Set $idtype
  !-------------------------------------------------------------
  selectcase( code_same_id )
  case( CODE_SAME_ID__NONE )
    call echo(code%ret)
    return
  case( CODE_SAME_ID__SID_REDUCE, &
        CODE_SAME_ID__SID_LIMIT )
    idtype = IDTYPE_SID
    if( CLEN_SID /= 8 )then
      call eerr('$CLEN_SID must be equal to 8')
    endif
  case( CODE_SAME_ID__SAID_REDUCE, &
        CODE_SAME_ID__SAID_LIMIT  )
    idtype = IDTYPE_SAID
  case default
    call eerr('Invalid value in $code_same_id: '//str(code_same_id))
  endselect
  !-------------------------------------------------------------
  ! Make a list of IDs
  !-------------------------------------------------------------
  nsub_all = sum(pb%msg(:)%nsub)

  allocate(lst_sub_all_id(nsub_all))
  allocate(lst_imsg(nsub_all))
  allocate(lst_isub(nsub_all))

  nsub_all = 0
  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)
      call add(nsub_all)
      selectcase( idtype )
      case( IDTYPE_SID )
        lst_sub_all_id(nsub_all) = transfer(sub%sid,0_8)
      case( IDTYPE_SAID )
        lst_sub_all_id(nsub_all) = int(sub%said,8)
      case default
        call eerr('Invalid value in $idtype: '//str(idtype))
      endselect
      lst_imsg(nsub_all) = imsg
      lst_isub(nsub_all) = isub
    enddo
  enddo
  !-------------------------------------------------------------
  ! Sort and count # of IDs
  !-------------------------------------------------------------
  allocate(arg(nsub_all))
  call argsort(lst_sub_all_id, arg)
  call sort(lst_sub_all_id, arg)
  call sort(lst_imsg, arg)
  call sort(lst_isub, arg)
  deallocate(arg)

  nid = 0
  ie = 0
  do while( ie < nsub_all )
    is = ie + 1
    ie = is
    do while( ie < nsub_all )
      if( lst_sub_all_id(ie+1) /= lst_sub_all_id(is) ) exit
      ie = ie + 1
    enddo
    call add(nid)
  enddo

  call edbg('Number of IDs: '//str(nid))
  !-------------------------------------------------------------
  ! Get indices of msg and sub for each ID
  !-------------------------------------------------------------
  selectcase( idtype )
  case( IDTYPE_SID )
    allocate(lst_idx_same_sid(nid))
    lst_idx_same_id => lst_idx_same_sid
  case( IDTYPE_SAID )
    allocate(lst_idx_same_said(nid))
    lst_idx_same_id => lst_idx_same_said
  case default
    call eerr('Invalid value in $idtype: '//str(idtype))
  endselect

  nid = 0
  ie = 0
  do while( ie < nsub_all )
    is = ie + 1
    ie = is
    do while( ie < nsub_all )
      if( lst_sub_all_id(ie+1) /= lst_sub_all_id(is) ) exit
      ie = ie + 1
    enddo
    call add(nid)

    lst => lst_idx_same_id(nid)

    lst%nsub = 0
    do i = is, ie
      call add(lst%nsub, pb%msg(lst_imsg(i))%nsub)
    enddo

    allocate(lst%imsg(lst%nsub))
    allocate(lst%isub(lst%nsub))

    lst%nsub = 0
    do i = is, ie
      call add(lst%nsub)
      lst%imsg(lst%nsub) = lst_imsg(i)
      lst%isub(lst%nsub) = lst_isub(i)

      sub => pb%msg(lst_imsg(i))%sub(lst_isub(i))
      selectcase( idtype )
      case( IDTYPE_SID )
        sub%rec(:)%idx_same_sid = nid
      case( IDTYPE_SAID )
        sub%rec(:)%idx_same_said = nid
      case default
        call eerr('Invalid value in $idtype: '//str(idtype))
      endselect
    enddo
  enddo  ! while( ie < nsub_all)/

  deallocate(lst_sub_all_id)
  deallocate(lst_imsg)
  deallocate(lst_isub)

  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)
      do irec = 1, sub%nrec
        rec => sub%rec(irec)

        selectcase( idtype )
        case( IDTYPE_SID )
          lst => lst_idx_same_id(rec%idx_same_sid)
        case( IDTYPE_SAID )
          lst => lst_idx_same_id(rec%idx_same_said)
        case default
          call eerr('Invalid value in $idtype: '//str(idtype))
        endselect

        do i = 1, lst%nsub
          imsg2 = lst%imsg(i)
          isub2 = lst%isub(i)
          sub2 => pb%msg(imsg2)%sub(isub2)

          selectcase( idtype )
          case( IDTYPE_SID )
            if( sub2%sid /= sub%sid )then
              call eerr('sub%sid: '//str(sub%sid)//' sub2%sid: '//str(sub2%sid))
            endif
          case( IDTYPE_SAID )
            if( sub2%said /= sub%said )then
              call eerr('sub%said: '//str(sub%said)//' sub2%said: '//str(sub2%said))
            endif
          case default
            call eerr('Invalid value in $idtype: '//str(idtype))
          endselect
        enddo
      enddo  ! irec/
    enddo  ! isub/
  enddo  ! imsg/
  !-------------------------------------------------------------
  ! Disconnect the pointer
  !-------------------------------------------------------------
  nullify(lst)
  nullify(lst_idx_same_id)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine find_records_same_id
!===============================================================
!
!===============================================================
subroutine free_records_same_id(code_same_id)
  implicit none
  integer, intent(in) :: code_same_id

  call echo(code%bgn, 'free_records_same_id', '-p -x2')
  !-------------------------------------------------------------
  selectcase( code_same_id )
  case( CODE_SAME_ID__NONE )
    call echo(code%ret)
    return
  case( CODE_SAME_ID__SID_REDUCE, &
        CODE_SAME_ID__SID_LIMIT )
    deallocate(lst_idx_same_sid)
  case( CODE_SAME_ID__SAID_REDUCE, &
        CODE_SAME_ID__SAID_LIMIT  )
    deallocate(lst_idx_same_said)
  case default
    call eerr('Invalid value in $code_same_id: '//str(code_same_id))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_records_same_id
!===============================================================
!
!===============================================================
subroutine pnt_lst_idx_same_id(lst, code_same_id)
  implicit none
  type(lst_idx_same_id_), pointer :: lst(:)
  integer, intent(in) :: code_same_id

  call echo(code%bgn, 'pnt_lst_idx_same_id', '-p -x2')
  !-------------------------------------------------------------
  selectcase( code_same_id )
  case( CODE_SAME_ID__NONE )
    continue
  case( CODE_SAME_ID__SID_REDUCE, &
        CODE_SAME_ID__SID_LIMIT )
    lst => lst_idx_same_sid
  case( CODE_SAME_ID__SAID_REDUCE, &
        CODE_SAME_ID__SAID_LIMIT )
    lst => lst_idx_same_said
  case default
    call eerr('Invalid value in $code_same_id: '//str(code_same_id))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine pnt_lst_idx_same_id
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
subroutine find_grids_around(dist_h_thresh, earth_r)
  use mod_sphere, only: &
        common_find_grids_around  => find_grids_around, &
        common_get_ngrid_around   => get_ngrid_around, &
        common_get_indices_around => get_indices_around, &
        common_free_grid          => free_grid
  implicit none
  real(8), intent(in) :: dist_h_thresh
  real(8), intent(in) :: earth_r

  type(grid_), pointer :: g
  integer :: ilon, ilat, ilev
  integer :: idx
  integer :: ilay_all
  real(8), save :: dist_h_thresh_prev = 0.d0
  logical, save :: is_first = .true.

  if( dist_h_thresh == dist_h_thresh_prev )then
    return
  endif

  if( is_first )then
    is_first = .false.
  else
    deallocate(grid)
  endif

  dist_h_thresh_prev = dist_h_thresh

  call common_find_grids_around(&
         nlon, nlat, lonb, latb, dist_h_thresh/earth_r)

  if( is_associated_grid )then
    deallocate(grid)
  else
    is_associated_grid = .true.
  endif

  allocate(grid(nlon,nlat))

  idx = 0
  ilay_all = 0
  do ilat = 1, nlat
  do ilon = 1, nlon
    idx = idx + 1
    g => grid(ilon,ilat)
    !g%idx = idx
    allocate(g%lay(nlev))
    do ilev = 1, nlev
      ilay_all = ilay_all + 1
      g%lay(ilev)%ilay_all = ilay_all
    enddo

    call common_get_ngrid_around(ilon, ilat, g%ngrid_around)
    allocate(g%ilon_around(g%ngrid_around))
    allocate(g%ilat_around(g%ngrid_around))
    call common_get_indices_around(ilon, ilat, g%ilon_around, g%ilat_around)
  enddo
  enddo

  call common_free_grid()
end subroutine find_grids_around
!===============================================================
!
!===============================================================
subroutine free_grid()
  implicit none

  call echo(code%bgn, 'free_grid', '-p -x2')
  !-------------------------------------------------------------
  deallocate(grid)
  is_associated_grid = .false.
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_grid
!===============================================================
!
!===============================================================
subroutine find_subsets_in_grids(pb)
  use mod_sphere, only: &
        find_ilon, &
        find_ilat
  implicit none
  type(pb_), intent(in), target :: pb

  type(msg_), pointer :: msg
  type(sub_) , pointer :: sub
  type(grid_), pointer :: g
  integer :: irun
  integer :: imsg, isub
  integer :: ilon, ilat

  call echo(code%bgn, 'find_subsets_in_grids')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do irun = 1, 2
    grid(:,:)%nsub = 0

    do imsg = 1, pb%nmsg
      msg => pb%msg(imsg)

      do isub = 1, msg%nsub
        sub => msg%sub(isub)

        if( find_ilon(sub%lon, nlon, lonb, ilon) /= 0 )then
          call eerr('lon-index was not detected'//&
                  '\nimsg: '//str(imsg)//' isub:'//str(isub))
        endif
        if( find_ilat(sub%lat, nlat, latb, ilat) /= 0 )then
          call eerr('lat-index was not detected'//&
                  '\nimsg: '//str(imsg)//' isub:'//str(isub))
        endif

        g => grid(ilon,ilat)
        g%nsub = g%nsub + 1
        if( irun == 2 )then
          g%imsg(g%nsub) = imsg
          g%isub(g%nsub) = isub
        endif
      enddo  ! isub/
    enddo  ! imsg/

    if( irun == 1 )then
      do ilat = 1, nlat
      do ilon = 1, nlon
        g => grid(ilon,ilat)
        if( g%nsub == 0 ) cycle
        allocate(g%imsg(g%nsub))
        allocate(g%isub(g%nsub))
      enddo
      enddo
    endif
  enddo  ! irun/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine find_subsets_in_grids
!===============================================================
!
!===============================================================
subroutine free_subsets_in_grids()
  implicit none

  type(grid_), pointer :: g
  integer :: ilon, ilat

  call echo(code%bgn, 'free_subsets_in_grids', '-p -x2')
  !-------------------------------------------------------------
  do ilat = 1, nlat
  do ilon = 1, nlon
    g => grid(ilon,ilat)
    if( g%nsub > 0 )then
      g%nsub = 0
      deallocate(g%imsg)
      deallocate(g%isub)
    endif
  enddo
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_subsets_in_grids
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
subroutine init_records_status(&
    pb, itime, vidx, wgt_qlt, info)
  implicit none
  type(pb_), intent(inout), target :: pb
  integer, intent(in) :: itime
  integer, intent(in) :: vidx
  real(8), intent(in) :: wgt_qlt(-1:)
  integer, intent(out) :: info

  type(msg_), pointer :: msg
  type(sub_) , pointer :: sub
  type(rec_) , pointer :: rec
  integer :: imsg, isub, irec

  call echo(code%bgn, 'init_records_status')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0

  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)

      if( sub%time /= itime )then
        sub%rec(:)%is_valid    = .false.
        sub%rec(:)%is_selected = .false.
        cycle
      endif

      do irec = 1, sub%nrec
        rec => sub%rec(irec)
        rec%irec_valid = 0

        rec%is_selected = .false.

        if( rec%obs(vidx) == PREPBUFR_MISS )then
          rec%is_valid = .false.
        elseif( rec%logp == PREPBUFR_MISS )then
          rec%is_valid = .false.
        elseif( wgt_qlt(rec%iqmk(vidx)) <= 0.d0 )then
          rec%is_valid = .false.
        else
          rec%is_valid = .true.
        endif
      enddo
    enddo
  enddo
  !-------------------------------------------------------------
  ! Remove P (Ps) in the wind reports
  !-------------------------------------------------------------
  if( vidx == vidx_P )then
    do imsg = 1, pb%nmsg
      msg => pb%msg(imsg)
      do isub = 1, msg%nsub
        sub => msg%sub(isub)
        if( sub%report_type == REPORT_TYPE_WIND )then
          do irec = 1, sub%nrec
            sub%rec(irec)%is_valid = .false.
          enddo  ! irec/
        endif
      enddo  ! isub/
    enddo  ! imsg/
  endif
  !-------------------------------------------------------------
  ! Count # of valid records
  !-------------------------------------------------------------
  pb%nrec_valid = 0
  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)
      do irec = 1, sub%nrec
        rec => sub%rec(irec)
        if( rec%is_valid )then
          call add(pb%nrec_valid)
          rec%irec_valid = pb%nrec_valid
        endif
      enddo
    enddo
  enddo
  !-------------------------------------------------------------
  if( pb%nrec_valid == 0 )then
    info = -1
  endif

  call edbg('Total number of valid records: '//str(pb%nrec_valid))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine init_records_status
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
subroutine find_local_records_of_layers(&
    pb, vidx, &
    earth_r, &
    dist_h_thresh, dist_v_thresh, rho_h, rho_v, &
    wgt_qlt, info)
  use mod_sphere, only: &
        calc_dist_h, &
        calc_dist_v, &
        calc_wgt_dist
  implicit none
  type(pb_), intent(in), target :: pb
  integer  , intent(in)         :: vidx
  real(8)  , intent(in)         :: earth_r
  real(8)  , intent(in)         :: dist_h_thresh, dist_v_thresh
  real(8)  , intent(in)         :: rho_h, rho_v
  real(8)  , intent(in)         :: wgt_qlt(-1:)
  integer  , intent(out)        :: info

  type(grid_)     , pointer :: g, g2
  type(grid_lay_) , pointer :: glay
  type(rec_local_), pointer :: rec_local
  type(lay_local_), pointer :: lay_local
  type(msg_)      , pointer :: msg
  type(sub_)      , pointer :: sub
  type(rec_)      , pointer :: rec
  integer :: ilon, ilat, ilev
  integer :: igrid, isub_grid
  integer :: imsg, isub, irec
  real(8) :: dist_h, dist_v
  integer :: nrec_local_max
  ! For debugging
  !integer, allocatable :: int4_3d(:,:,:)

  call echo(code%bgn, 'find_local_records_of_layers')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0
  !-------------------------------------------------------------
  ! Count local records
  !-------------------------------------------------------------
  do imsg = 1, pb%nmsg
    do isub = 1, pb%msg(imsg)%nsub
      pb%msg(imsg)%sub(isub)%rec(:)%nlay_local = 0
    enddo
  enddo

  do ilat = 1, nlat
  do ilon = 1, nlon
    grid(ilon,ilat)%lay(:)%nrec_local = 0
  enddo
  enddo

  nrec_local_max = 0
  do ilat = 1, nlat
  do ilon = 1, nlon
    g => grid(ilon,ilat)
    g%lay(:)%nrec_local = 0
    do igrid = 1, g%ngrid_around
      g2 => grid(g%ilon_around(igrid),g%ilat_around(igrid))
      do isub_grid = 1, g2%nsub
        sub => pb%msg(g2%imsg(isub_grid))%sub(g2%isub(isub_grid))
        dist_h = calc_dist_h(lon(ilon), lat(ilat), sub%lon, sub%lat) * earth_r
        if( dist_h > dist_h_thresh ) cycle

        do ilev = 1, nlev
          glay => g%lay(ilev)
          do irec = 1, sub%nrec
            rec => sub%rec(irec)
            if( .not. rec%is_valid ) cycle

            dist_v = calc_dist_v(rec%logp, logplv(ilev))
            if( dist_v > dist_v_thresh ) cycle

            call add(glay%nrec_local)
            call add(rec%nlay_local)
          enddo  ! irec/
        enddo  ! ilev/
      enddo  ! isub_grid/
    enddo  ! igrid/

    nrec_local_max = max(nrec_local_max, maxval(g%lay(:)%nrec_local))
  enddo  ! ilon/
  enddo  ! ilat/

  call edbg('Max. number of local records: '//str(nrec_local_max))

  if( nrec_local_max == 0 )then
    info = -1
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Init. and. allocate
  !-------------------------------------------------------------
  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)
      do irec = 1, sub%nrec
        rec => sub%rec(irec)
        rec%ilay_all_nearest = 0
        rec%ilay_all_belong = 0
        rec%wgt_dist_max = 0.d0

        if( .not. rec%is_valid ) cycle
        if( rec%nlay_local > 0 )then
          allocate(rec%lay_local(rec%nlay_local))
          rec%nlay_local = 0
        else
          nullify(rec%lay_local)
        endif
      enddo
    enddo
  enddo

  do ilat = 1, nlat
  do ilon = 1, nlon
    g => grid(ilon,ilat)
    do ilev = 1, nlev
      glay => g%lay(ilev)
      if( glay%nrec_local > 0 )then
        allocate(glay%rec_local(glay%nrec_local))
        glay%nrec_local = 0
      else
        nullify(glay%rec_local)
      endif
    enddo
  enddo
  enddo
  !-------------------------------------------------------------
  ! Store info.
  !-------------------------------------------------------------
  do ilat = 1, nlat
  do ilon = 1, nlon
    g => grid(ilon,ilat)
    do igrid = 1, g%ngrid_around
      g2 => grid(g%ilon_around(igrid),g%ilat_around(igrid))
      do isub_grid = 1, g2%nsub
        sub => pb%msg(g2%imsg(isub_grid))%sub(g2%isub(isub_grid))
        dist_h = calc_dist_h(lon(ilon), lat(ilat), sub%lon, sub%lat) * earth_r
        if( dist_h > dist_h_thresh ) cycle

        do ilev = 1, nlev
          glay => g%lay(ilev)
          do irec = 1, sub%nrec
            rec => sub%rec(irec)
            if( .not. rec%is_valid ) cycle

            dist_v = calc_dist_v(rec%logp, logplv(ilev))
            if( dist_v > dist_v_thresh ) cycle

            glay%nrec_local = glay%nrec_local + 1
            rec_local => glay%rec_local(glay%nrec_local)
            rec_local%imsg = g2%imsg(isub_grid)
            rec_local%isub = g2%isub(isub_grid)
            rec_local%irec = irec
            rec_local%ilay_all = glay%ilay_all
            rec_local%dist_h = dist_h
            rec_local%dist_v = dist_v
            rec_local%wgt_dist = calc_wgt_dist(dist_h, dist_v, rho_h, rho_v)

            rec => pb%msg(rec_local%imsg)%sub(rec_local%isub)%rec(rec_local%irec)
            rec_local%irec_valid = rec%irec_valid
            rec_local%wgt = rec_local%wgt_dist * wgt_qlt(rec%iqmk(vidx))

            if( rec_local%wgt_dist > rec%wgt_dist_max )then
              rec%wgt_dist_max = rec_local%wgt_dist
              rec%ilay_all_nearest = glay%ilay_all
            endif

            call add(rec%nlay_local)
            lay_local => rec%lay_local(rec%nlay_local)
            lay_local%ilay_all = glay%ilay_all
            lay_local%irec_local = glay%nrec_local
          enddo  ! irec/
        enddo  ! ilev/
      enddo  ! isub_grid/
    enddo  ! igrid/
  enddo  ! ilon/
  enddo  ! ilat/
  !-------------------------------------------------------------
  ! Check consistencies
  !-------------------------------------------------------------
  do ilat = 1, nlat
  do ilon = 1, nlon
  do ilev = 1, nlev
    glay => g%lay(ilev)
    if( glay%nrec_local == 0 )then
      if( associated(glay%rec_local) )then
        call eerr('!!! INTERNAL ERROR !!!')
      endif
    else
      if( .not. associated(glay%rec_local) )then
        call eerr('!!! INTERNAL ERROR !!!')
      elseif( size(glay%rec_local) /= glay%nrec_local )then
        call eerr('!!! INTERNAL ERROR !!!')
      endif
    endif
  enddo
  enddo
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine find_local_records_of_layers
!===============================================================
!
!===============================================================
subroutine free_local_records_of_layers()
  implicit none

  type(grid_lay_), pointer :: glay
  integer :: ilon, ilat, ilev

  call echo(code%bgn, 'free_local_records_of_layers', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ilat = 1, nlat
  do ilon = 1, nlon
    do ilev = 1, nlev
      glay => grid(ilon,ilat)%lay(ilev)
      if( glay%nrec_local > 0 )then
        deallocate(glay%rec_local)
        nullify(glay%rec_local)
        glay%nrec_local = 0
      endif
    enddo
  enddo
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_local_records_of_layers
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
subroutine make_lst_lay_all(lst_lay_all)
  implicit none
  type(lay_all_), intent(out), target :: lst_lay_all(:) !(nlay_all)

  type(grid_lay_), pointer :: glay
  type(lay_all_) , pointer :: lay_all
  integer :: ilon, ilat, ilev
  integer :: ilay_all

  call echo(code%bgn, 'make_lst_lay_all')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( size(lst_lay_all) /= nlay_all )then
    call eerr('size(lst_lay_all) /= nlay_all')
  endif

  lst_lay_all(:)%dnst_all = 0.d0
  lst_lay_all(:)%dnst_used = 0.d0

  ilay_all = 0
  do ilat = 1, nlat
  do ilon = 1, nlon
  do ilev = 1, nlev
    ilay_all = ilay_all + 1

    glay => grid(ilon,ilat)%lay(ilev)

    lay_all => lst_lay_all(ilay_all)
    lay_all%ilon = ilon
    lay_all%ilat = ilat
    lay_all%ilev = ilev
    lay_all%nrec_local = glay%nrec_local

    if( glay%nrec_local == 0 )then
      nullify(lay_all%rec_local)
      cycle
    endif

    lay_all%rec_local => glay%rec_local

    lay_all%dnst_all = 0.d0
    lay_all%dnst_all = sum(lay_all%rec_local(:)%wgt)
  enddo  ! ilev/
  enddo  ! ilon/
  enddo  ! ilat/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_lst_lay_all
!===============================================================
!
!===============================================================
!subroutine free_lst_lay_all()
!  implicit none
!
!  type(lay_all_), pointer :: lay_all
!  integer :: ilay_all
!
!  call echo(code%bgn, 'free_lst_lay_all', '-p -x2')
!  !-------------------------------------------------------------
!  do ilay_all = 1, nlay_all
!    lay_all => lst_lay_all(ilay_all)
!    if( lay_all%nrec_local > 0 )then
!      nullify(lay_all%rec_local)
!    endif
!  enddo
!
!  nullify(lay_all)
!  deallocate(lst_lay_all)
!  !-------------------------------------------------------------
!  call echo(code%ret)
!end subroutine free_lst_lay_all
!===============================================================
!
!===============================================================
subroutine make_lst_rec_valid(pb, lst_lay_all, lst_rec_valid)
  implicit none
  type(pb_), intent(in), target :: pb
  type(lay_all_)  , intent(in) , target :: lst_lay_all(:)
  type(rec_valid_), intent(out), target :: lst_rec_valid(:) !(pb%nrec_valid)

  type(rec_valid_), pointer :: rec_valid
  type(lay_all_)  , pointer :: lay_all
  type(rec_local_), pointer :: rec_local
  type(msg_)      , pointer :: msg
  type(sub_)      , pointer :: sub
  integer :: irec_valid
  integer :: ilay_all
  integer :: irec_local
  integer :: imsg, isub, irec

  call echo(code%bgn, 'make_lst_rec_valid')
  !-------------------------------------------------------------
  ! Store info.
  !-------------------------------------------------------------
!  allocate(lst_rec_valid(pb%nrec_valid))
  if( size(lst_rec_valid) /= pb%nrec_valid )then
    call eerr('size(lst_rec_valid) /= pb%nrec_valid')
  endif

  irec_valid = 0
  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)
      do irec = 1, sub%nrec
        if( .not. sub%rec(irec)%is_valid ) cycle
        call add(irec_valid)
        rec_valid => lst_rec_valid(irec_valid)
        rec_valid%imsg = imsg
        rec_valid%isub = isub
        rec_valid%irec = irec
      enddo  ! irec/
    enddo  ! isub/
  enddo  ! imsg/
  !-------------------------------------------------------------
  ! Count local layers
  ! Ignored if weight of the record is below threshold
  !-------------------------------------------------------------
  lst_rec_valid(:)%nlay_local = 0
  do ilay_all = 1, nlay_all
    lay_all => lst_lay_all(ilay_all)
    do irec_local = 1, lay_all%nrec_local
      rec_local => lay_all%rec_local(irec_local)
      if( rec_local%wgt < WGT_THRESH ) cycle

      call add(lst_rec_valid(rec_local%irec_valid)%nlay_local)
    enddo
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do irec_valid = 1, pb%nrec_valid
    rec_valid => lst_rec_valid(irec_valid)
    if( rec_valid%nlay_local > 0 )then
      allocate(rec_valid%ilay_all(rec_valid%nlay_local))
      allocate(rec_valid%irec_local(rec_valid%nlay_local))
    endif
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  lst_rec_valid(:)%nlay_local = 0
  do ilay_all = 1, nlay_all
    lay_all => lst_lay_all(ilay_all)
    do irec_local = 1, lay_all%nrec_local
      rec_local => lay_all%rec_local(irec_local)
      if( rec_local%wgt < WGT_THRESH ) cycle

      rec_valid => lst_rec_valid(rec_local%irec_valid)
      call add(rec_valid%nlay_local)
      rec_valid%ilay_all(rec_valid%nlay_local) = ilay_all
      rec_valid%irec_local(rec_valid%nlay_local) = irec_local
    enddo
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
!  nrec_updated = pb%nrec_valid

!  allocate(lst_rec_valid_is_updated(nrec_valid))

!  allocate(lst_irec_updated(nrec_valid))
!  do irec_valid = 1, nrec_valid
!    lst_irec_updated(irec_valid) = irec_valid
!  enddo

!  allocate(lst_rec_valid_cntrb(nrec_valid))

!  allocate(lst_cntrb(maxval(lst_rec_valid(:)%nlay_local)))
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine make_lst_rec_valid
!===============================================================
!
!===============================================================
!subroutine free_lst_rec_valid()
!  implicit none

!  deallocate(lst_rec_valid)
!
!  deallocate(lst_rec_valid_is_updated)
!
!  deallocate(lst_irec_updated)
!
!  deallocate(lst_rec_valid_cntrb)
!
!  deallocate(lst_cntrb)
!end subroutine free_lst_rec_valid
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
subroutine copy_pb_org(pb0, pb, order, geo, monit)
  use lib_util, only: &
        degC_to_K, &
        K_to_degC, &
        conv_RHerr_to_Qerr, &
        conv_vtemp_to_temp
  implicit none
  type(prepbufr_), intent(in) , target :: pb0
  type(pb_)      , intent(out), target :: pb
  type(order_)   , intent(in)          :: order
  type(model_)   , intent(in) , target :: geo
  type(monit_)   , intent(in)          :: monit

  type(message_), pointer :: msg0
  type(subset_) , pointer :: sub0
  type(record_) , pointer :: rec0
  type(msg_), pointer :: msg
  type(sub_), pointer :: sub
  type(rec_), pointer :: rec
  integer :: imsg, isub, irec
  integer :: ivar
  integer :: time1, time2

  type(sub_), pointer :: sub_prev
  integer, pointer :: lst_typ_mass(:)
  integer, pointer :: lst_typ_wind(:)
  logical :: is_same

  call echo(code%bgn, 'copy_pb_org')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Copying data')

  pb%nrec_all = 0
!  tob = 0
!  tof = 0

  pb%msgtyp = pb0%msgtyp
  pb%nobs = pb0%nobs
  pb%noer = pb0%noer
  pb%nqmk = pb0%nqmk
  pb%nmsg = pb0%nmsg
  allocate(pb%msg(pb%nmsg))

  pb%nobs_ext = pb%nobs + 1  ! for Tv

  pb%is_surface = order%is_surface

  do imsg = 1, pb%nmsg
    msg0 => pb0%msg(imsg)
    msg => pb%msg(imsg)

    nullify(msg%sub)

    msg%typ = msg0%typ
    msg%nsub = msg0%nsub
    allocate(msg%sub(msg%nsub))
    !-----------------------------------------------------------
    ! Copy and process subset data
    !-----------------------------------------------------------
    do isub = 1, msg%nsub
      sub0 => msg0%sub(isub)
      sub => msg%sub(isub)
      !---------------------------------------------------------
      ! Copy subset data
      !---------------------------------------------------------
      sub%sid   = sub0%sid
      sub%lon   = sub0%lon
      sub%lat   = sub0%lat
      sub%tdiff = sub0%tdiff
      sub%elv   = sub0%elv
      sub%said  = sub0%said
      sub%t29   = sub0%t29

      allocate(sub%hdr(size(sub0%hdr)))
      sub%hdr   = sub0%hdr

      sub%report_type = REPORT_TYPE_UNDEF

      sub%nrec = sub0%nrec
      allocate(sub%rec(sub%nrec))
      !---------------------------------------------------------
      ! Initialize
      !---------------------------------------------------------
      sub%ilon = 0
      sub%ilat = 0
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      time1 = ceiling(sub%tdiff - order%ticld_bhd)
      time2 = floor  (sub%tdiff + order%ticld_ahd)
      !if( sub%tdiff /= 0.d0 )then
      !  call edbg('tdiff '//str(sub%tdiff,'f4.1')//' time '//str(time1,2)//' - '//str(time2,2))
      !endif
      if( time1 /= time2 )then
        call eerr('Unexpected condition: time1 /= time2'//&
                '\n  tdiff: '//str(sub%tdiff)//&
                '\n  ticld_bhd : '//str(order%ticld_bhd)//&
                  ', tdiff-ticld_bhd: '//str(sub%tdiff-order%ticld_bhd)//&
                '\n  ticld_ahd: '//str(order%ticld_ahd)//&
                  ', tdiff+ticld_ahd: '//str(sub%tdiff-order%ticld_ahd))
      endif
      sub%time = time1

!      tob = min(tob, sub%time)
!      tof = max(tof, sub%time)
      !---------------------------------------------------------
      ! Copy and process record data
      !---------------------------------------------------------
      do irec = 1, sub%nrec
        call add(pb%nrec_all)

        rec0 => sub0%rec(irec)
        rec => sub%rec(irec)

        allocate(rec%obs(pb%nobs_ext))
        allocate(rec%oer(pb%nobs_ext))
        allocate(rec%qmk(pb%nobs_ext))

        rec%obs(:pb0%nobs) = rec0%obs(:)
        rec%oer(:pb0%nobs) = rec0%oer(:)
        rec%qmk(:pb0%nobs) = rec0%qmk(:)

        if( rec0%obs(vidx_P) <= 0.d0 )then
          rec%logp = PREPBUFR_MISS
        else
          rec%logp = log(rec0%obs(vidx_P)*1d2)  ! hPa -> Pa -> log(Pa)
        endif

        allocate(rec%iqmk(pb%nobs_ext))
        do ivar = 1, pb0%nobs
          if( rec0%qmk(ivar) == PREPBUFR_MISS )then
            rec%iqmk(ivar) = -1
          else
            rec%iqmk(ivar) = min(int(rec0%qmk(ivar),4),4)
          endif
        enddo
      enddo  ! irec/
    enddo  ! isub/
  enddo  ! imsg/

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking TYP')

  nullify(lst_typ_mass)
  nullify(lst_typ_wind)
  selectcase( pb%msgtyp )
  case( MSGTYP_ADPSFC )
    call copy(TYP_MASS_ADPSFC, lst_typ_mass)
    call copy(TYP_WIND_ADPSFC, lst_typ_wind)
  case( MSGTYP_SFCSHP )
    call copy(TYP_MASS_SFCSHP, lst_typ_mass)
    call copy(TYP_WIND_SFCSHP, lst_typ_wind)
  case( MSGTYP_SFCBOG )
    call copy(TYP_MASS_SFCBOG, lst_typ_mass)
    call copy(TYP_WIND_SFCBOG, lst_typ_wind)
  case default
    call copy(TYP_MASS_ALL, lst_typ_mass)
    call copy(TYP_WIND_ALL, lst_typ_wind)
  endselect

  imsg = 0
  isub = 0
  do
    !-----------------------------------------------------------
    if( imsg == 0 .and. isub == 0 )then
      nullify(sub_prev)
      imsg = 1
      isub = 1
    else
      sub_prev => pb%msg(imsg)%sub(isub)
      if( isub == pb%msg(imsg)%nsub )then
        imsg = imsg + 1
        isub = 1
      else
        isub = isub + 1
      endif
    endif
    if( imsg > pb%nmsg ) exit
    sub => pb%msg(imsg)%sub(isub)
    !-----------------------------------------------------------
    ! Get report type
    !-----------------------------------------------------------
    if( any(nint(sub%hdr(IHDR_TYP)) == lst_typ_mass) )then
      sub%report_type = REPORT_TYPE_MASS
    elseif( any(nint(sub%hdr(IHDR_TYP)) == lst_typ_wind) )then
      sub%report_type = REPORT_TYPE_WIND
    else
      call eerr('TYP does not match any report type.'//&
              '\n  imsg: '//str(imsg)//', isub: '//str(isub)//&
              '\n  TYP: '//str(sub%hdr(IHDR_TYP)))
    endif
    !-----------------------------------------------------------
    ! Go to next loop if new SID or different time
    !-----------------------------------------------------------
    is_same = .false.
    if( associated(sub_prev) )then
      if( sub%sid == sub_prev%sid .and. sub%hdr(IHDR_DHR) == sub_prev%hdr(IHDR_DHR) )then
        is_same = .true.
      endif
    endif

    if( .not. is_same ) cycle
    !-----------------------------------------------------------
    ! Check data
    !-----------------------------------------------------------
    if( .false. )then
    !-----------------------------------------------------------
    ! Case: Same report type
    if( sub%report_type == sub_prev%report_type )then
      call ewrn('Same report type.'//&
              '\n  imsg: '//str(imsg)//', isub: '//str(isub)//&
              '\n  this TYP: '//str(sub%hdr(IHDR_TYP))//&
                ' report_type: '//str(sub%report_type)//&
              '\n  prev TYP: '//str(sub_prev%hdr(IHDR_TYP))//&
                ' report_type: '//str(sub_prev%report_type))
      if( any(sub%hdr(:) /= sub_prev%hdr(:)) )then
        call edbg('Headers do not match.'//&
                '\n  this: '//str(sub%hdr)//&
                '\n  prev: '//str(sub_prev%hdr))
      else
        !-------------------------------------------------------
        ! Case: Surface obs.
        if( pb%is_surface )then
          !-----------------------------------------------------
          ! Case: All obs. match
          if( all(sub%rec(1)%obs(:) == sub_prev%rec(1)%obs(:)) )then
            call edbg('Headers and all observation data match.')
          !-----------------------------------------------------
          ! Case: Ps does not match
          elseif( sub%rec(1)%obs(vidx_P) /= sub_prev%rec(1)%obs(vidx_P) )then
            call edbg('Ps does not match.'//&
                    '\n  this: '//str(sub%rec(1)%obs(vidx_P))//&
                    '\n  prev: '//str(sub_prev%rec(1)%obs(vidx_P)))
          endif
        !-------------------------------------------------------
        ! Case: Upper air obs.
        else
          !-----------------------------------------------------
          ! Case: Different record num.
          if( sub%nrec /= sub_prev%nrec )then
            call edbg('Headers match but num. of recs. are different.'//&
                    '\n  this: '//str(sub%nrec)//&
                    '\n  prev: '//str(sub_prev%nrec))
          !-----------------------------------------------------
          ! Case: Different obs.
          else
            is_same = .true.
            do irec = 1, sub%nrec
              if( any(sub%rec(irec)%obs(:) /= sub_prev%rec(irec)%obs(:)) )then
                is_same = .false.
                exit
              endif
            enddo
            if( is_same )then
              call edbg('Headers and all observation data match.')
            else
              call edbg('Headers match but observations do not match @ rec '//str(irec)//'.'//&
                      '\n  this: '//str(sub%rec(irec)%obs)//&
                      '\n  prev: '//str(sub_prev%rec(irec)%obs))
            endif
          endif
        endif
      endif
    !-----------------------------------------------------------
    ! Case: Different report type
    else
      if( pb%is_surface )then
        !-------------------------------------------------------
        ! Case: Ps does not match
        if( sub_prev%rec(1)%obs(vidx_P) /= sub%rec(1)%obs(vidx_P) )then
          call ewrn('Ps does not match in different report types.'//&
                  '\n  imsg: '//str(imsg)//', isub: '//str(isub)//&
                  '\n  this Ps: '//str(sub%rec(1)%obs(vidx_P))//&
                    ' report_type: '//str(sub%report_type)//&
                  '\n  prev Ps: '//str(sub_prev%rec(1)%obs(vidx_P))//&
                    ' report_type: '//str(sub_prev%report_type))
        endif
      endif
    endif

    endif
    !-----------------------------------------------------------
  enddo  ! sub, msg/

  nullify(sub_prev)

  call realloc(lst_typ_mass, 0)
  call realloc(lst_typ_wind, 0)

  call echo(code%ext)
  !-------------------------------------------------------------
  ! Conv. Qerr
  !-------------------------------------------------------------
  call echo(code%ent, 'Processing Qerr')
  call conv_Qerr(pb, order%opt_Qerr, monit)
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Conv. Tv
  !-------------------------------------------------------------
  call echo(code%ent, 'Processing Tv')
  call conv_Tv(pb, pb0, order%opt_Tv, monit)
  call echo(code%ext)
  !-------------------------------------------------------------
  ! Modify Ps
  !-------------------------------------------------------------
  if( pb%is_surface )then
    call echo(code%ent, 'Processing Ps')
    call modify_surface(pb, order%opt_sfc, order%lapse_rate, geo, monit)
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( pb%nrec_all == 0 )then
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
!  tob = max(tout_bhd, tob)
!  tof = min(tout_ahd, tof)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine copy_pb_org
!===============================================================
!
!===============================================================
subroutine conv_Qerr(pb, opt_Qerr, monit)
  use mod_pb, only: &
        dgt_nmsg, &
        dgt_nsub, &
        dgt_nrec
  implicit none
  type(pb_)   , intent(in), target :: pb
  integer     , intent(in) :: opt_Qerr
  type(monit_), intent(in) :: monit

  type(msg_), pointer :: msg
  type(sub_), pointer :: sub
  type(rec_), pointer :: rec
  integer :: imsg, isub, irec
  real(8) :: Qerr
  integer :: info
  integer :: n_all, n_ok, &
             n_Qobs_miss, n_Qoer_miss, n_Q_qmk, &
             n_Pobs_miss, n_P_qmk, &
             n_Tobs_miss, n_T_qmk

  character(CLEN_PATH) :: path_monit
  integer :: un
  character(64) :: wfmt

  call echo(code%bgn, 'conv_Qerr', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( opt_Qerr )
  !-------------------------------------------------------------
  ! Case: Conv. RHerr to Qerr
  case( OPT_QERR__CONV_RHERR )
    if( monit%save_modif )then
      path_monit = joined(monit%dir,'Qerr.'//trim(pb%msgtyp)//'.txt')
      call edbg('Monitoring output: '//str(path_monit))
      un = unit_number()
      open(un, file=path_monit, status='replace')
      write(un,"(a)") &
            'info imsg isub irec lon lat P[hPa] Q[mg/kg] RHerr[0.1%] Qerr[mg/kg]'
      wfmt = "(1x,i2,1x,i"//str(dgt_nmsg())//",1x,i"//str(dgt_nsub())//&
             ",1x,i"//str(dgt_nrec())//",2(1x,f8.3),"//&
              "4(1x,es11.4))"
    endif

    n_all       = 0
    n_ok        = 0
    n_Qobs_miss = 0
    n_Qoer_miss = 0
    n_Q_qmk     = 0
    n_Pobs_miss = 0
    n_P_qmk     = 0
    n_Tobs_miss = 0
    n_T_qmk     = 0

    do imsg = 1, pb%nmsg
      msg => pb%msg(imsg)
      do isub = 1, msg%nsub
        sub => msg%sub(isub)

        if( sub%report_type /= REPORT_TYPE_MASS ) cycle

        do irec = 1, sub%nrec
          rec => sub%rec(irec)

          call add(n_all)
          !-----------------------------------------------------
          ! Judge if it is converted
          !-----------------------------------------------------
          info = 0
          if( rec%obs(vidx_Q) == PREPBUFR_MISS )then
            info = 1
            call add(n_Qobs_miss)
          elseif( rec%oer(vidx_Q) == PREPBUFR_MISS )then
            info = 2
            call add(n_Qoer_miss)
          !elseif( rec%iqmk(vidx_Q) > 3 )then
          !  info = 3
          !  call add(n_Q_qmk)
          elseif( rec%obs(vidx_P) == PREPBUFR_MISS )then
            info = 4
            call add(n_Pobs_miss)
          !elseif( rec%iqmk(vidx_P) > 3 )then
          !  info = 5
          !  call add(n_P_qmk)
          elseif( rec%obs(vidx_T) == PREPBUFR_MISS )then
            info = 6
            call add(n_Tobs_miss)
          !elseif( rec%iqmk(vidx_T) > 3 )then
          !  info = 7
          !  call add(n_T_qmk)
          endif
          !-----------------------------------------------------
          ! Convert RHerr to Qerr
          !-----------------------------------------------------
          if( info == 0 )then
            call add(n_ok)
            Qerr = conv_RHerr_to_Qerr(&
                     rec%obs(vidx_Q)*1d-6, rec%obs(vidx_P), degC_to_K(rec%obs(vidx_T)), &
                     rec%oer(vidx_Q)) * 1d6  ![kg/kg] -> [mg/kg]
          else
            Qerr = PREPBUFR_MISS
          endif
          !-----------------------------------------------------
          ! Output for monitoring
          !-----------------------------------------------------
          if( monit%save_modif )then
            selectcase( info )
            case( 1:2 )
              continue
            case( 0, 3:7 )
              write(un,wfmt) &
                    info, imsg, isub, irec, sub%lon, sub%lat, &
                    rec%obs(vidx_P), rec%obs(vidx_Q), rec%oer(vidx_Q), Qerr
            case default
              call eerr('Invalid value in $info: '//str(info))
            endselect
          endif
          !-----------------------------------------------------
          ! Update value of the dataset
          !-----------------------------------------------------
          rec%oer(vidx_Q) = Qerr
          !-----------------------------------------------------
        enddo  ! irec/
      enddo  ! isub/
    enddo  ! imsg/

    if( monit%save_modif )then
      write(un,"(a)") '# Report'
    endif
    call report_n(  'all        ')
    if( n_all > 0 )then
      call report_n('OK         ', n_ok       )
      call report_n('NG         ', n_all-n_ok )
      call report_n('  Qobs_miss', n_Qobs_miss)
      call report_n('  Qoer_miss', n_Qoer_miss)
      !call report_n('  Q_qmk    ', n_Q_qmk    )
      call report_n('  Pobs_miss', n_Pobs_miss)
      !call report_n('  P_qmk    ', n_P_qmk    )
      call report_n('  Tobs_miss', n_Tobs_miss)
      !call report_n('  T_qmk    ', n_T_qmk    )
    endif
  
    if( monit%save_modif )then
      close(un)
    endif
  !-------------------------------------------------------------
  ! Case: Do not use
  case( OPT_QERR__DO_NOT_USE )
    do imsg = 1, pb%nmsg
      msg => pb%msg(imsg)
      do isub = 1, msg%nsub
        sub => msg%sub(isub)
        do irec = 1, sub%nrec
          rec => sub%rec(irec)

          if( rec%oer(vidx_Q) /= PREPBUFR_MISS )then
            rec%oer(vidx_Q) = PREPBUFR_MISS
          endif
        enddo
      enddo
    enddo
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr('Invalid value in opt_Qerr: '//str(opt_Qerr))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine report_n(nam, n)
  implicit none
  character(*), intent(in) :: nam
  integer, intent(in), optional :: n

  character(CLEN_MSG) :: msg

  if( present(n) )then
    msg = nam//': '//str(n,dgt(n_all))//' '//str(real(n)/n_all*1e2,'f6.2')//'%'
  else
    msg = nam//': '//str(n_all)
  endif

  call edbg(trim(msg))
  if( monit%save_modif )then
    call echo(un, trim(msg))
  endif
end subroutine report_n
!---------------------------------------------------------------
end subroutine conv_Qerr
!===============================================================
!
!===============================================================
subroutine conv_Tv(pb, pb0, opt_Tv, monit)
  use mod_pb, only: &
        dgt_nmsg, &
        dgt_nsub, &
        dgt_nrec
  implicit none
  type(pb_)      , intent(in), target :: pb
  type(prepbufr_), intent(in) :: pb0
  integer        , intent(in) :: opt_Tv
  type(monit_)   , intent(in) :: monit

  type(msg_), pointer :: msg
  type(sub_), pointer :: sub
  type(rec_), pointer :: rec
  integer :: imsg, isub, irec
  integer :: info
  integer :: n_all, &
             n_ok, &
             n_Tobs_miss, &
             n_not_Tv, &
             n_Qobs_miss

  character(CLEN_PATH) :: path_monit
  integer :: un
  character(64) :: wfmt

  call echo(code%bgn, 'conv_Tv', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( monit%save_modif )then
    path_monit = joined(monit%dir,'Tv.'//trim(pb%msgtyp)//'.txt')
    call edbg('Monitoring output: '//str(path_monit))
    un = unit_number()
    open(un, file=path_monit, status='replace')
    write(un,"(a)") 'info imsg isub irec lon lat P[hPa] Tv[degC] T[degC]'

    wfmt = "(1x,i2,1x,i"//str(dgt_nmsg())//",1x,i"//str(dgt_nsub())//&
           ",1x,i"//str(dgt_nrec())//",2(1x,f8.3),4(1x,es11.4))"
  endif

  n_all       = 0
  n_ok        = 0
  n_Tobs_miss = 0
  n_not_Tv    = 0
  n_Qobs_miss = 0

  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)

      if( sub%report_type /= REPORT_TYPE_MASS ) cycle

      do irec = 1, sub%nrec
        rec => sub%rec(irec)

        rec%obs(vidx_Tv) = PREPBUFR_MISS
        rec%oer(vidx_Tv) = PREPBUFR_MISS
        rec%qmk(vidx_Tv) = PREPBUFR_MISS
        rec%iqmk(vidx_Tv) = UB_WGT_QLT

        call add(n_all)

        info = 0
        !-------------------------------------------------------
        ! Case: No obs. of Temp. (INFO=1)
        if( rec%obs(vidx_T) == PREPBUFR_MISS )then
          call add(n_Tobs_miss)
          info = 1
        !-------------------------------------------------------
        ! Case: Temp. is not Tv (INFO=2)
        elseif( pb0%msg(imsg)%sub(isub)%rec(irec)%pcd(vidx_T) /= PCD_TV )then
          call add(n_not_Tv)
          info = 2
        endif
        !-------------------------------------------------------
        ! Copy values of T to Tv if temp. is Tv
        !-------------------------------------------------------
        if( info == 0 )then
          rec%obs(vidx_Tv) = rec%obs(vidx_T)
          rec%oer(vidx_Tv) = rec%oer(vidx_T)
          rec%qmk(vidx_Tv) = rec%qmk(vidx_T)
          rec%iqmk(vidx_Tv) = rec%iqmk(vidx_T)
        endif
        !-------------------------------------------------------
        ! Case: No obs. of Q (INFO=3). Cannot conv. Tv
        if( info == 0 )then
          if( rec%obs(vidx_Q) == PREPBUFR_MISS )then
            call add(n_Qobs_miss)
            info = 3
          endif
        endif
        !-------------------------------------------------------
        ! Conv. Tv to T
        !-------------------------------------------------------
        if( info == 0 )then
          call add(n_ok)

          rec%obs(vidx_T) &
            = K_to_degC(conv_vtemp_to_temp(&
                        degC_to_K(rec%obs(vidx_Tv)), rec%obs(vidx_Q)*1d-6))
          rec%oer(vidx_T) = rec%oer(vidx_Tv)
          rec%qmk(vidx_T) = rec%qmk(vidx_Tv)

          selectcase( opt_Tv )
          !-----------------------------------------------------
          ! Cases: T converted from Tv is used
          ! * The quality $iqmk is set to the value correspondant
          !   to the option.
          case( OPT_TV__CONV_TO_T_Q1 )
            rec%iqmk(vidx_T) = 1
          case( OPT_TV__CONV_TO_T_Q2 )
            rec%iqmk(vidx_T) = 2
          case( OPT_TV__CONV_TO_T_Q3 )
            rec%iqmk(vidx_T) = 3
          case( OPT_TV__CONV_TO_T_Q4 )
            rec%iqmk(vidx_T) = 4
          !-----------------------------------------------------
          ! Case: T converted from Tv is not used
          ! * The quality $iqmk is set to 5 so that the weight
          !   $wgt_qlt is zero and T converted from Tv is not used.
          case( OPT_TV__DO_NOT_USE )
            rec%iqmk(vidx_T) = 5   ! 5 = UB_WGT_QLT; wgt_qlt(5) is always zero
          !-----------------------------------------------------
          ! Case: Use Tv as Tv (not active for ClimaX)
          ! * Quality $iqmk of T is set to 5 so that the weight
          !   $wgt_qlt is zero and T converted from Tv is not used.
          ! * Quality of Tv does not matter because the all Tv
          !   will have the same value of quality as this step.
          case( OPT_TV__USE_AS_TV )
            rec%iqmk(vidx_T) = 5
            rec%iqmk(vidx_Tv) = 2
          !-----------------------------------------------------
          ! Case: ERROR
          case default
            call eerr('Invalid value in $opt_Tv: '//str(opt_Tv))
          endselect
        endif
        !-------------------------------------------------------
        ! Output for monitoring
        !-------------------------------------------------------
        if( monit%save_modif )then
          if( info /= 1 )then
            write(un,wfmt) &
                  info, imsg, isub, irec, sub%lon, sub%lat, &
                  rec%obs(vidx_P), rec%obs(vidx_Q), rec%obs(vidx_Tv), rec%obs(vidx_T)
          endif
        endif
        !-------------------------------------------------------
      enddo  ! irec/
    enddo  ! isub/
  enddo  ! imsg/

  if( monit%save_modif )then
    write(un,"(a)") '# Report'
  endif
  call report_n(  'all        ')
  if( n_all > 0 )then
    call report_n('OK         ', n_ok    )
    call report_n('NG         ', n_all-n_ok )
    call report_n('  Tobs_miss', n_Tobs_miss)
    call report_n('  not_Tv   ', n_not_Tv   )
    call report_n('  Qobs_miss', n_Qobs_miss)
  endif

  if( monit%save_modif )then
    close(un)
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine report_n(nam, n)
  implicit none
  character(*), intent(in) :: nam
  integer, intent(in), optional :: n

  character(CLEN_MSG) :: msg

  if( present(n) )then
    msg = nam//': '//str(n,dgt(n_all))//' '//str(real(n)/n_all*1e2,'f6.2')//'%'
  else
    msg = nam//': '//str(n_all)
  endif

  call edbg(trim(msg))
  if( monit%save_modif )then
    call echo(un, trim(msg))
  endif
end subroutine report_n
!---------------------------------------------------------------
end subroutine conv_Tv
!===============================================================
!
!===============================================================
subroutine modify_surface(pb, opt_sfc, lapse_rate, geo, monit)
  use mod_pb, only: &
        dgt_nmsg, &
        dgt_nsub, &
        dgt_nrec
  use mod_sphere, only: &
        find_ilon, &
        find_ilat
  use mod_share, only: &
        nlon, nlat, lonb, latb, f_elv
  implicit none
  type(pb_)   , intent(in), target :: pb
  integer     , intent(in)         :: opt_sfc
  real(8)     , intent(in)         :: lapse_rate
  type(model_), intent(in), target :: geo
  type(monit_), intent(in)         :: monit

  type(msg_), pointer :: msg
  type(sub_), pointer :: sub, sub_prev
  type(rec_), pointer :: rec
  integer :: imsg, isub
  integer :: ilon, ilat
  integer :: jlon, jlat
  real(8) :: z, z0
  real(8) :: T
  real(8) :: Ps
  integer :: info
  type(file_), pointer :: f
  integer :: n_all, n_ok, &
             n_Pobs_miss, n_P_qmk, n_Tobs_miss, &
             n_noGeo
  real(4), allocatable :: model_elv(:,:)
  real(4), allocatable :: geo_elv(:,:)
  logical :: is_same

  character(CLEN_PATH) :: path_monit
  integer :: un
  character(64) :: wfmt

  call echo(code%bgn, 'modify_surface', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( .not. pb%is_surface )then
    call echo(code%ret)
    return
  endif

  if( opt_sfc /= OPT_SFC__MODIFY )then
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Read elevation map
  !-------------------------------------------------------------
  allocate(model_elv(nlon,nlat))
  call edbg('Reading model_elv '//fileinfo(f_elv))
  call rbin(model_elv, f_elv%path, f_elv%dtype, f_elv%endian, f_elv%rec)
  !call showarr(model_elv, 'model_elv')

  allocate(geo_elv(geo%nlon,geo%nlat))
  f => geo%f_elv
  call edbg('Reading geo_elv '//str(f%path))
  call rbin(geo_elv, f%path, f%dtype, f%endian, f%rec)
  if( .not. geo%is_south_to_north )then
    call reverse(geo_elv,2)
  endif
  !call showarr(geo_elv, 'geo_elv')

  do jlat = 1, geo%nlat
    do jlon = 1, geo%nlon
      if( geo_elv(jlon,jlat) == geo%elv_miss )then
        geo_elv(jlon,jlat) = 0.d0
      endif
    enddo
  enddo
  !-------------------------------------------------------------
  ! Conv. Ps and T of the subsets whose report type is MASS 
  !-------------------------------------------------------------
  if( monit%save_modif )then
    path_monit = joined(monit%dir,'sfc.'//trim(pb%msgtyp)//'.txt')
    call edbg('Monitoring output: '//str(path_monit))
    un = unit_number()
    open(un, file=path_monit, status='replace')
    write(un,"(1x,a)") 'info imsg isub lon lat z0[m] T0[degC] Ps0[hPa] z[m] T0[degC] Ps[hPa]'

    wfmt = "(1x,i2,1x,i"//str(dgt_nmsg())//",1x,i"//str(dgt_nsub())//","//&
            "2(1x,f8.3),6(1x,es11.4))"
  endif

  n_all       = 0
  n_ok        = 0
  n_Pobs_miss = 0
  n_P_qmk     = 0
  n_Tobs_miss = 0
  n_noGeo     = 0

  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)

      if( sub%nrec > 1 )then
        call eerr('Unexpected condition.'//&
                '\n  sub%nrec > 1')
      endif

      rec => sub%rec(1)
      !---------------------------------------------------------
      ! Skip if it is a WIND report
      !---------------------------------------------------------
      if( sub%report_type /= REPORT_TYPE_MASS ) cycle
      !---------------------------------------------------------
      ! Get grid indices
      !---------------------------------------------------------
      if( find_ilon(sub%lon, nlon, lonb, ilon) /= 0 )then
        call eerr('Error in function find_ilon for ilon'//&
                '\n  pb%msg('//str(imsg)//')%sub('//str(isub)//&
                  ') (lon,lat) = ('//str((/sub%lon,sub%lat/),'f8.3')//')')
      endif
      if( find_ilat(sub%lat, nlat, latb, ilat) /= 0 )then
        call eerr('Error in function find_ilat for ilat'//&
                '\n  pb%msg('//str(imsg)//')%sub('//str(isub)//&
                  ') (lon,lat) = ('//str((/sub%lon,sub%lat/),'f8.3')//')')
      endif

      if( find_ilon(sub%lon, geo%nlon, geo%lonb, jlon) /= 0 )then
        call eerr('Error in function find_ilon for jlon'//&
                '\n  pb%msg('//str(imsg)//')%sub('//str(isub)//&
                  ') (lon,lat) = ('//str((/sub%lon,sub%lat/),'f8.3')//')')
      endif
      if( find_ilat(sub%lat, geo%nlat, geo%latb, jlat) /= 0 )then
        call eerr('Error in function find_ilat for jlat'//&
                '\n  pb%msg('//str(imsg)//')%sub('//str(isub)//&
                  ') (lon,lat) = ('//str((/sub%lon,sub%lat/),'f8.3')//')')
      endif
      !---------------------------------------------------------
      !
      !---------------------------------------------------------
      call add(n_all)
      !---------------------------------------------------------
      ! Check if it is possible to modify T and Ps
      !---------------------------------------------------------
      info = 0
      if( rec%obs(vidx_P) == PREPBUFR_MISS )then
        info = 1
        call add(n_Pobs_miss)
      !elseif( rec%iqmk(vidx_P) > 3 )then
      !  info = 2
      !  call add(n_P_qmk)
      elseif( rec%obs(vidx_T) == PREPBUFR_MISS )then
        info = 3
        call add(n_Tobs_miss)
      endif

      ! TODO: prep. data for Antarctic
      if( info == 0 )then
        if( sub%lat < -60.d0 )then
          info = -1
          call add(n_noGeo)
        endif
      endif
      !---------------------------------------------------------
      ! Modify T and Ps
      !---------------------------------------------------------
      z = dble(model_elv(ilon,ilat))
      z0 = dble(geo_elv(jlon,jlat))

      selectcase( info )
      case( 0 )
        call add(n_ok)
        T = K_to_degC(calc_T_tropo(z, z0, degC_to_K(rec%obs(vidx_T)), lapse_rate))
        Ps = calc_P_tropo(z, z0, degC_to_K(rec%obs(vidx_T)), rec%obs(vidx_P), lapse_rate)
      case( -1 )
        call add(n_ok)
        T = rec%obs(vidx_T)
        Ps = rec%obs(vidx_P)
      case( 1:3 )
        T = PREPBUFR_MISS
        Ps = PREPBUFR_MISS
      case default
        call eerr('Invalid value in $info: '//str(info))
      endselect
      !---------------------------------------------------------
      ! Output for monitoring
      !---------------------------------------------------------
      if( monit%save_modif )then
        if( info /= 1 )then
          write(un,wfmt) &
                info, imsg, isub, sub%lon, sub%lat, &
                z0, rec%obs(vidx_T), rec%obs(vidx_P), z, T, Ps
        endif
      endif
      !---------------------------------------------------------
      ! Update values of the dataset
      !---------------------------------------------------------
      selectcase( info )
      case( 0 )
        rec%obs(vidx_T) = T
        rec%obs(vidx_P) = Ps
      case( -1 )
        continue
      case( 1:3 )
        rec%obs(vidx_P) = PREPBUFR_MISS
        rec%oer(vidx_P) = PREPBUFR_MISS
        rec%qmk(vidx_P) = PREPBUFR_MISS
        rec%iqmk(vidx_P) = UB_WGT_QLT
      case default
        call eerr('Invalid value in $info: '//str(info))
      endselect
      !---------------------------------------------------------
    enddo  ! isub/
  enddo  ! imsg/

  if( monit%save_modif )then
    write(un,"(a)") '# Report'
  endif
  call report_n(  'all        ')
  if( n_all > 0 )then
    call report_n('OK         ', n_ok       )
    call report_n('  noGeo    ', n_noGeo    )
    call report_n('NG         ', n_all-n_ok )
    call report_n('  Pobs_miss', n_Pobs_miss)
    !call report_n('  P_qmk    ', n_P_qmk    )
    call report_n('  Tobs_miss', n_Tobs_miss)
  endif

  if( monit%save_modif )then
    close(un)
  endif

  deallocate(model_elv)
  deallocate(geo_elv)
  !-------------------------------------------------------------
  ! Copy converted Ps of the subsets whose report type is MASS
  ! to Ps of the subsets whose report type is WIND
  !-------------------------------------------------------------
  imsg = 0
  isub = 0
  do
    !-----------------------------------------------------------
    if( imsg == 0 .and. isub == 0 )then
      nullify(sub_prev)
      imsg = 1
      isub = 1
    else
      sub_prev => pb%msg(imsg)%sub(isub)
      if( isub == pb%msg(imsg)%nsub )then
        imsg = imsg + 1
        isub = 1
      else
        isub = isub + 1
      endif
    endif
    if( imsg > pb%nmsg ) exit
    sub => pb%msg(imsg)%sub(isub)
    !-----------------------------------------------------------
    ! Go to next loop if new SID or different time
    !-----------------------------------------------------------
    is_same = .false.
    if( associated(sub_prev) )then
      if( sub%sid == sub_prev%sid .and. sub%hdr(IHDR_DHR) == sub_prev%hdr(IHDR_DHR) )then
        is_same = .true.
      endif
    endif

    if( .not. is_same ) cycle
    !-----------------------------------------------------------
    ! Copy 
    !-----------------------------------------------------------
    selectcase( sub%report_type )
    case( REPORT_TYPE_MASS )
      selectcase( sub_prev%report_type )
      case( REPORT_TYPE_WIND )
        sub_prev%rec(1)%obs(vidx_P) = sub%rec(1)%obs(vidx_P)
      case( REPORT_TYPE_MASS )
        continue
      case( REPORT_TYPE_UNDEF )
        call eerr('Unexpected value in $sub_prev%report_type: '//str(sub_prev%report_type))
      case default
        call eerr('Invalid value in $sub_prev%report_type: '//str(sub_prev%report_type))
      endselect
    case( REPORT_TYPE_WIND )
      selectcase( sub_prev%report_type )
      case( REPORT_TYPE_MASS )
        sub%rec(1)%obs(vidx_P) = sub_prev%rec(1)%obs(vidx_P)
      case( REPORT_TYPE_WIND )
        continue
      case( REPORT_TYPE_UNDEF )
        call eerr('Unexpected value in $sub_prev%report_type: '//str(sub_prev%report_type))
      case default
        call eerr('Invalid value in $sub_prev%report_type: '//str(sub_prev%report_type))
      endselect
    case( REPORT_TYPE_UNDEF )
      call eerr('Unexpected value in $sub%report_type: '//str(sub%report_type))
    case default
      call eerr('Invalid value in $sub%report_type: '//str(sub%report_type))
    endselect
  enddo  ! for all subsets/
  !-------------------------------------------------------------
  call echo(code%ret)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
subroutine report_n(nam, n)
  implicit none
  character(*), intent(in) :: nam
  integer, intent(in), optional :: n

  character(CLEN_MSG) :: msg

  if( present(n) )then
    msg = nam//': '//str(n,dgt(n_all))//' '//str(real(n)/n_all*1e2,'f6.2')//'%'
  else
    msg = nam//': '//str(n_all)
  endif

  call edbg(trim(msg))
  if( monit%save_modif )then
    call echo(un, trim(msg))
  endif
end subroutine report_n
!---------------------------------------------------------------
end subroutine modify_surface
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
subroutine output_obs(&
    f_obs, pb, itime, msgtyp, var, obserr_scale, obserr)
  use common, only: &
        ! var.
        r_size, &
        ! proc.
        com_randn
  use mod_pb, only: &
        typ_to_idx_msgtyp
  use mod_base, only: &
        var_to_id_obs, &
        var_to_idx_obs, &
        var_to_idx_obserr
  use mod_stats, only: &
        get_stats_output
  implicit none
  character(*), intent(in) :: f_obs
  type(pb_), intent(in), target :: pb
  integer, intent(in) :: itime
  character(*), intent(in) :: msgtyp
  character(*), intent(in) :: var
  real(8), intent(in) :: obserr_scale
  real(8), intent(in) :: obserr(:)

  type(sub_), pointer :: sub
  type(rec_), pointer :: rec
  integer :: imsg, isub, irec
  integer :: id_var
  integer :: imsgtyp
  integer :: vidx
  integer :: vidx_obserr
  integer :: irec_selected
  real(4) :: dat(7)
  real(r_size), allocatable :: rand(:)

  integer :: un

  call echo(code%bgn, 'output_obs')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg(str(pb%nrec_selected)//' in '//str(pb%nrec_valid)//&
            ' valid observations are selected')
  !-------------------------------------------------------------
  ! Open output file
  !-------------------------------------------------------------
  call edbg('Updating '//str(f_obs))
  un = unit_number()
  open(un, file=f_obs, form='unformatted', access='sequential', &
       status='old', action='readwrite', position='append')
  !-------------------------------------------------------------
  ! Get indices and id
  !-------------------------------------------------------------
  imsgtyp = typ_to_idx_msgtyp(msgtyp)

  id_var = var_to_id_obs(var, swap_p_ps=pb%is_surface)

  vidx = var_to_idx_obs(var, nobs) ! Do not swap P ans Ps
  vidx_obserr = var_to_idx_obserr(var, swap_p_ps=pb%is_surface)
  call edbg('id: '//str(id_var)//' obserr: '//str(obserr(vidx_obserr)))
  !-------------------------------------------------------------
  ! Get stats.
  !-------------------------------------------------------------
  call get_stats_output('valid'   , pb, itime, imsgtyp, vidx)
  call get_stats_output('selected', pb, itime, imsgtyp, vidx)
  !-------------------------------------------------------------
  ! Return if no output
  !-------------------------------------------------------------
  if( pb%nrec_selected == 0 )then
    close(un)
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Process data and output
  !-------------------------------------------------------------
  allocate(rand(pb%nrec_selected))
  call com_randn(pb%nrec_selected, rand)

  irec_selected = 0
  do imsg = 1, pb%nmsg
    do isub = 1, pb%msg(imsg)%nsub
      sub => pb%msg(imsg)%sub(isub)
      do irec = 1, sub%nrec
        rec => sub%rec(irec)
        if( .not. rec%is_selected ) cycle
        call add(irec_selected)
        !-------------------------------------------------------
        !
        !-------------------------------------------------------
        dat(1) = real(id_var,4)
        dat(2) = real(sub%lon,4)
        dat(3) = real(sub%lat,4)
        dat(4) = real(rec%obs(vidx_p),4)  ! hPa
        dat(5) = real(rec%obs(vidx),4)
        if( rec%oer(vidx) == PREPBUFR_MISS )then
          dat(6) = abs(real(rand(irec_selected) * obserr(vidx_obserr),4))
        else
          dat(6) = abs(real(rec%oer(vidx),4))
        endif

        selectcase( var )
        case( var_t )
          dat(5) = real(degC_to_K(real(dat(5),8)),4)  ! degC -> K
        case( var_q )
          dat(5) = dat(5) * 1e-6  ! mg/kg -> kg/kg
          dat(6) = dat(6) * 1e-6  ! mg/kg -> kg/kg
        case default
          continue
        endselect

        dat(7) = real(imsgtyp,4)
        !-------------------------------------------------------
        !
        !-------------------------------------------------------
        dat(6) = dat(6) * obserr_scale
        !-------------------------------------------------------
        write(un) dat
      enddo  ! irec/
    enddo  ! isub/
  enddo  ! imsg/

  deallocate(rand)
  !-------------------------------------------------------------
  ! Close output file
  !-------------------------------------------------------------
  close(un)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
!  if( monit%save_modif )then
!    open(un, file=f, status='replace')
!    do imsg = 1, pb%nmsg
!      msg => pb%msg(imsg)
!      do isub = 1, msg%nsub
!        sub => msg%sub(isub)
!        do irec = 1, sub%nrec
!          rec => sub%rec(irec)
!
!          write(un,wfmt) irec_all, imsg, isub, irec, rec%is_selected
!        enddo
!      enddo
!    enddo
!    close(un)
!  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine output_obs
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
subroutine free_pb(pb)
  implicit none
  type(pb_), intent(inout) :: pb

  call echo(code%bgn, 'free_pb', '-p -x2')
  !-------------------------------------------------------------
  pb%msgtyp = ''
  if( pb%nmsg > 0 )then
    deallocate(pb%msg)
    pb%nmsg = 0
  endif
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine free_pb
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
subroutine write_monit_summary(&
    monit, pb, order, var, itime)
  use mod_base, only: &
        get_s_time, &
        var_to_idx_obs
  implicit none
  type(monit_), intent(in) :: monit
  type(pb_)   , intent(in), target :: pb
  type(order_), intent(in) :: order
  character(*), intent(in) :: var
  integer     , intent(in) :: itime

  type(msg_)      , pointer :: msg
  type(sub_)      , pointer :: sub
  type(rec_)      , pointer :: rec
  type(rec_local_), pointer :: rec_local
  type(grid_)     , pointer :: g
  type(grid_lay_) , pointer :: glay
  integer :: ilon, ilat, ilev
  integer :: ilon_near, ilat_near, ilev_near
  integer :: ilon_blng, ilat_blng, ilev_blng
  integer :: irec_local
  integer :: ilay_all
  integer :: imsg, isub, irec
  integer :: vidx
  integer, allocatable :: lst_lay_all_ilon(:)
  integer, allocatable :: lst_lay_all_ilat(:)
  integer, allocatable :: lst_lay_all_ilev(:)
  real(8), allocatable :: dnst(:,:,:)
  integer, allocatable :: num_vald(:,:,:), num_slct(:,:,:)
  character(clen_path) :: fid
  character(clen_path) :: f
  integer :: un

  call echo(code%bgn, 'write_monit_summary')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  fid = str(order%msgtyp)//'_'//str(var)//'_'//&
        str(get_s_time(itime))

  vidx = var_to_idx_obs(var, nobs)
  !-------------------------------------------------------------
  ! Density of each layer of all obs.
  !-------------------------------------------------------------
  allocate(dnst(nlon,nlat,nlev))
  dnst(:,:,:) = 0.d0

  do ilat = 1, nlat
  do ilon = 1, nlon
    g => grid(ilon,ilat)
    do ilev = 1, nlev
      glay => g%lay(ilev)
      do irec_local = 1, glay%nrec_local
        rec_local => glay%rec_local(irec_local)
        rec => pb%msg(rec_local%imsg)%sub(rec_local%isub)%rec(rec_local%irec)
        if( .not. rec%is_valid ) cycle

        call add(dnst(ilon,ilat,ilev), rec_local%wgt_dist * order%wgt_qlt(rec%iqmk(vidx)))
      enddo  ! irec_local/
    enddo  ! ilev/
  enddo  ! ilon/
  enddo  ! ilat/

  f = joined(monit%dir,'dnst_lay_vald_'//trim(fid)//'.bin')
  call edbg('Writing '//str(f))
  call wbin(dnst, f, replace=.true.)

  deallocate(dnst)
  !-------------------------------------------------------------
  ! Density of each layer of selected obs.
  !-------------------------------------------------------------
  allocate(dnst(nlon,nlat,nlev))
  dnst(:,:,:) = 0.d0

  do ilat = 1, nlat
  do ilon = 1, nlon
    g => grid(ilon,ilat)
    do ilev = 1, nlev
      glay => g%lay(ilev)
      do irec_local = 1, glay%nrec_local
        rec_local => glay%rec_local(irec_local)
        rec => pb%msg(rec_local%imsg)%sub(rec_local%isub)%rec(rec_local%irec)
        if( .not. rec%is_valid ) cycle

        if( rec%is_selected )then
          call add(dnst(ilon,ilat,ilev), rec_local%wgt_dist * order%wgt_qlt(rec%iqmk(vidx)))
        endif
      enddo  ! irec_local/
    enddo  ! ilev/
  enddo  ! ilon/
  enddo  ! ilat/

  f = joined(monit%dir,'dnst_lay_slct_'//trim(fid)//'.bin')
  call edbg('Writing '//str(f))
  call wbin(dnst, f, replace=.true.)

  deallocate(dnst)
  !-------------------------------------------------------------
  ! All records
  !-------------------------------------------------------------
  allocate(lst_lay_all_ilon(nlay_all))
  allocate(lst_lay_all_ilat(nlay_all))
  allocate(lst_lay_all_ilev(nlay_all))
  ilay_all = 0
  do ilat = 1, nlat
  do ilon = 1, nlon
  do ilev = 1, nlev
    call add(ilay_all)
    lst_lay_all_ilon(ilay_all) = ilon
    lst_lay_all_ilat(ilay_all) = ilat
    lst_lay_all_ilev(ilay_all) = ilev
  enddo
  enddo
  enddo

  allocate(num_vald(nlon,nlat,nlev))
  allocate(num_slct(nlon,nlat,nlev))
  num_vald(:,:,:) = 0
  num_slct(:,:,:) = 0

  f = joined(monit%dir,'rec_all_'//trim(fid)//'.txt')
  call edbg('Writing '//str(f))
  un = unit_number()
  open(un, file=f, status='replace')
  write(un,"(1x,a)") &
        'lon lat lev iqmk obs oer '//&
        'ilon_near ilat_near ilev_near '//&
        'ilon_blng ilat_blng ilev_blng '//&
        'wgtmax '//&
        'is_valid is_selected'
  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)
      do irec = 1, sub%nrec
        rec => sub%rec(irec)
        if( rec%obs(vidx) == PREPBUFR_MISS ) cycle

        if( rec%ilay_all_nearest == 0 )then
          ilon_near = 0
          ilat_near = 0
          ilev_near = 0
        else
          ilon_near = lst_lay_all_ilon(rec%ilay_all_nearest)
          ilat_near = lst_lay_all_ilat(rec%ilay_all_nearest)
          ilev_near = lst_lay_all_ilev(rec%ilay_all_nearest)
        endif

        if( rec%ilay_all_belong == 0 )then
          ilon_blng = 0
          ilat_blng = 0
          ilev_blng = 0
        else
          ilon_blng = lst_lay_all_ilon(rec%ilay_all_belong)
          ilat_blng = lst_lay_all_ilat(rec%ilay_all_belong)
          ilev_blng = lst_lay_all_ilev(rec%ilay_all_belong)
        endif

        if( rec%ilay_all_nearest /= 0 )then
          if( rec%is_valid )then
            call add(num_vald(ilon_near,ilat_near,ilev_near))
          endif
          if( rec%is_selected )then
            call add(num_slct(ilon_near,ilat_near,ilev_near))
          endif
        endif

        write(un,"(2(1x,f6.1),1x,es10.3,1x,i2,2(1x,es9.2),"//&
                  "3(1x,i2),3(1x,i2),1x,es9.2,2(1x,l1))") &
              sub%lon, sub%lat, rec%obs(vidx_P), rec%iqmk(vidx), &
              rec%obs(vidx), rec%oer(vidx), &
              ilon_near, ilat_near, ilev_near, &
              ilon_blng, ilat_blng, ilev_blng, &
              rec%wgt_dist_max, &
              rec%is_valid, rec%is_selected
      enddo  ! irec/
    enddo  ! isub/
  enddo  ! imsg/
  close(un)

  f = joined(monit%dir,'num_vald_'//str(fid)//'.bin')
  call edbg('Writing '//str(f))
  call wbin(num_vald, f, replace=.true.)

  f = joined(monit%dir,'num_slct_'//str(fid)//'.bin')
  call edbg('Writing '//str(f))
  call wbin(num_slct, f, replace=.true.)

  deallocate(num_vald)
  deallocate(num_slct)
  !-------------------------------------------------------------
  ! Sum. of weights of local records of each grid
  !-------------------------------------------------------------
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine write_monit_summary
!===============================================================
!
!===============================================================
end module mod_common
