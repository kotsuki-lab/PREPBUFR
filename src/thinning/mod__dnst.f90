module mod__dnst
  use lib_const
  use lib_time
  use lib_log
  use lib_array
  use lib_math
  use mod_share
  use mod_sphere, only: &
        calc_dist_h, &
        calc_dist_v, &
        calc_wgt_dist
  use def_const
  use def_type
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: select_records
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  integer :: nrec_updated
  integer, allocatable :: lst_irec_updated(:)
  real(8), allocatable :: lst_rec_valid_cntrb(:)
  real(8), allocatable :: lst_cntrb(:)
  logical, allocatable :: lst_rec_valid_is_updated(:)

  type(lst_idx_same_id_), pointer :: lst_idx_same_id(:)

!  real(8) :: dnst_thresh
!  real(8) :: dist_h_thresh, dist_v_thresh
!  real(8) :: rho_h, rho_v

  integer :: dgt_xy
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine select_records(&
    pb, lst_lay_all, lst_rec_valid, &
    order, earth_r, monit)
  use mod_common, only: &
        pnt_lst_idx_same_id
  implicit none
  type(pb_)       , intent(inout), target :: pb
  type(lay_all_)  , intent(inout)         :: lst_lay_all(:)
  type(rec_valid_), intent(inout)         :: lst_rec_valid(:)
  type(order_)    , intent(in)            :: order
  real(8)         , intent(in)            :: earth_r
  type(monit_)    , intent(in)            :: monit

  integer :: imsg, isub, irec
  integer :: info
  integer :: imax, i
  integer :: irec_valid
  type(timer_), allocatable :: ptime_select(:)
  type(timer_), allocatable :: ptime_add(:)

  call echo(code%bgn, 'select_records')
  !-------------------------------------------------------------
  allocate(ptime_select(0:8))
  allocate(ptime_add(0:8))
  call init_timer(ptime_select)
  call init_timer(ptime_add)

  if( order%nmax > 0 )then
    imax = order%nmax
  else
    imax = pb%nrec_valid
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nrec_updated = pb%nrec_valid

  allocate(lst_irec_updated(pb%nrec_valid))
  do irec_valid = 1, pb%nrec_valid
    lst_irec_updated(irec_valid) = irec_valid
  enddo

  allocate(lst_rec_valid_cntrb(pb%nrec_valid))

  allocate(lst_cntrb(maxval(lst_rec_valid(:)%nlay_local)))

  allocate(lst_rec_valid_is_updated(pb%nrec_valid))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(lst_idx_same_id)
  call pnt_lst_idx_same_id(lst_idx_same_id, order%code_same_id)

!  dnst_thresh = order%dnst_thresh
!  dist_h_thresh = order%dist_h_thresh
!  dist_v_thresh = order%dist_v_thresh
!  rho_h = order%rho_h
!  rho_v = order%rho_v
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  dgt_xy = dgt(max(nlon,nlat))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  pb%nrec_selected = 0

  i = 0
  do while( i < imax )
    i = i + 1

    call select_record_core(&
           pb, lst_lay_all, lst_rec_valid, &
           order, &
           imsg, isub, irec, info, &
           monit, i, ptime_select)
    if( info == -1 ) exit

    call add_record_core(&
           pb, lst_lay_all, lst_rec_valid, &
           order, earth_r, &
           imsg, isub, irec, info, &
           monit, i, ptime_add)
    if( info == -1 ) exit
  enddo

  if( i == order%nmax )then
    call edbg('Num. of selected records reached the ulim. of '//str(order%nmax))
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  deallocate(lst_irec_updated)
  deallocate(lst_rec_valid_cntrb)
  deallocate(lst_cntrb)
  deallocate(lst_rec_valid_is_updated)

  nullify(lst_idx_same_id)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call edbg('Exec. time')
  call edbg('  select: '//str(ptime_select(0)%t_acc))
  call edbg('    update contribution: '//str(ptime_select(1)%t_acc))
  call edbg('    find target record : '//str(ptime_select(2)%t_acc))
  call edbg('  add   : '//str(ptime_add(0)%t_acc))
  call edbg('    check density            : '//str(ptime_add(1)%t_acc))
  call edbg('    update selected record   : '//str(ptime_add(2)%t_acc))
  call edbg('    update density and weight: '//str(ptime_add(3)%t_acc))
  call edbg('    process same id records  : '//str(ptime_add(4)%t_acc))
  call edbg('    set up flags             : '//str(ptime_add(5)%t_acc))
  deallocate(ptime_select)
  deallocate(ptime_add)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine select_records
!===============================================================
!
!===============================================================
subroutine select_record_core(&
    pb, lst_lay_all, lst_rec_valid, &
    order, &
    imsg, isub, irec, info, &
    monit, i, ptime)
  implicit none
  type(order_)    , intent(in)            :: order
  type(pb_)       , intent(inout), target :: pb
  type(lay_all_)  , intent(inout), target :: lst_lay_all(:)
  type(rec_valid_), intent(inout), target :: lst_rec_valid(:)
  integer, intent(out) :: imsg, isub, irec
  integer, intent(out) :: info
  type(monit_), intent(in) :: monit
  integer, intent(in)  :: i
  type(timer_), intent(inout) :: ptime(0:)

  type(lay_all_)  , pointer :: lay_all
  type(rec_local_), pointer :: rec_local
  type(rec_valid_), pointer :: rec_valid
  type(sub_), pointer :: sub
  integer :: irec_valid
  integer :: ilay_local
  integer :: irec_updated
  real(8) :: cntrb

  call echo(code%bgn, 'select_record_core', '-p -x2')
  call start_timer(ptime(0))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0
  !-------------------------------------------------------------
  !(1) Update contributions
  !-------------------------------------------------------------
  call start_timer(ptime(1))
  !$omp parallel do private(rec_valid, lay_all, rec_local, cntrb)
  do irec_updated = 1, nrec_updated
    rec_valid => lst_rec_valid(lst_irec_updated(irec_updated))

    cntrb = 0.d0
    do ilay_local = 1, rec_valid%nlay_local
      lay_all => lst_lay_all(rec_valid%ilay_all(ilay_local))

      rec_local => lay_all%rec_local(rec_valid%irec_local(ilay_local))
      if( rec_local%wgt < WGT_THRESH ) cycle

      cntrb = max(cntrb, calc_cntrb(rec_local%wgt, lay_all%dnst_used, order%dnst_thresh))
    enddo  ! ilay_local
    lst_rec_valid_cntrb(lst_irec_updated(irec_updated)) = cntrb
  enddo  ! iirec/
  !$omp end parallel do
  call stop_timer(ptime(1))
  !-------------------------------------------------------------
  !(2) Find a record with the highest contribution and layer to which it belongs
  !-------------------------------------------------------------
  call start_timer(ptime(2))
  irec_valid = maxloc(lst_rec_valid_cntrb, 1)

  if( lst_rec_valid_cntrb(irec_valid) < CNTRB_THRESH )then
    call edbg('Contribution is below threshold. Exit')
    info = -1
    call stop_timer(ptime(2))
    call stop_timer(ptime(0))
    call echo(code%ret)
    return
  endif

  rec_valid => lst_rec_valid(irec_valid)
  imsg = rec_valid%imsg
  isub = rec_valid%isub
  irec = rec_valid%irec

  lst_cntrb(:rec_valid%nlay_local) = 0.d0
  do ilay_local = 1, rec_valid%nlay_local
    lay_all => lst_lay_all(rec_valid%ilay_all(ilay_local))
    if( lay_all%dnst_used >= order%dnst_thresh ) cycle

    rec_local => lay_all%rec_local(rec_valid%irec_local(ilay_local))
    if( rec_local%wgt < WGT_THRESH ) cycle
    lst_cntrb(ilay_local) = calc_cntrb(rec_local%wgt, lay_all%dnst_used, order%dnst_thresh)
  enddo

  pb%msg(imsg)%sub(isub)%rec(irec)%ilay_all_belong &
    = rec_valid%ilay_all(maxloc(lst_cntrb(:rec_valid%nlay_local),1))

  call stop_timer(ptime(2))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( monit%monit_itr_sct_rec )then
    sub => pb%msg(imsg)%sub(isub)
    call edbg(str(i,dgt(pb%nrec_valid))//' '//str(irec_valid,dgt(pb%nrec_valid))//&
              ' ('//str((/sub%lon,sub%lat/),'f8.3',',')//')'//&
              ' contribution: '//str(lst_rec_valid_cntrb(irec_valid),'es9.2'))
  endif
  !-------------------------------------------------------------
  call stop_timer(ptime(0))
  call echo(code%ret)
end subroutine select_record_core
!===============================================================
!
!===============================================================
subroutine add_record_core(&
    pb, lst_lay_all, lst_rec_valid, &
    order, earth_r, &
    imsg, isub, irec, info, &
    monit, i, ptime)
  implicit none
  type(pb_)       , intent(inout), target :: pb
  type(lay_all_)  , intent(inout), target :: lst_lay_all(:)
  type(rec_valid_), intent(inout), target :: lst_rec_valid(:)
  type(order_)    , intent(in)            :: order
  real(8)         , intent(in)            :: earth_r
  integer, intent(in)  :: imsg, isub, irec
  integer, intent(out) :: info
  type(monit_), intent(in) :: monit
  integer, intent(in)  :: i
  type(timer_), intent(inout) :: ptime(0:)

  type(lay_local_), pointer :: lay_local
  type(lay_all_)  , pointer :: lay_all
  type(rec_)      , pointer :: rec
  type(rec_local_), pointer :: rec_local
  integer :: ilay_local
  integer :: irec_local
  real(8) :: dnst
  character(1) :: marker
  integer :: irec_valid
  real(8) :: wgt
  type(sub_), pointer :: sub, sub2
  type(rec_), pointer :: rec2
  real(8) :: lon, lat, logp
  real(8) :: dist_h, dist_v

  call echo(code%bgn, 'add_record_core', '-p -x2')
  call start_timer(ptime(0))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0
  !-------------------------------------------------------------
  ! Check if dnst. after updating is less than thresh.
  ! If not return
  !-------------------------------------------------------------
  call start_timer(ptime(1))

  sub => pb%msg(imsg)%sub(isub)
  rec => sub%rec(irec)

  do ilay_local = 1, rec%nlay_local
    lay_local => rec%lay_local(ilay_local)
    lay_all   => lst_lay_all(lay_local%ilay_all)
    rec_local => lay_all%rec_local(lay_local%irec_local)
    if( rec_local%imsg /= imsg .or. &
        rec_local%isub /= isub .or. &
        rec_local%irec /= irec )then
      call eerr('!!! INTERNAL ERROR !!! Index mismatch')
    endif

    dnst = lay_all%dnst_used + rec_local%wgt
    if( dnst > order%dnst_thresh )then
      info = -1
      marker = '*'
    else
      marker = ''
    endif

    if( monit%monit_itr_add_rec )then
      call edbg('lay '//str(lay_local%ilay_all,dgt(nlay_all))//&
                ' ('//str((/lay_all%ilon,lay_all%ilat/),dgt_xy,',')//&
                ','//str(lay_all%ilev)//') dnst: '//&
                str(lay_all%dnst_used,'es9.2')//' -> '//str(dnst,'es9.2')//&
                ' rec wgt: '//str(rec_local%wgt,'es9.2')//' '//marker)
    endif
  enddo

  call stop_timer(ptime(1))

  if( info == -1 )then
    call edbg('Dnst. of layer reached ulim. Not updated. Exit')
    call stop_timer(ptime(0))
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  ! Update info. of newly selected record
  !-------------------------------------------------------------
  call start_timer(ptime(2))

  rec%is_selected = .true.
  call add(pb%nrec_selected)
  rec%irec_selected = pb%nrec_selected

  !$omp parallel do
  do irec_valid = 1, pb%nrec_valid
    lst_rec_valid_is_updated(irec_valid) = .false.
  enddo
  !$omp end parallel do

  call stop_timer(ptime(2))
  !-------------------------------------------------------------
  ! Update density of local layers and weight of local records
  !-------------------------------------------------------------
  call start_timer(ptime(3))

  lon = sub%lon
  lat = sub%lat
  logp = rec%logp

  !$omp parallel do private(lay_local, lay_all, rec_local, wgt, sub2, rec2, dist_h, dist_v)
  do ilay_local = 1, rec%nlay_local
    lay_local => rec%lay_local(ilay_local)
    lay_all   => lst_lay_all(lay_local%ilay_all)

    wgt = lay_all%rec_local(lay_local%irec_local)%wgt
    if( wgt == 0.d0 ) cycle

    ! Update dnst of layer
    call add(lay_all%dnst_used, wgt)

    ! Set the weight of added record 0
    lay_all%rec_local(lay_local%irec_local)%wgt = 0.d0

    ! Update weight of records local to added record
    do irec_local = 1, lay_all%nrec_local
      rec_local => lay_all%rec_local(irec_local)
      lst_rec_valid_is_updated(rec_local%irec_valid) = .true.
      sub2 => pb%msg(rec_local%imsg)%sub(rec_local%isub)
      rec2 => sub2%rec(rec_local%irec)
      dist_h = calc_dist_h(lon, lat, sub2%lon, sub2%lat) * earth_r
      dist_v = calc_dist_v(logp, rec2%logp)
      if( dist_h > order%dist_h_thresh .or. dist_v > order%dist_v_thresh ) cycle
      call mul(rec_local%wgt, 1.d0 - calc_wgt_dist(dist_h, dist_v, order%rho_h, order%rho_v)**2)
    enddo
  enddo
  !$omp end parallel do

  call stop_timer(ptime(3))
  !-------------------------------------------------------------
  ! Update weights of the records that have same id to the selected record
  !-------------------------------------------------------------
  call start_timer(ptime(4))

  selectcase( order%code_same_id )
  case( CODE_SAME_ID__NONE )
    continue
  case( CODE_SAME_ID__SID_REDUCE )
    call reduce_records_same_id(&
           pb, lst_lay_all, lst_rec_valid, &
           lst_idx_same_id(rec%idx_same_sid), order%wgt_same_sid)
  case( CODE_SAME_ID__SID_LIMIT  )
    call limit_records_same_id(&
           pb, lst_lay_all, lst_rec_valid, &
           lst_idx_same_id(rec%idx_same_sid), order%nmax_same_sid, order%dnst_thresh)
  case( CODE_SAME_ID__SAID_REDUCE )
    call reduce_records_same_id(&
           pb, lst_lay_all, lst_rec_valid, &
           lst_idx_same_id(rec%idx_same_said), order%wgt_same_said)
  case( CODE_SAME_ID__SAID_LIMIT  )
    call limit_records_same_id(&
           pb, lst_lay_all, lst_rec_valid, &
           lst_idx_same_id(rec%idx_same_said), order%nmax_same_said, order%dnst_thresh)
  case default
    call eerr('Invalid value in $order%code_same_id: '//str(order%code_same_id))
  endselect

  call stop_timer(ptime(4))
  !-------------------------------------------------------------
  ! Set up flags on the updated records
  !-------------------------------------------------------------
  call start_timer(ptime(5))

  nrec_updated = 0
  do irec_valid = 1, pb%nrec_valid
    if( lst_rec_valid_is_updated(irec_valid) )then
      call add(nrec_updated)
      lst_irec_updated(nrec_updated) = irec_valid
    endif
  enddo

  call stop_timer(ptime(5))
  !-------------------------------------------------------------
  ! Check if all valid records are selected
  !-------------------------------------------------------------
  if( pb%nrec_selected == pb%nrec_valid )then
    info = -1
    call edbg('All valid records have been selected. Exit')
    call stop_timer(ptime(0))
    call echo(code%ret)
    return
  endif
  !-------------------------------------------------------------
  call stop_timer(ptime(0))
  call echo(code%ret)
end subroutine add_record_core
!===============================================================
!
!===============================================================
subroutine reduce_records_same_id(&
    pb, lst_lay_all, lst_rec_valid, lst_idx_same_id, wgt_same_id)
  implicit none
  type(pb_), intent(inout), target :: pb
  type(lay_all_), intent(in), target :: lst_lay_all(:)
  type(rec_valid_), intent(in), target :: lst_rec_valid(:)
  type(lst_idx_same_id_), intent(in), target :: lst_idx_same_id
  real(8), intent(in) :: wgt_same_id

  type(sub_), pointer :: sub
  type(rec_), pointer :: rec
  type(rec_valid_), pointer :: rec_valid
  type(rec_local_), pointer :: rec_local
  integer :: iisub
  integer :: imsg, isub, irec
  integer :: ilay_local
  integer :: ilay_all
  integer :: irec_local

  call echo(code%bgn, 'reduce_records_same_id', '-p -x2')
  !-------------------------------------------------------------
  do iisub = 1, lst_idx_same_id%nsub
    imsg = lst_idx_same_id%imsg(iisub)
    isub = lst_idx_same_id%isub(iisub)
    sub => pb%msg(imsg)%sub(isub)
    do irec = 1, sub%nrec
      rec => sub%rec(irec)
      if( .not. rec%is_valid .or. rec%is_selected ) cycle

      rec_valid => lst_rec_valid(rec%irec_valid)
      do ilay_local = 1, rec_valid%nlay_local
        ilay_all = rec_valid%ilay_all(ilay_local)
        irec_local = rec_valid%irec_local(ilay_local)
        rec_local => lst_lay_all(ilay_all)%rec_local(irec_local)
        call mul(rec_local%wgt, wgt_same_id)
      enddo
    enddo  ! irec/
  enddo  ! iisub/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine reduce_records_same_id
!===============================================================
! Set the status of validity of the records with 
! the (nrec_sameid-nrecmax+1)th smallest cntrb. invalid.
!===============================================================
subroutine limit_records_same_id(&
    pb, lst_lay_all, lst_rec_valid, lst_idx_same_id, nrecmax, dnst_thresh)
  implicit none
  type(pb_), intent(inout), target :: pb
  type(lay_all_), intent(in), target :: lst_lay_all(:)
  type(rec_valid_), intent(in), target :: lst_rec_valid(:)
  type(lst_idx_same_id_), intent(in), target :: lst_idx_same_id
  integer, intent(in) :: nrecmax
  real(8), intent(in) :: dnst_thresh

  type(sub_), pointer :: sub
  type(rec_), pointer :: rec
  type(rec_local_), pointer :: rec_local
  type(rec_valid_), pointer :: rec_valid
  type(lay_all_), pointer :: lay_all
  integer, allocatable :: lst_iisub(:)
  integer, allocatable :: lst_irec(:)
  real(8), allocatable :: lst_cntrb(:)
  integer, allocatable :: arg(:)
  integer :: imsg, isub, irec
  integer :: iisub
  integer :: ilay_local
  integer :: nrec_sameid, irec_sameid

  call echo(code%bgn, 'limit_records_same_id', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nrec_sameid = 0
  do iisub = 1, lst_idx_same_id%nsub
    imsg = lst_idx_same_id%imsg(iisub)
    isub = lst_idx_same_id%isub(iisub)
    sub => pb%msg(imsg)%sub(isub)
    do irec = 1, sub%nrec
      rec => sub%rec(irec)
      if( rec%is_valid .and. .not. rec%is_selected )then
        nrec_sameid = nrec_sameid + 1
      endif
    enddo
  enddo

  if( nrec_sameid < nrecmax )then
    call echo(code%ret)
    return
  endif

  allocate(lst_cntrb(nrec_sameid))
  allocate(lst_iisub(nrec_sameid))
  allocate(lst_irec(nrec_sameid))
  allocate(arg(nrec_sameid))

  irec_sameid = 0
  do iisub = 1, lst_idx_same_id%nsub
    imsg = lst_idx_same_id%imsg(iisub)
    isub = lst_idx_same_id%isub(iisub)
    sub => pb%msg(imsg)%sub(isub)
    do irec = 1, sub%nrec
      rec => sub%rec(irec)
      if( .not. rec%is_valid .or. rec%is_selected ) cycle

      irec_sameid = irec_sameid + 1

      rec_valid => lst_rec_valid(rec%irec_valid)

      lst_iisub(irec_sameid) = iisub
      lst_irec(irec_sameid) = irec
      lst_cntrb(irec_sameid) = 0.d0
      do ilay_local = 1, rec_valid%nlay_local
        lay_all => lst_lay_all(rec_valid%ilay_all(ilay_local))

        rec_local => lay_all%rec_local(rec_valid%irec_local(ilay_local))
        if( rec_local%wgt < WGT_THRESH ) cycle

        lst_cntrb(irec_sameid) &
          = max(lst_cntrb(irec_sameid), &
                calc_cntrb(rec_local%wgt, lay_all%dnst_used, dnst_thresh))
      enddo  ! ilay_local/
    enddo  ! irec/
  enddo  ! iisub/

  call argsort(lst_cntrb, arg)
  call sort(lst_cntrb, arg)
  call sort(lst_iisub, arg)
  call sort(lst_irec, arg)

  do irec_sameid = nrec_sameid, nrecmax+1, -1
    iisub = lst_iisub(irec_sameid)
    irec = lst_irec(irec_sameid)
    imsg = lst_idx_same_id%imsg(iisub)
    isub = lst_idx_same_id%isub(iisub)
    pb%msg(imsg)%sub(isub)%rec(irec)%is_valid = .false.
  enddo

  deallocate(arg)
  deallocate(lst_cntrb)
  deallocate(lst_iisub)
  deallocate(lst_irec)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine limit_records_same_id
!===============================================================
!
!===============================================================
real(8) function calc_cntrb(wgt, dnst_used, dnst_thresh) result(ret)
  implicit none
  real(8), intent(in) :: wgt
  real(8), intent(in) :: dnst_used
  real(8), intent(in) :: dnst_thresh

  ret = max(0.d0, wgt / (dnst_used+1d-10) * (dnst_thresh-dnst_used))
  !ret = max(0.d0, wgt * (1.d0-dnst_used/dnst_thresh))
end function calc_cntrb
!===============================================================
!
!===============================================================
end module mod__dnst
