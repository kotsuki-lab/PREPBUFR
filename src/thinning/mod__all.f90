module mod__all
  use lib_log
  use lib_array
  use lib_math
  use mod_share
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: select_records
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine select_records(&
    pb, lst_lay_all, lst_rec_valid, order)
  implicit none
  type(pb_)       , intent(inout), target :: pb
  type(lay_all_)  , intent(inout)         :: lst_lay_all(:)
  type(rec_valid_), intent(inout)         :: lst_rec_valid(:)
  type(order_)    , intent(in)            :: order

  type(msg_), pointer :: msg
  type(sub_), pointer :: sub
  type(rec_), pointer :: rec
  integer :: imsg, isub, irec

  call echo(code%bgn, 'select_records')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( order%code_same_id )
  case( CODE_SAME_ID__SID_REDUCE, &
        CODE_SAME_ID__SAID_REDUCE )
    call reduce_records_same_id(order, pb, lst_lay_all, lst_rec_valid)
  case( CODE_SAME_ID__SID_LIMIT, &
        CODE_SAME_ID__SAID_LIMIT )
    !call limit_records_same_id(pb, order)
  endselect

  pb%nrec_selected = 0

  do imsg = 1, pb%nmsg
    msg => pb%msg(imsg)
    do isub = 1, msg%nsub
      sub => msg%sub(isub)
      do irec = 1, sub%nrec
        rec => sub%rec(irec)

        rec%is_selected = rec%is_valid
        rec%ilay_all_belong = rec%ilay_all_nearest

        if( rec%is_selected )then
          call add(pb%nrec_selected)
        endif
      enddo  ! irec/
    enddo  ! isub/
  enddo  ! imsg/
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine select_records
!===============================================================
!
!===============================================================
subroutine reduce_records_same_id(&
    order, pb, lst_lay_all, lst_rec_valid)
  use mod_common, only: &
        pnt_lst_idx_same_id
  implicit none
  type(order_)    , intent(in) :: order
  type(pb_)       , intent(inout), target :: pb
  type(lay_all_)  , intent(inout), target :: lst_lay_all(:)
  type(rec_valid_), intent(inout), target :: lst_rec_valid(:)

  type(rec_valid_)       , pointer :: rec_valid
  type(rec_local_)       , pointer :: rec_local
  type(lst_idx_same_id_), pointer :: lst_idx_same_id(:)
  type(lst_idx_same_id_), pointer :: lst
  type(sub_), pointer :: sub
  type(rec_), pointer :: rec
  integer :: imsg, isub, irec
  integer :: irec_valid
  integer :: ilay_all
  integer :: ilay_local
  integer :: irec_local
  integer :: ilst
  integer :: iisub
  integer :: is, ie, i
  integer :: nrec
  integer :: nrec_wgt_max
  real(8) :: wgt_max
  real(8) :: wgt
  real(8) :: wgt_same_id
  real(8) :: xwgt
  real(8), allocatable :: lst_rec_valid_wgt(:)
  integer, pointer :: lst_iisub(:)
  integer, pointer :: lst_irec(:)
  real(8), pointer :: lst_wgt(:)
  integer, pointer :: arg(:)

  call echo(code%bgn, 'reduce_records_same_id', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nullify(lst_idx_same_id)
  call pnt_lst_idx_same_id(lst_idx_same_id, order%code_same_id)

  selectcase( order%code_same_id )
  case( CODE_SAME_ID__SID_REDUCE, &
        CODE_SAME_ID__SID_LIMIT )
    wgt_same_id = order%wgt_same_sid
  case( CODE_SAME_ID__SAID_REDUCE, &
        CODE_SAME_ID__SAID_LIMIT )
    wgt_same_id = order%wgt_same_said
  case default
    call eerr('Invalid value in $order%code_same_id: '//str(order%code_same_id))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(lst_rec_valid_wgt(pb%nrec_valid))
  lst_rec_valid_wgt(:) = -1d20

  do irec_valid = 1, pb%nrec_valid
    rec_valid => lst_rec_valid(irec_valid)
    do ilay_local = 1, rec_valid%nlay_local
      ilay_all = rec_valid%ilay_all(ilay_local)
      irec_local = rec_valid%irec_local(ilay_local)
      rec_local => lst_lay_all(ilay_all)%rec_local(irec_local)
      lst_rec_valid_wgt(irec_valid) = &
        max(lst_rec_valid_wgt(irec_valid), rec_local%wgt)
    enddo
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(lst_iisub(1024))
  allocate(lst_irec(1024))
  allocate(lst_wgt(1024))
  allocate(arg(1024))

  do ilst = 1, size(lst_idx_same_id)
    lst => lst_idx_same_id(ilst)
    !-------------------------------------------------------------
    ! Get max weight
    !-------------------------------------------------------------
    wgt_max = 0.d0
    nrec = 0
    do iisub = 1, lst%nsub
      imsg = lst%imsg(iisub)
      isub = lst%isub(iisub)
      sub => pb%msg(imsg)%sub(isub)
      do irec = 1, sub%nrec
        rec => sub%rec(irec)
        wgt = lst_rec_valid_wgt(rec%irec_valid)

        if( wgt > wgt_max )then
          nrec_wgt_max = 1
          wgt_max = wgt
        elseif( wgt == wgt_max )then
          call add(nrec_wgt_max)
        endif

        if( nrec > size(lst_iisub) )then
          call realloc(lst_iisub, nrec*2, clear=.false.)
          call realloc(lst_irec , nrec*2, clear=.false.)
          call realloc(lst_wgt  , nrec*2, clear=.false.)
          call realloc(arg      , nrec*2, clear=.true.)
        endif
        call add(nrec)
        lst_iisub(nrec) = iisub
        lst_irec(nrec) = irec
        lst_wgt(nrec) = wgt
      enddo  ! irec/
    enddo  ! iisub/
    !-------------------------------------------------------------
    ! Modify weights of records that have same id
    !-------------------------------------------------------------
    if( wgt_max == 0.d0 )then
      do iisub = 1, lst%nsub
        imsg = lst%imsg(iisub)
        isub = lst%isub(iisub)
        sub => pb%msg(imsg)%sub(isub)
        sub%rec(:)%is_selected = .false.
      enddo  ! iisub/
    else
      call argsort(lst_iisub(:nrec), arg(:nrec))
      call sort(lst_iisub(:nrec), arg(:nrec))
      call sort(lst_irec(:nrec), arg(:nrec))
      call sort(lst_wgt(:nrec), arg(:nrec))

      xwgt = wgt_same_id
      ie = 0
      do while( ie < nrec )
        is = ie + 1
        ie = is
        do while( ie < nrec )
          if( lst_wgt(ie+1) /= lst_wgt(is) ) exit
          ie = ie + 1
        enddo

        do i = is, ie
          iisub = lst_iisub(i)
          irec = lst_irec(i)
          rec => pb%msg(lst%imsg(iisub))%sub(lst%isub(iisub))%rec(irec)
          do ilay_local = 1, rec_valid%nlay_local
            ilay_all = rec_valid%ilay_all(ilay_local)
            irec_local = rec_valid%irec_local(ilay_local)
            rec_local => lst_lay_all(ilay_all)%rec_local(irec_local)
            call mul(rec_local%wgt, xwgt/(ie-is+1))
          enddo
        enddo

        xwgt = xwgt * wgt_same_id**(ie-is+1)
      enddo

    endif
    !-------------------------------------------------------------
  enddo  ! ilst/

  deallocate(lst_iisub)
  deallocate(lst_irec)
  deallocate(lst_wgt)
  deallocate(arg)

  nullify(lst_idx_same_id)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine reduce_records_same_id
!===============================================================
!
!===============================================================
end module mod__all
