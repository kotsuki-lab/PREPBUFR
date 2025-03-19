module mod__wgt
  use lib_const
  use lib_array
  use lib_math
  use lib_log
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
  type(lay_all_)  , intent(inout), target :: lst_lay_all(:)
  type(rec_valid_), intent(inout), target :: lst_rec_valid(:)
  type(order_)    , intent(in)            :: order

  integer, allocatable :: lst_idx(:)
  real(8), allocatable :: lst_wgt(:)
  integer, allocatable :: arg(:)
  type(rec_), pointer :: rec
  type(rec_local_), pointer :: rec_local
  type(lay_all_), pointer :: lay_all
  integer :: nmax, n, i
  integer :: ilay_all
  integer :: irec_local

  call echo(code%bgn, 'select_records')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nmax = maxval(lst_lay_all(:)%nrec_local)
  allocate(lst_wgt(nmax))
  allocate(lst_idx(nmax))
  allocate(arg(nmax))

  pb%nrec_selected = 0

  do ilay_all = 1, nlay_all
    lay_all => lst_lay_all(ilay_all)

    n = lay_all%nrec_local
    if( n == 0 ) cycle

    do irec_local = 1, n
      lst_wgt(irec_local) = lay_all%rec_local(irec_local)%wgt
      lst_idx(irec_local) = irec_local
    enddo  ! irec_local/

    call argsort(lst_wgt(:n), arg(:n))
    call reverse(arg(:n))
    call sort(lst_wgt(:n), arg(:n))
    call sort(lst_idx(:n), arg(:n))

    do i = 1, min(n,order%nmax)
      if( lst_wgt(i) <= 0.d0 ) exit
      rec_local => lay_all%rec_local(lst_idx(i))
      rec => pb%msg(rec_local%imsg)%sub(rec_local%isub)%rec(rec_local%irec)
      if( .not. rec%is_valid )then
        call eerr('Unexpected condition'//&
                '\n  .not. rec%is_valid .and. wgt > 0.d0')
      endif
      if( .not. rec%is_selected )then
        rec%is_selected = .true.
        rec%ilay_all_belong = rec%ilay_all_nearest
        call add(pb%nrec_selected)
      endif
    enddo
  enddo  ! ilay_all/

  deallocate(lst_wgt)
  deallocate(lst_idx)
  deallocate(arg)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine select_records
!===============================================================
!
!===============================================================
end module mod__wgt
