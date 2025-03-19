module lib_math_interp
  use lib_const
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  ! Procedures
  !-------------------------------------------------------------
  public :: interpolate
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface interpolate
    module procedure interpolate_real_2d
  end interface interpolate

  interface interpolate_linear
    module procedure interpolate_linear_real_2d
  end interface interpolate_linear
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine interpolate_real_2d(s, t, method)
  real(4)     , intent(in)  :: s(:,:)
  real(4)     , intent(out) :: t(:,:)
  character(*), intent(in)  :: method

  call echo(CODE%BGN, 'interpolate__mp__interpolate_real_2d', '-p')
  !-------------------------------------------------------------
  if( all(shape(s) == shape(t)) )then
    t(:,:) = s(:,:)
  else
    selectcase( method )
    case(  INTERP_METHOD_LINEAR )
      call interpolate_linear(s, t)
    case default
      call eerr("Invalid interpolation method '"//str(method)//"' was specified.")
    endselect
  endif
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine interpolate_real_2d
!===============================================================
!
!===============================================================
subroutine interpolate_linear_real_2d(s, t)
  implicit none
  integer, parameter :: byte = 4
  real(byte), intent(in)  :: s(:,:)
  real(byte), intent(out) :: t(:,:)
  integer :: nsx, nsy
  integer :: ntx, nty, itx, ity
  integer, allocatable :: sxi(:), sxf(:), syi(:), syf(:)
  real(8), allocatable :: wsxi(:), wsxf(:), wsyi(:), wsyf(:)
  real(8) :: ws
  integer :: sx0, sx1
  integer :: sy0, sy1
  real(8) :: wsx0, wsx1, wsy0, wsy1
  integer :: sxnum_in, synum_in

  call echo(CODE%BGN, 'interpolate_linear_real_2d', '-p')
  !-------------------------------------------------------------
  nsx = size(s,1)
  nsy = size(s,2)

  ntx = size(t,1)
  nty = size(t,2)

  allocate(sxi(ntx), &
           sxf(ntx), &
           syi(nty), &
           syf(nty))
  allocate(wsxi(ntx), &
           wsxf(ntx), &
           wsyi(nty), &
           wsyf(nty))

  call getRelationOfGridLines(nsx, ntx, sxi, sxf, wsxi, wsxf)
  call getRelationOfGridLines(nsy, nty, syi, syf, wsyi, wsyf)
  ws = (nsx*nsy)/real(ntx*nty)

  do ity = 1, nty
    sy0 = syi(ity)
    sy1 = syf(ity)
    synum_in = sy1 - sy0 - 1
    wsy0 = wsyi(ity)
    wsy1 = wsyf(ity)

    do itx = 1, ntx
      sx0 = sxi(itx)
      sx1 = sxf(itx)
      sxnum_in = sx1 - sx0 - 1
      wsx0 = wsxi(itx)
      wsx1 = wsxf(itx)

      ! Edges
      t(itx,ity) = real(s(sx0,sy0) * wsx0 * wsy0 &
                          + s(sx0,sy1) * wsx0 * wsy1 &
                          + s(sx1,sy0) * wsx1 * wsy0 &
                          + s(sx1,sy1) * wsx1 * wsy1, kind=byte)

      if( sxnum_in > 0 )then
        ! Lower side
        t(itx,ity) = t(itx,ity) + real(sum(s(sx0+1:sx1-1,sy0))/sxnum_in * wsy0, kind=byte)
        ! Upper side
        t(itx,ity) = t(itx,ity) + real(sum(s(sx0+1:sx1-1,sy1))/sxnum_in * wsy1, kind=byte)
      endif

      if( synum_in > 0 )then
        ! Left side
        t(itx,ity) = t(itx,ity) + real(sum(s(sx0,sy0+1:sy1-1))/synum_in * wsx0, kind=byte)
        ! Right side
        t(itx,ity) = t(itx,ity) + real(sum(s(sx1,sy0+1:sy1-1))/synum_in * wsx1, kind=byte)
      endif

      ! Inside
      t(itx,ity) = t(itx,ity) + real(sum(s(sx0+1:sx1-1,sy0+1:sy1-1)) * ws, kind=byte)

    enddo
  enddo
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine interpolate_linear_real_2d
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
subroutine getRelationOfGridLines(isize_row, isize_reg, irow0, irow1, coef0, coef1)
  implicit none
  integer, intent(in)  :: isize_row  ! Resolution of row data
  integer, intent(in)  :: isize_reg  !               regridded data
  integer, intent(out) :: irow0(:), irow1(:)  !(isize_reg)
  real(8), intent(out) :: coef0(:), coef1(:)  !(isize_reg)
  integer :: i_reg

  do i_reg = 1, isize_reg
    irow0(i_reg) = max(int(isize_row * real(i_reg-1)/isize_reg)+1, 1)
    irow1(i_reg) = min(int(isize_row * real(i_reg  )/isize_reg)+1, isize_row)
    if( irow0(i_reg) == irow1(i_reg) )then
      coef0(i_reg) = 1.d0
      coef1(i_reg) = 0.d0
    else
      coef0(i_reg) = real(irow0(i_reg))/isize_row*isize_reg - real(i_reg-1)
      coef1(i_reg) = real(i_reg) - real(irow1(i_reg)-1)/isize_row*isize_reg
    endif
  enddo
end subroutine getRelationOfGridLines
!===============================================================
!
!===============================================================
end module lib_math_interp
