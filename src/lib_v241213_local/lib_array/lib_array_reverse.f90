module lib_array_reverse
  use lib_log, only: &
    CODE, &
    str, &
    dgt, &
    echo, &
    edbg, &
    eerr
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: reverse
  public :: reversed
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  interface reverse
    module procedure reverse_char_0d
    module procedure reverse_int1_1d
    module procedure reverse_int1_2d
    module procedure reverse_int1_3d
    module procedure reverse_int2_1d
    module procedure reverse_int2_2d
    module procedure reverse_int2_3d
    module procedure reverse_int4_1d
    module procedure reverse_int4_2d
    module procedure reverse_int4_3d
    module procedure reverse_int8_1d
    module procedure reverse_int8_2d
    module procedure reverse_int8_3d
    module procedure reverse_real_1d
    module procedure reverse_real_2d
    module procedure reverse_real_3d
    module procedure reverse_dble_1d
    module procedure reverse_dble_2d
    module procedure reverse_dble_3d
  end interface

  interface reversed
    module procedure reversed_char_0d
    module procedure reversed_int1_1d
    module procedure reversed_int1_2d
    module procedure reversed_int1_3d
    module procedure reversed_int2_1d
    module procedure reversed_int2_2d
    module procedure reversed_int2_3d
    module procedure reversed_int4_1d
    module procedure reversed_int4_2d
    module procedure reversed_int4_3d
    module procedure reversed_int8_1d
    module procedure reversed_int8_2d
    module procedure reversed_int8_3d
    module procedure reversed_real_1d
    module procedure reversed_real_2d
    module procedure reversed_real_3d
    module procedure reversed_dble_1d
    module procedure reversed_dble_2d
    module procedure reversed_dble_3d
  end interface
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine reverse_char_0d(c)
  implicit none
  character(*), intent(inout) :: c

  c = reversed_char_0d(c)
end subroutine reverse_char_0d
!===============================================================
!
!===============================================================
subroutine reverse_int1_1d(arr)
  implicit none
  integer, parameter :: byte = 1
  integer(byte), intent(inout) :: arr(:)
  integer(byte) :: tmp(size(arr,kind=8))
  integer(8) :: i, imax

  imax = size(arr, kind=8)
  do i = 1_8, imax
    tmp(imax-i+1_8) = arr(i)
  enddo
  arr(:) = tmp(:)
end subroutine reverse_int1_1d
!===============================================================
!
!===============================================================
subroutine reverse_int1_2d(arr, d)
  implicit none
  integer, parameter :: byte = 1
  integer(byte), intent(inout) :: arr(:,:)
  integer      , intent(in)    :: d
  integer(byte), allocatable :: tmp(:)
  integer(8) :: imax, jmax, j

  call echo(CODE%BGN, 'reverse__MP__reverse_int'//str(byte)//'_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  imax = size(arr,1,kind=8)
  jmax = size(arr,2,kind=8)

  selectcase( d )
  case( 1 )
    do j = 1_8, jmax
      call reverse(arr(:,j))
    enddo
  case( 2 )
    allocate(tmp(imax))
    do j = 1_8, jmax/2
      tmp(:) = arr(:,j)
      arr(:,j) = arr(:,jmax-j+1)
      arr(:,jmax-j+1) = tmp(:)
    enddo
    deallocate(tmp)
  case default
    call eerr('Value is invalid. $d: '//str(d))
  endselect
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine reverse_int1_2d
!===============================================================
!
!===============================================================
subroutine reverse_int1_3d(arr, d)
  implicit none
  integer, parameter :: byte = 1
  integer(byte), intent(inout) :: arr(:,:,:)
  integer      , intent(in)    :: d
  integer(byte), allocatable :: tmp(:,:)
  integer(8) :: imax, jmax, kmax, j, k

  call echo(CODE%BGN, 'reverse__MP__reverse_int'//str(byte)//'_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  imax = size(arr,1,kind=8)
  jmax = size(arr,2,kind=8)
  kmax = size(arr,3,kind=8)

  selectcase( d )
  case( 1 )
    do k = 1_8, kmax
      do j = 1_8, jmax
        call reverse(arr(:,j,k))
      enddo
    enddo
  case( 2 )
    do k = 1_8, kmax
      call reverse(arr(:,:,k), 2)
    enddo
  case( 3 )
    allocate(tmp(imax,jmax))
    do k = 1_8, kmax/2
      tmp(:,:) = arr(:,:,k)
      arr(:,:,k) = arr(:,:,kmax-k+1)
      arr(:,:,kmax-k+1) = tmp(:,:)
    enddo
    deallocate(tmp)
  case default
    call eerr('Value is invalid. $d: '//str(d))
  endselect
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine reverse_int1_3d
!===============================================================
!
!===============================================================
subroutine reverse_int2_1d(arr)
  implicit none
  integer, parameter :: byte = 2
  integer(byte), intent(inout) :: arr(:)
  integer(byte), allocatable :: tmp(:)
  integer(8) :: i, imax

  imax = size(arr, kind=8)
  allocate(tmp(imax))
  do i = 1_8, imax
    tmp(imax-i+1_8) = arr(i)
  enddo
  arr(:) = tmp(:)
  deallocate(tmp)
end subroutine reverse_int2_1d
!===============================================================
!
!===============================================================
subroutine reverse_int2_2d(arr, d)
  implicit none
  integer, parameter :: byte = 2
  integer(byte), intent(inout) :: arr(:,:)
  integer      , intent(in)    :: d
  integer(byte), allocatable :: tmp(:)
  integer(8) :: imax, jmax, j

  call echo(CODE%BGN, 'reverse__MP__reverse_int'//str(byte)//'_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  imax = size(arr,1,kind=8)
  jmax = size(arr,2,kind=8)

  selectcase( d )
  case( 1 )
    do j = 1_8, jmax
      call reverse(arr(:,j))
    enddo
  case( 2 )
    allocate(tmp(imax))
    do j = 1_8, jmax/2
      tmp(:) = arr(:,j)
      arr(:,j) = arr(:,jmax-j+1)
      arr(:,jmax-j+1) = tmp(:)
    enddo
    deallocate(tmp)
  case default
    call eerr('Value is invalid. $d: '//str(d))
  endselect
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine reverse_int2_2d
!===============================================================
!
!===============================================================
subroutine reverse_int2_3d(arr, d)
  implicit none
  integer, parameter :: byte = 2
  integer(byte), intent(inout) :: arr(:,:,:)
  integer      , intent(in)    :: d
  integer(byte), allocatable :: tmp(:,:)
  integer(8) :: imax, jmax, kmax, j, k

  call echo(CODE%BGN, 'reverse__MP__reverse_int'//str(byte)//'_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  imax = size(arr,1,kind=8)
  jmax = size(arr,2,kind=8)
  kmax = size(arr,3,kind=8)

  selectcase( d )
  case( 1 )
    do k = 1_8, kmax
      do j = 1_8, jmax
        call reverse(arr(:,j,k))
      enddo
    enddo
  case( 2 )
    do k = 1_8, kmax
      call reverse(arr(:,:,k), 2)
    enddo
  case( 3 )
    allocate(tmp(imax,jmax))
    do k = 1_8, kmax/2
      tmp(:,:) = arr(:,:,k)
      arr(:,:,k) = arr(:,:,kmax-k+1)
      arr(:,:,kmax-k+1) = tmp(:,:)
    enddo
    deallocate(tmp)
  case default
    call eerr('Value is invalid. $d: '//str(d))
  endselect
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine reverse_int2_3d
!===============================================================
!
!===============================================================
subroutine reverse_int4_1d(arr)
  implicit none
  integer, parameter :: byte = 4
  integer(byte), intent(inout) :: arr(:)
  integer(byte) :: tmp(size(arr,kind=8))
  integer(8) :: i, imax

  imax = size(arr, kind=8)
  do i = 1_8, imax
    tmp(imax-i+1_8) = arr(i)
  enddo
  arr(:) = tmp(:)
end subroutine reverse_int4_1d
!===============================================================
!
!===============================================================
subroutine reverse_int4_2d(arr, d)
  implicit none
  integer, parameter :: byte = 4
  integer(byte), intent(inout) :: arr(:,:)
  integer      , intent(in)    :: d
  integer(byte), allocatable :: tmp(:)
  integer(8) :: imax, jmax, j

  call echo(CODE%BGN, 'reverse__MP__reverse_int'//str(byte)//'_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  imax = size(arr,1,kind=8)
  jmax = size(arr,2,kind=8)

  selectcase( d )
  case( 1 )
    do j = 1_8, jmax
      call reverse(arr(:,j))
    enddo
  case( 2 )
    allocate(tmp(imax))
    do j = 1_8, jmax/2
      tmp(:) = arr(:,j)
      arr(:,j) = arr(:,jmax-j+1)
      arr(:,jmax-j+1) = tmp(:)
    enddo
    deallocate(tmp)
  case default
    call eerr('Value is invalid. $d: '//str(d))
  endselect
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine reverse_int4_2d
!===============================================================
!
!===============================================================
subroutine reverse_int4_3d(arr, d)
  implicit none
  integer, parameter :: byte = 4
  integer(byte), intent(inout) :: arr(:,:,:)
  integer      , intent(in)    :: d
  integer(byte), allocatable :: tmp(:,:)
  integer(8) :: imax, jmax, kmax, j, k

  call echo(CODE%BGN, 'reverse__MP__reverse_int'//str(byte)//'_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  imax = size(arr,1,kind=8)
  jmax = size(arr,2,kind=8)
  kmax = size(arr,3,kind=8)

  selectcase( d )
  case( 1 )
    do k = 1_8, kmax
      do j = 1_8, jmax
        call reverse(arr(:,j,k))
      enddo
    enddo
  case( 2 )
    do k = 1_8, kmax
      call reverse(arr(:,:,k), 2)
    enddo
  case( 3 )
    allocate(tmp(imax,jmax))
    do k = 1_8, kmax/2
      tmp(:,:) = arr(:,:,k)
      arr(:,:,k) = arr(:,:,kmax-k+1)
      arr(:,:,kmax-k+1) = tmp(:,:)
    enddo
    deallocate(tmp)
  case default
    call eerr('Value is invalid. $d: '//str(d))
  endselect
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine reverse_int4_3d
!===============================================================
!
!===============================================================
subroutine reverse_int8_1d(arr)
  implicit none
  integer, parameter :: byte = 8
  integer(byte), intent(inout) :: arr(:)
  integer(byte), allocatable :: tmp(:)
  integer(8) :: i, imax

  imax = size(arr, kind=8)
  allocate(tmp(size(arr,kind=8)))
  do i = 1_8, imax
    tmp(imax-i+1_8) = arr(i)
  enddo
  arr(:) = tmp(:)
  deallocate(tmp)
end subroutine reverse_int8_1d
!===============================================================
!
!===============================================================
subroutine reverse_int8_2d(arr, d)
  implicit none
  integer, parameter :: byte = 8
  integer(byte), intent(inout) :: arr(:,:)
  integer      , intent(in)    :: d
  integer(byte), allocatable :: tmp(:)
  integer(8) :: imax, jmax, j

  call echo(CODE%BGN, 'reverse__MP__reverse_int'//str(byte)//'_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  imax = size(arr,1,kind=8)
  jmax = size(arr,2,kind=8)

  selectcase( d )
  case( 1 )
    do j = 1_8, jmax
      call reverse(arr(:,j))
    enddo
  case( 2 )
    allocate(tmp(imax))
    do j = 1_8, jmax/2
      tmp(:) = arr(:,j)
      arr(:,j) = arr(:,jmax-j+1)
      arr(:,jmax-j+1) = tmp(:)
    enddo
    deallocate(tmp)
  case default
    call eerr('Value is invalid. $d: '//str(d))
  endselect
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine reverse_int8_2d
!===============================================================
!
!===============================================================
subroutine reverse_int8_3d(arr, d)
  implicit none
  integer, parameter :: byte = 8
  integer(byte), intent(inout) :: arr(:,:,:)
  integer      , intent(in)    :: d
  integer(byte), allocatable :: tmp(:,:)
  integer(8) :: imax, jmax, kmax, j, k

  call echo(CODE%BGN, 'reverse__MP__reverse_int'//str(byte)//'_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  imax = size(arr,1,kind=8)
  jmax = size(arr,2,kind=8)
  kmax = size(arr,3,kind=8)

  selectcase( d )
  case( 1 )
    do k = 1_8, kmax
      do j = 1_8, jmax
        call reverse(arr(:,j,k))
      enddo
    enddo
  case( 2 )
    do k = 1_8, kmax
      call reverse(arr(:,:,k), 2)
    enddo
  case( 3 )
    allocate(tmp(imax,jmax))
    do k = 1_8, kmax/2
      tmp(:,:) = arr(:,:,k)
      arr(:,:,k) = arr(:,:,kmax-k+1)
      arr(:,:,kmax-k+1) = tmp(:,:)
    enddo
    deallocate(tmp)
  case default
    call eerr('Value is invalid. $d: '//str(d))
  endselect
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine reverse_int8_3d
!===============================================================
!
!===============================================================
subroutine reverse_real_1d(arr)
  implicit none
  integer, parameter :: byte = 4
  real(byte), intent(inout) :: arr(:)
  real(byte), allocatable :: tmp(:)
  integer(8) :: i, imax

  imax = size(arr, kind=8)
  allocate(tmp(size(arr,kind=8)))
  do i = 1_8, imax
    tmp(imax-i+1_8) = arr(i)
  enddo
  arr(:) = tmp(:)
  deallocate(tmp)
end subroutine reverse_real_1d
!===============================================================
!
!===============================================================
subroutine reverse_real_2d(arr, d)
  implicit none
  integer, parameter :: byte = 4
  real(byte), intent(inout) :: arr(:,:)
  integer   , intent(in)    :: d
  real(byte), allocatable :: tmp(:)
  integer(8) :: imax, jmax, j

  call echo(CODE%BGN, 'reverse__MP__reverse_real_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  imax = size(arr,1,kind=8)
  jmax = size(arr,2,kind=8)

  selectcase( d )
  case( 1 )
    do j = 1_8, jmax
      call reverse(arr(:,j))
    enddo
  case( 2 )
    allocate(tmp(imax))
    do j = 1_8, jmax/2
      tmp(:) = arr(:,j)
      arr(:,j) = arr(:,jmax-j+1)
      arr(:,jmax-j+1) = tmp(:)
    enddo
    deallocate(tmp)
  case default
    call eerr('Value is invalid. $d: '//str(d))
  endselect
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine reverse_real_2d
!===============================================================
!
!===============================================================
subroutine reverse_real_3d(arr, d)
  implicit none
  integer, parameter :: byte = 4
  real(byte), intent(inout) :: arr(:,:,:)
  integer      , intent(in)    :: d
  real(byte), allocatable :: tmp(:,:)
  integer(8) :: imax, jmax, kmax, j, k

  call echo(CODE%BGN, 'reverse__MP__reverse_real_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  imax = size(arr,1,kind=8)
  jmax = size(arr,2,kind=8)
  kmax = size(arr,3,kind=8)

  selectcase( d )
  case( 1 )
    do k = 1_8, kmax
      do j = 1_8, jmax
        call reverse(arr(:,j,k))
      enddo
    enddo
  case( 2 )
    do k = 1_8, kmax
      call reverse(arr(:,:,k), 2)
    enddo
  case( 3 )
    allocate(tmp(imax,jmax))
    do k = 1_8, kmax/2
      tmp(:,:) = arr(:,:,k)
      arr(:,:,k) = arr(:,:,kmax-k+1)
      arr(:,:,kmax-k+1) = tmp(:,:)
    enddo
    deallocate(tmp)
  case default
    call eerr('Value is invalid. $d: '//str(d))
  endselect
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine reverse_real_3d
!===============================================================
!
!===============================================================
subroutine reverse_dble_1d(arr)
  implicit none
  integer, parameter :: byte = 8
  real(byte), intent(inout) :: arr(:)
  real(byte) :: tmp(size(arr,kind=8))
  integer(8) :: i, imax

  imax = size(arr, kind=8)
  do i = 1_8, imax
    tmp(imax-i+1_8) = arr(i)
  enddo
  arr(:) = tmp(:)
end subroutine reverse_dble_1d
!===============================================================
!
!===============================================================
subroutine reverse_dble_2d(arr, d)
  implicit none
  integer, parameter :: byte = 8
  real(byte), intent(inout) :: arr(:,:)
  integer   , intent(in)    :: d
  real(byte), allocatable :: tmp(:)
  integer(8) :: imax, jmax, j

  call echo(CODE%BGN, 'reverse__MP__reverse_dble_2d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  imax = size(arr,1,kind=8)
  jmax = size(arr,2,kind=8)

  selectcase( d )
  case( 1 )
    do j = 1_8, jmax
      call reverse(arr(:,j))
    enddo
  case( 2 )
    allocate(tmp(imax))
    do j = 1_8, jmax/2
      tmp(:) = arr(:,j)
      arr(:,j) = arr(:,jmax-j+1)
      arr(:,jmax-j+1) = tmp(:)
    enddo
    deallocate(tmp)
  case default
    call eerr('Value is invalid. $d: '//str(d))
  endselect
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine reverse_dble_2d
!===============================================================
!
!===============================================================
subroutine reverse_dble_3d(arr, d)
  implicit none
  integer, parameter :: byte = 8
  real(byte), intent(inout) :: arr(:,:,:)
  integer   , intent(in)    :: d
  real(byte), allocatable :: tmp(:,:)
  integer(8) :: imax, jmax, kmax, j, k

  call echo(CODE%BGN, 'reverse__MP__reverse_dble_3d', '-p')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  imax = size(arr,1,kind=8)
  jmax = size(arr,2,kind=8)
  kmax = size(arr,3,kind=8)

  selectcase( d )
  case( 1 )
    do k = 1_8, kmax
      do j = 1_8, jmax
        call reverse(arr(:,j,k))
      enddo
    enddo
  case( 2 )
    do k = 1_8, kmax
      call reverse(arr(:,:,k), 2)
    enddo
  case( 3 )
    allocate(tmp(imax,jmax))
    do k = 1_8, kmax
      tmp(:,:) = arr(:,:,k)
      arr(:,:,k) = arr(:,:,kmax-k+1)
      arr(:,:,kmax-k+1) = tmp(:,:)
    enddo
    deallocate(tmp)
  case default
    call eerr('Value is invalid. $d: '//str(d))
  endselect
  !-------------------------------------------------------------
  call echo(CODE%RET)
end subroutine reverse_dble_3d
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
pure function reversed_char_0d(ic) result(oc)
  implicit none
  character(*), intent(in) :: ic
  character(len(ic))       :: oc
  integer(8) :: imax, i

  imax = len(ic)
  do i = 1_8, imax
    oc(i:i) = ic(imax-i+1_8:imax-i+1_8)
  enddo
end function reversed_char_0d
!===============================================================
!
!===============================================================
function reversed_int1_1d(iarr) result(oarr)
  implicit none
  integer(1), intent(in) :: iarr(:)
  integer(1)             :: oarr(size(iarr,kind=8))

  oarr(:) = iarr(:)
  call reverse(oarr)
end function reversed_int1_1d
!===============================================================
!
!===============================================================
function reversed_int1_2d(iarr, d) result(oarr)
  implicit none
  integer(1), intent(in) :: iarr(:,:)
  integer   , intent(in) :: d
  integer(1)             :: oarr(size(iarr,1,kind=8),size(iarr,2,kind=8))

  oarr(:,:) = iarr(:,:)
  call reverse(oarr, d)
end function reversed_int1_2d
!===============================================================
!
!===============================================================
function reversed_int1_3d(iarr, d) result(oarr)
  implicit none
  integer(1), intent(in) :: iarr(:,:,:)
  integer   , intent(in) :: d
  integer(1)             :: oarr(size(iarr,1,kind=8),size(iarr,2,kind=8),size(iarr,3,kind=8))

  oarr(:,:,:) = iarr(:,:,:)
  call reverse(oarr, d)
end function reversed_int1_3d
!===============================================================
!
!===============================================================
function reversed_int2_1d(iarr) result(oarr)
  implicit none
  integer(2), intent(in) :: iarr(:)
  integer(2)             :: oarr(size(iarr,kind=8))

  oarr(:) = iarr(:)
  call reverse(oarr)
end function reversed_int2_1d
!===============================================================
!
!===============================================================
function reversed_int2_2d(iarr, d) result(oarr)
  implicit none
  integer(2), intent(in) :: iarr(:,:)
  integer   , intent(in) :: d
  integer(2)             :: oarr(size(iarr,1,kind=8),size(iarr,2,kind=8))

  oarr(:,:) = iarr(:,:)
  call reverse(oarr, d)
end function reversed_int2_2d
!===============================================================
!
!===============================================================
function reversed_int2_3d(iarr, d) result(oarr)
  implicit none
  integer(2), intent(in) :: iarr(:,:,:)
  integer   , intent(in) :: d
  integer(2)             :: oarr(size(iarr,1,kind=8),size(iarr,2,kind=8),size(iarr,3,kind=8))

  oarr(:,:,:) = iarr(:,:,:)
  call reverse(oarr, d)
end function reversed_int2_3d
!===============================================================
!
!===============================================================
function reversed_int4_1d(iarr) result(oarr)
  implicit none
  integer(4), intent(in) :: iarr(:)
  integer(4)             :: oarr(size(iarr,kind=8))

  oarr(:) = iarr(:)
  call reverse(oarr)
end function reversed_int4_1d
!===============================================================
!
!===============================================================
function reversed_int4_2d(iarr, d) result(oarr)
  implicit none
  integer(4), intent(in) :: iarr(:,:)
  integer   , intent(in) :: d
  integer(4)             :: oarr(size(iarr,1,kind=8),size(iarr,2,kind=8))

  oarr(:,:) = iarr(:,:)
  call reverse(oarr, d)
end function reversed_int4_2d
!===============================================================
!
!===============================================================
function reversed_int4_3d(iarr, d) result(oarr)
  implicit none
  integer(4), intent(in) :: iarr(:,:,:)
  integer   , intent(in) :: d
  integer(4)             :: oarr(size(iarr,1,kind=8),size(iarr,2,kind=8),size(iarr,3,kind=8))

  oarr(:,:,:) = iarr(:,:,:)
  call reverse(oarr, d)
end function reversed_int4_3d
!===============================================================
!
!===============================================================
function reversed_int8_1d(iarr) result(oarr)
  implicit none
  integer(8), intent(in) :: iarr(:)
  integer(8)             :: oarr(size(iarr,kind=8))

  oarr(:) = iarr(:)
  call reverse(oarr)
end function reversed_int8_1d
!===============================================================
!
!===============================================================
function reversed_int8_2d(iarr, d) result(oarr)
  implicit none
  integer(8), intent(in) :: iarr(:,:)
  integer   , intent(in) :: d
  integer(8)             :: oarr(size(iarr,1,kind=8),size(iarr,2,kind=8))

  oarr(:,:) = iarr(:,:)
  call reverse(oarr, d)
end function reversed_int8_2d
!===============================================================
!
!===============================================================
function reversed_int8_3d(iarr, d) result(oarr)
  implicit none
  integer(8), intent(in) :: iarr(:,:,:)
  integer   , intent(in) :: d
  integer(8)             :: oarr(size(iarr,1,kind=8),size(iarr,2,kind=8),size(iarr,3,kind=8))

  oarr(:,:,:) = iarr(:,:,:)
  call reverse(oarr, d)
end function reversed_int8_3d
!===============================================================
!
!===============================================================
function reversed_real_1d(iarr) result(oarr)
  implicit none
  real(4), intent(in) :: iarr(:)
  real(4)             :: oarr(size(iarr,kind=8))

  oarr(:) = iarr(:)
  call reverse(oarr)
end function reversed_real_1d
!===============================================================
!
!===============================================================
function reversed_real_2d(iarr, d) result(oarr)
  implicit none
  real(4), intent(in) :: iarr(:,:)
  integer, intent(in) :: d
  real(4)             :: oarr(size(iarr,1,kind=8),size(iarr,2,kind=8))

  oarr(:,:) = iarr(:,:)
  call reverse(oarr, d)
end function reversed_real_2d
!===============================================================
!
!===============================================================
function reversed_real_3d(iarr, d) result(oarr)
  implicit none
  real(4), intent(in) :: iarr(:,:,:)
  integer, intent(in) :: d
  real(4)             :: oarr(size(iarr,1,kind=8),size(iarr,2,kind=8),size(iarr,3,kind=8))

  oarr(:,:,:) = iarr(:,:,:)
  call reverse(oarr, d)
end function reversed_real_3d
!===============================================================
!
!===============================================================
function reversed_dble_1d(iarr) result(oarr)
  implicit none
  real(8), intent(in) :: iarr(:)
  real(8)             :: oarr(size(iarr,kind=8))

  oarr(:) = iarr(:)
  call reverse(oarr)
end function reversed_dble_1d
!===============================================================
!
!===============================================================
function reversed_dble_2d(iarr, d) result(oarr)
  implicit none
  real(8), intent(in) :: iarr(:,:)
  integer, intent(in) :: d
  real(8)             :: oarr(size(iarr,1,kind=8),size(iarr,2,kind=8))

  oarr(:,:) = iarr(:,:)
  call reverse(oarr, d)
end function reversed_dble_2d
!===============================================================
!
!===============================================================
function reversed_dble_3d(iarr, d) result(oarr)
  implicit none
  real(8), intent(in) :: iarr(:,:,:)
  integer, intent(in) :: d
  real(8)             :: oarr(size(iarr,1,kind=8),size(iarr,2,kind=8),size(iarr,3,kind=8))

  oarr(:,:,:) = iarr(:,:,:)
  call reverse(oarr, d)
end function reversed_dble_3d
!===============================================================
!
!===============================================================
end module lib_array_reverse
