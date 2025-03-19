module lib_log_array
  use lib_const
  use lib_log_proc
  use lib_log_str
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: showarr
  public :: showmat
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface showarr
    module procedure showarr__int1_1d
    module procedure showarr__int1_2d
    module procedure showarr__int1_3d
    module procedure showarr__int2_1d
    module procedure showarr__int2_2d
    module procedure showarr__int2_3d
    module procedure showarr__int4_1d
    module procedure showarr__int4_2d
    module procedure showarr__int4_3d
    module procedure showarr__int8_1d
    module procedure showarr__int8_2d
    module procedure showarr__int8_3d
    module procedure showarr__real_1d
    module procedure showarr__real_2d
    module procedure showarr__real_3d
    module procedure showarr__dble_1d
    module procedure showarr__dble_2d
    module procedure showarr__dble_3d
  end interface

  interface showmat
    module procedure showmat__int4
    module procedure showmat__dble
  end interface
  !-------------------------------------------------------------
  ! Module Variables
  !-------------------------------------------------------------
  integer, parameter :: NSHOW_DEFAULT = 3
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine showarr__int1_1d(x, nam, d, n, c_l, c_r)
  implicit none
  integer(1)  , intent(in) :: x(:)
  character(*), intent(in), optional :: nam
  integer     , intent(in), optional :: d  ! digit
  integer     , intent(in), optional :: n  ! num. of components to show
  character(*), intent(in), optional :: c_l, c_r

  integer :: d_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_

  integer :: n1

  n_ = NSHOW_DEFAULT
  if( present(n) ) n_ = n

  allocate(character(1) :: c_l_)
  allocate(character(1) :: c_r_)
  c_l_ = '['
  c_r_ = ']'
  if( present(c_l) ) c_l_ = c_l
  if( present(c_r) ) c_r_ = c_r

  n1 = size(x)

  if( present(d) )then
    d_ = d
  else
    if( n_*2+1 >= n1 )then
      d_ = dgt(x,dgt_opt_max)
    else
      d_ = max(dgt(x(:n_),dgt_opt_max),dgt(x(n1-n_+1:),dgt_opt_max))
    endif
  endif

  if( present(nam) )then
    call show_basic_info(nam, shape(x), str(minval(x),d_), str(maxval(x),d_))
  endif

  if( n_*2+1 >= n1 )then
    call edbg(c_l_//str(x,d_)//c_r_)
  else
    call edbg(c_l_//str(x(:n_),d_)//' ... '//str(x(n1-n_+1:),d_)//c_r_)
  endif
end subroutine showarr__int1_1d
!===============================================================
!
!===============================================================
subroutine showarr__int1_2d(x, nam, d, n, c_l, c_r, ismtx)
  implicit none
  integer(1)  , intent(in) :: x(:,:)
  character(*), intent(in), optional :: nam
  integer     , intent(in), optional :: d
  integer     , intent(in), optional :: n
  character(*), intent(in), optional :: c_l, c_r
  logical     , intent(in), optional :: ismtx

  integer :: d_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_
  logical :: ismtx_

  integer :: clen_l, clen_r
  character(:), allocatable :: c_l_tmp, c_r_tmp
  integer :: n1, n2
  integer :: i2
  integer :: i1s(2), i1e(2), i2s(2), i2e(2)
  integer :: ib1, ib2

  n_ = NSHOW_DEFAULT
  ismtx_ = .false.
  if( present(n) ) n_ = n
  if( present(ismtx) ) ismtx_ = ismtx

  clen_l = 2
  clen_r = 2
  if( present(c_l) ) clen_l = len(c_l)
  if( present(c_r) ) clen_r = len(c_r)
  allocate(character(clen_l) :: c_l_)
  allocate(character(clen_r) :: c_r_)
  allocate(character(clen_l) :: c_l_tmp)
  allocate(character(clen_r) :: c_r_tmp)
  if( present(c_l) ) c_l_tmp = c_l
  if( present(c_r) ) c_r_tmp = c_r

  n1 = size(x,1)
  n2 = size(x,2)

  if( present(d) )then
    d_ = d
  else
    i1s(1) = 1; i1e(1) = 1; i1s(2) = 1; i1e(2) = n1
    i2s(1) = 1; i2e(1) = 1; i2s(2) = 1; i2e(2) = n2
    if( n_*2+1 < n1 )then
      i1e(1) = n_
      i1s(2) = n1 - n_ + 1
    endif
    if( n_*2+1 < n2 )then
      i2e(1) = n_
      i2s(2) = n2 - n_ + 1
    endif

    d_ = 0
    do ib2 = 1, 2
    do ib1 = 1, 2
      d_ = max(d_, dgt(x(i1s(ib1):i1e(ib1),i2s(ib2):i2e(ib2)),dgt_opt_max))
    enddo
    enddo
  endif

  if( present(nam) )then
    if( ismtx_ )then
      call show_basic_info(&
             nam, (/size(x,2),size(x,1)/), str(minval(x),d_), str(maxval(x),d_))
    else
      call show_basic_info(&
             nam, shape(x), str(minval(x),d_), str(maxval(x),d_))
    endif
  endif

  if( n_*2+1 >= n2 )then
    do i2 = 1, n2
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__int1_1d(x(:,i2), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  else
    do i2 = 1, n_
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__int1_1d(x(:,i2), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
    if( ismtx_ )then
      call edbg(get_str_omitted_row__2d(abs(d_), n_, n1))
    else
      call edbg(str('',clen_l-1)//'...')
    endif
    do i2 = n2-n_+1, n2
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__int1_1d(x(:,i2), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  endif
end subroutine showarr__int1_2d
!===============================================================
!
!===============================================================
subroutine showarr__int1_3d(x, nam, d, n, c_l, c_r, ismtx)
  implicit none
  integer(1)  , intent(in) :: x(:,:,:)
  character(*), intent(in), optional :: nam
  integer     , intent(in), optional :: d
  integer     , intent(in), optional :: n
  character(*), intent(in), optional :: c_l, c_r
  logical     , intent(in), optional :: ismtx

  integer :: d_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_
  logical :: ismtx_

  integer :: clen_l, clen_r
  integer :: n1, n2, n3
  integer :: i3
  integer :: i1s(2), i1e(2), i2s(2), i2e(2), i3s(2), i3e(2)
  integer :: ib1, ib2, ib3

  n_ = NSHOW_DEFAULT
  ismtx_ = .false.
  if( present(n) ) n_ = n
  if( present(ismtx) ) ismtx_ = ismtx

  clen_l = 3
  clen_r = 3
  allocate(character(clen_l) :: c_l_)
  allocate(character(clen_r) :: c_r_)

  n1 = size(x,1)
  n2 = size(x,2)
  n3 = size(x,3)

  if( present(d) )then
    d_ = d
  else
    i1s(1) = 1; i1e(1) = 1; i1s(2) = 1; i1e(2) = n1
    i2s(1) = 1; i2e(1) = 1; i2s(2) = 1; i2e(2) = n2
    i3s(1) = 1; i3e(1) = 1; i3s(2) = 1; i3e(2) = n3
    if( n_*2+1 < n1 )then
      i1e(1) = n_
      i1s(2) = n1 - n_ + 1
    endif
    if( n_*2+1 < n2 )then
      i2e(1) = n_
      i2s(2) = n2 - n_ + 1
    endif
    if( n_*2+1 < n3 )then
      i3e(1) = n_
      i3s(2) = n3 - n_ + 1
    endif

    d_ = 0
    do ib3 = 1, 2
    do ib2 = 1, 2
    do ib1 = 1, 2
      d_ = max(d_, dgt(x(i1s(ib1):i1e(ib1),i2s(ib2):i2e(ib2),i3s(ib3):i3e(ib3)),dgt_opt_max))
    enddo
    enddo
    enddo
  endif

  if( present(nam) )then
    if( ismtx_ )then
      call show_basic_info(&
             nam, (/size(x,2),size(x,1)/), str(minval(x),d_), str(maxval(x),d_))
    else
      call show_basic_info(&
             nam, shape(x), str(minval(x),d_), str(maxval(x),d_))
    endif
  endif

  if( n_*2+1 >= n3 )then
    do i3 = 1, n3
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__int1_2d(x(:,:,i3), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  else
    do i3 = 1, n_
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__int1_2d(x(:,:,i3), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
      call edbg('')
    enddo
    call edbg(str('',clen_l-2)//'...')
    call edbg('')
    do i3 = n3-n_+1, n3
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__int1_2d(x(:,:,i3), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
      if( i3/=n3 ) call edbg('')
    enddo
  endif
end subroutine showarr__int1_3d
!===============================================================
!
!===============================================================
subroutine showarr__int2_1d(x, nam, d, n, c_l, c_r)
  implicit none
  integer(2)  , intent(in) :: x(:)
  character(*), intent(in), optional :: nam
  integer     , intent(in), optional :: d  ! digit
  integer     , intent(in), optional :: n  ! num. of components to show
  character(*), intent(in), optional :: c_l, c_r

  integer :: d_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_

  integer :: n1

  n_ = NSHOW_DEFAULT
  if( present(n) ) n_ = n

  allocate(character(1) :: c_l_)
  allocate(character(1) :: c_r_)
  c_l_ = '['
  c_r_ = ']'
  if( present(c_l) ) c_l_ = c_l
  if( present(c_r) ) c_r_ = c_r

  n1 = size(x)

  if( present(d) )then
    d_ = d
  else
    if( n_*2+1 >= n1 )then
      d_ = dgt(x,dgt_opt_max)
    else
      d_ = max(dgt(x(:n_),dgt_opt_max),dgt(x(n1-n_+1:),dgt_opt_max))
    endif
  endif

  if( present(nam) )then
    call show_basic_info(nam, shape(x), str(minval(x),d_), str(maxval(x),d_))
  endif

  if( n_*2+1 >= n1 )then
    call edbg(c_l_//str(x,d_)//c_r_)
  else
    call edbg(c_l_//str(x(:n_),d_)//' ... '//str(x(n1-n_+1:),d_)//c_r_)
  endif
end subroutine showarr__int2_1d
!===============================================================
!
!===============================================================
subroutine showarr__int2_2d(x, nam, d, n, c_l, c_r, ismtx)
  implicit none
  integer(2)  , intent(in) :: x(:,:)
  character(*), intent(in), optional :: nam
  integer     , intent(in), optional :: d
  integer     , intent(in), optional :: n
  character(*), intent(in), optional :: c_l, c_r
  logical     , intent(in), optional :: ismtx

  integer :: d_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_
  logical :: ismtx_

  integer :: clen_l, clen_r
  character(:), allocatable :: c_l_tmp, c_r_tmp
  integer :: n1, n2
  integer :: i2
  integer :: i1s(2), i1e(2), i2s(2), i2e(2)
  integer :: ib1, ib2

  n_ = NSHOW_DEFAULT
  ismtx_ = .false.
  if( present(n) ) n_ = n
  if( present(ismtx) ) ismtx_ = ismtx

  clen_l = 2
  clen_r = 2
  if( present(c_l) ) clen_l = len(c_l)
  if( present(c_r) ) clen_r = len(c_r)
  allocate(character(clen_l) :: c_l_)
  allocate(character(clen_r) :: c_r_)
  allocate(character(clen_l) :: c_l_tmp)
  allocate(character(clen_r) :: c_r_tmp)
  if( present(c_l) ) c_l_tmp = c_l
  if( present(c_r) ) c_r_tmp = c_r

  n1 = size(x,1)
  n2 = size(x,2)

  if( present(d) )then
    d_ = d
  else
    i1s(1) = 1; i1e(1) = 1; i1s(2) = 1; i1e(2) = n1
    i2s(1) = 1; i2e(1) = 1; i2s(2) = 1; i2e(2) = n2
    if( n_*2+1 < n1 )then
      i1e(1) = n_
      i1s(2) = n1 - n_ + 1
    endif
    if( n_*2+1 < n2 )then
      i2e(1) = n_
      i2s(2) = n2 - n_ + 1
    endif

    d_ = 0
    do ib2 = 1, 2
    do ib1 = 1, 2
      d_ = max(d_, dgt(x(i1s(ib1):i1e(ib1),i2s(ib2):i2e(ib2)),dgt_opt_max))
    enddo
    enddo
  endif

  if( present(nam) )then
    if( ismtx_ )then
      call show_basic_info(&
             nam, (/size(x,2),size(x,1)/), str(minval(x),d_), str(maxval(x),d_))
    else
      call show_basic_info(&
             nam, shape(x), str(minval(x),d_), str(maxval(x),d_))
    endif
  endif

  if( n_*2+1 >= n2 )then
    do i2 = 1, n2
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__int2_1d(x(:,i2), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  else
    do i2 = 1, n_
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__int2_1d(x(:,i2), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
    if( ismtx_ )then
      call edbg(get_str_omitted_row__2d(abs(d_), n_, n1))
    else
      call edbg(str('',clen_l-1)//'...')
    endif
    do i2 = n2-n_+1, n2
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__int2_1d(x(:,i2), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  endif
end subroutine showarr__int2_2d
!===============================================================
!
!===============================================================
subroutine showarr__int2_3d(x, nam, d, n, c_l, c_r, ismtx)
  implicit none
  integer(2)  , intent(in) :: x(:,:,:)
  character(*), intent(in), optional :: nam
  integer     , intent(in), optional :: d
  integer     , intent(in), optional :: n
  character(*), intent(in), optional :: c_l, c_r
  logical     , intent(in), optional :: ismtx

  integer :: d_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_
  logical :: ismtx_

  integer :: clen_l, clen_r
  integer :: n1, n2, n3
  integer :: i3
  integer :: i1s(2), i1e(2), i2s(2), i2e(2), i3s(2), i3e(2)
  integer :: ib1, ib2, ib3

  n_ = NSHOW_DEFAULT
  ismtx_ = .false.
  if( present(n) ) n_ = n
  if( present(ismtx) ) ismtx_ = ismtx

  clen_l = 3
  clen_r = 3
  allocate(character(clen_l) :: c_l_)
  allocate(character(clen_r) :: c_r_)

  n1 = size(x,1)
  n2 = size(x,2)
  n3 = size(x,3)

  if( present(d) )then
    d_ = d
  else
    i1s(1) = 1; i1e(1) = 1; i1s(2) = 1; i1e(2) = n1
    i2s(1) = 1; i2e(1) = 1; i2s(2) = 1; i2e(2) = n2
    i3s(1) = 1; i3e(1) = 1; i3s(2) = 1; i3e(2) = n3
    if( n_*2+1 < n1 )then
      i1e(1) = n_
      i1s(2) = n1 - n_ + 1
    endif
    if( n_*2+1 < n2 )then
      i2e(1) = n_
      i2s(2) = n2 - n_ + 1
    endif
    if( n_*2+1 < n3 )then
      i3e(1) = n_
      i3s(2) = n3 - n_ + 1
    endif

    d_ = 0
    do ib3 = 1, 2
    do ib2 = 1, 2
    do ib1 = 1, 2
      d_ = max(d_, dgt(x(i1s(ib1):i1e(ib1),i2s(ib2):i2e(ib2),i3s(ib3):i3e(ib3)),dgt_opt_max))
    enddo
    enddo
    enddo
  endif

  if( present(nam) )then
    if( ismtx_ )then
      call show_basic_info(&
             nam, (/size(x,2),size(x,1)/), str(minval(x),d_), str(maxval(x),d_))
    else
      call show_basic_info(&
             nam, shape(x), str(minval(x),d_), str(maxval(x),d_))
    endif
  endif

  if( n_*2+1 >= n3 )then
    do i3 = 1, n3
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__int2_2d(x(:,:,i3), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  else
    do i3 = 1, n_
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__int2_2d(x(:,:,i3), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
      call edbg('')
    enddo
    call edbg(str('',clen_l-2)//'...')
    call edbg('')
    do i3 = n3-n_+1, n3
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__int2_2d(x(:,:,i3), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
      if( i3/=n3 ) call edbg('')
    enddo
  endif
end subroutine showarr__int2_3d
!===============================================================
!
!===============================================================
subroutine showarr__int4_1d(x, nam, d, n, c_l, c_r)
  implicit none
  integer(4)  , intent(in) :: x(:)
  character(*), intent(in), optional :: nam
  integer     , intent(in), optional :: d  ! digit
  integer     , intent(in), optional :: n  ! num. of components to show
  character(*), intent(in), optional :: c_l, c_r

  integer :: d_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_

  integer :: n1

  n_ = NSHOW_DEFAULT
  if( present(n) ) n_ = n

  allocate(character(1) :: c_l_)
  allocate(character(1) :: c_r_)
  c_l_ = '['
  c_r_ = ']'
  if( present(c_l) ) c_l_ = c_l
  if( present(c_r) ) c_r_ = c_r

  n1 = size(x)

  if( present(d) )then
    d_ = d
  else
    if( n_*2+1 >= n1 )then
      d_ = dgt(x,dgt_opt_max)
    else
      d_ = max(dgt(x(:n_),dgt_opt_max),dgt(x(n1-n_+1:),dgt_opt_max))
    endif
  endif

  if( present(nam) )then
    call show_basic_info(nam, shape(x), str(minval(x),d_), str(maxval(x),d_))
  endif

  if( n_*2+1 >= n1 )then
    call edbg(c_l_//str(x,d_)//c_r_)
  else
    call edbg(c_l_//str(x(:n_),d_)//' ... '//str(x(n1-n_+1:),d_)//c_r_)
  endif
end subroutine showarr__int4_1d
!===============================================================
!
!===============================================================
subroutine showarr__int4_2d(x, nam, d, n, c_l, c_r, ismtx)
  implicit none
  integer(4)  , intent(in) :: x(:,:)
  character(*), intent(in), optional :: nam
  integer     , intent(in), optional :: d
  integer     , intent(in), optional :: n
  character(*), intent(in), optional :: c_l, c_r
  logical     , intent(in), optional :: ismtx

  integer :: d_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_
  logical :: ismtx_

  integer :: clen_l, clen_r
  character(:), allocatable :: c_l_tmp, c_r_tmp
  integer :: n1, n2
  integer :: i2
  integer :: i1s(2), i1e(2), i2s(2), i2e(2)
  integer :: ib1, ib2

  n_ = NSHOW_DEFAULT
  ismtx_ = .false.
  if( present(n) ) n_ = n
  if( present(ismtx) ) ismtx_ = ismtx

  clen_l = 2
  clen_r = 2
  if( present(c_l) ) clen_l = len(c_l)
  if( present(c_r) ) clen_r = len(c_r)
  allocate(character(clen_l) :: c_l_)
  allocate(character(clen_r) :: c_r_)
  allocate(character(clen_l) :: c_l_tmp)
  allocate(character(clen_r) :: c_r_tmp)
  if( present(c_l) ) c_l_tmp = c_l
  if( present(c_r) ) c_r_tmp = c_r

  n1 = size(x,1)
  n2 = size(x,2)

  if( present(d) )then
    d_ = d
  else
    i1s(1) = 1; i1e(1) = 1; i1s(2) = 1; i1e(2) = n1
    i2s(1) = 1; i2e(1) = 1; i2s(2) = 1; i2e(2) = n2
    if( n_*2+1 < n1 )then
      i1e(1) = n_
      i1s(2) = n1 - n_ + 1
    endif
    if( n_*2+1 < n2 )then
      i2e(1) = n_
      i2s(2) = n2 - n_ + 1
    endif

    d_ = 0
    do ib2 = 1, 2
    do ib1 = 1, 2
      d_ = max(d_, dgt(x(i1s(ib1):i1e(ib1),i2s(ib2):i2e(ib2)),dgt_opt_max))
    enddo
    enddo
  endif

  if( present(nam) )then
    if( ismtx_ )then
      call show_basic_info(&
             nam, (/size(x,2),size(x,1)/), str(minval(x),d_), str(maxval(x),d_))
    else
      call show_basic_info(&
             nam, shape(x), str(minval(x),d_), str(maxval(x),d_))
    endif
  endif

  if( n_*2+1 >= n2 )then
    do i2 = 1, n2
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__int4_1d(x(:,i2), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  else
    do i2 = 1, n_
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__int4_1d(x(:,i2), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
    if( ismtx_ )then
      call edbg(get_str_omitted_row__2d(abs(d_), n_, n1))
    else
      call edbg(str('',clen_l-1)//'...')
    endif
    do i2 = n2-n_+1, n2
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__int4_1d(x(:,i2), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  endif
end subroutine showarr__int4_2d
!===============================================================
!
!===============================================================
subroutine showarr__int4_3d(x, nam, d, n, c_l, c_r, ismtx)
  implicit none
  integer(4)  , intent(in) :: x(:,:,:)
  character(*), intent(in), optional :: nam
  integer     , intent(in), optional :: d
  integer     , intent(in), optional :: n
  character(*), intent(in), optional :: c_l, c_r
  logical     , intent(in), optional :: ismtx

  integer :: d_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_
  logical :: ismtx_

  integer :: clen_l, clen_r
  integer :: n1, n2, n3
  integer :: i3
  integer :: i1s(2), i1e(2), i2s(2), i2e(2), i3s(2), i3e(2)
  integer :: ib1, ib2, ib3

  n_ = NSHOW_DEFAULT
  ismtx_ = .false.
  if( present(n) ) n_ = n
  if( present(ismtx) ) ismtx_ = ismtx

  clen_l = 3
  clen_r = 3
  allocate(character(clen_l) :: c_l_)
  allocate(character(clen_r) :: c_r_)

  n1 = size(x,1)
  n2 = size(x,2)
  n3 = size(x,3)

  if( present(d) )then
    d_ = d
  else
    i1s(1) = 1; i1e(1) = 1; i1s(2) = 1; i1e(2) = n1
    i2s(1) = 1; i2e(1) = 1; i2s(2) = 1; i2e(2) = n2
    i3s(1) = 1; i3e(1) = 1; i3s(2) = 1; i3e(2) = n3
    if( n_*2+1 < n1 )then
      i1e(1) = n_
      i1s(2) = n1 - n_ + 1
    endif
    if( n_*2+1 < n2 )then
      i2e(1) = n_
      i2s(2) = n2 - n_ + 1
    endif
    if( n_*2+1 < n3 )then
      i3e(1) = n_
      i3s(2) = n3 - n_ + 1
    endif

    d_ = 0
    do ib3 = 1, 2
    do ib2 = 1, 2
    do ib1 = 1, 2
      d_ = max(d_, dgt(x(i1s(ib1):i1e(ib1),i2s(ib2):i2e(ib2),i3s(ib3):i3e(ib3)),dgt_opt_max))
    enddo
    enddo
    enddo
  endif

  if( present(nam) )then
    if( ismtx_ )then
      call show_basic_info(&
             nam, (/size(x,2),size(x,1)/), str(minval(x),d_), str(maxval(x),d_))
    else
      call show_basic_info(&
             nam, shape(x), str(minval(x),d_), str(maxval(x),d_))
    endif
  endif

  if( n_*2+1 >= n3 )then
    do i3 = 1, n3
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__int4_2d(x(:,:,i3), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  else
    do i3 = 1, n_
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__int4_2d(x(:,:,i3), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
      call edbg('')
    enddo
    call edbg(str('',clen_l-2)//'...')
    call edbg('')
    do i3 = n3-n_+1, n3
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__int4_2d(x(:,:,i3), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
      if( i3/=n3 ) call edbg('')
    enddo
  endif
end subroutine showarr__int4_3d
!===============================================================
!
!===============================================================
subroutine showarr__int8_1d(x, nam, d, n, c_l, c_r)
  implicit none
  integer(8)  , intent(in) :: x(:)
  character(*), intent(in), optional :: nam
  integer     , intent(in), optional :: d  ! digit
  integer     , intent(in), optional :: n  ! num. of components to show
  character(*), intent(in), optional :: c_l, c_r

  integer :: d_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_

  integer :: n1

  n_ = NSHOW_DEFAULT
  if( present(n) ) n_ = n

  allocate(character(1) :: c_l_)
  allocate(character(1) :: c_r_)
  c_l_ = '['
  c_r_ = ']'
  if( present(c_l) ) c_l_ = c_l
  if( present(c_r) ) c_r_ = c_r

  n1 = size(x)

  if( present(d) )then
    d_ = d
  else
    if( n_*2+1 >= n1 )then
      d_ = dgt(x,dgt_opt_max)
    else
      d_ = max(dgt(x(:n_),dgt_opt_max),dgt(x(n1-n_+1:),dgt_opt_max))
    endif
  endif

  if( present(nam) )then
    call show_basic_info(nam, shape(x), str(minval(x),d_), str(maxval(x),d_))
  endif

  if( n_*2+1 >= n1 )then
    call edbg(c_l_//str(x,d_)//c_r_)
  else
    call edbg(c_l_//str(x(:n_),d_)//' ... '//str(x(n1-n_+1:),d_)//c_r_)
  endif
end subroutine showarr__int8_1d
!===============================================================
!
!===============================================================
subroutine showarr__int8_2d(x, nam, d, n, c_l, c_r, ismtx)
  implicit none
  integer(8)  , intent(in) :: x(:,:)
  character(*), intent(in), optional :: nam
  integer     , intent(in), optional :: d
  integer     , intent(in), optional :: n
  character(*), intent(in), optional :: c_l, c_r
  logical     , intent(in), optional :: ismtx

  integer :: d_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_
  logical :: ismtx_

  integer :: clen_l, clen_r
  character(:), allocatable :: c_l_tmp, c_r_tmp
  integer :: n1, n2
  integer :: i2
  integer :: i1s(2), i1e(2), i2s(2), i2e(2)
  integer :: ib1, ib2

  n_ = NSHOW_DEFAULT
  ismtx_ = .false.
  if( present(n) ) n_ = n
  if( present(ismtx) ) ismtx_ = ismtx

  clen_l = 2
  clen_r = 2
  if( present(c_l) ) clen_l = len(c_l)
  if( present(c_r) ) clen_r = len(c_r)
  allocate(character(clen_l) :: c_l_)
  allocate(character(clen_r) :: c_r_)
  allocate(character(clen_l) :: c_l_tmp)
  allocate(character(clen_r) :: c_r_tmp)
  if( present(c_l) ) c_l_tmp = c_l
  if( present(c_r) ) c_r_tmp = c_r

  n1 = size(x,1)
  n2 = size(x,2)

  if( present(d) )then
    d_ = d
  else
    i1s(1) = 1; i1e(1) = 1; i1s(2) = 1; i1e(2) = n1
    i2s(1) = 1; i2e(1) = 1; i2s(2) = 1; i2e(2) = n2
    if( n_*2+1 < n1 )then
      i1e(1) = n_
      i1s(2) = n1 - n_ + 1
    endif
    if( n_*2+1 < n2 )then
      i2e(1) = n_
      i2s(2) = n2 - n_ + 1
    endif

    d_ = 0
    do ib2 = 1, 2
    do ib1 = 1, 2
      d_ = max(d_, dgt(x(i1s(ib1):i1e(ib1),i2s(ib2):i2e(ib2)),dgt_opt_max))
    enddo
    enddo
  endif

  if( present(nam) )then
    if( ismtx_ )then
      call show_basic_info(&
             nam, (/size(x,2),size(x,1)/), str(minval(x),d_), str(maxval(x),d_))
    else
      call show_basic_info(&
             nam, shape(x), str(minval(x),d_), str(maxval(x),d_))
    endif
  endif

  if( n_*2+1 >= n2 )then
    do i2 = 1, n2
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__int8_1d(x(:,i2), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  else
    do i2 = 1, n_
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__int8_1d(x(:,i2), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
    if( ismtx_ )then
      call edbg(get_str_omitted_row__2d(abs(d_), n_, n1))
    else
      call edbg(str('',clen_l-1)//'...')
    endif
    do i2 = n2-n_+1, n2
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__int8_1d(x(:,i2), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  endif
end subroutine showarr__int8_2d
!===============================================================
!
!===============================================================
subroutine showarr__int8_3d(x, nam, d, n, c_l, c_r, ismtx)
  implicit none
  integer(8)  , intent(in) :: x(:,:,:)
  character(*), intent(in), optional :: nam
  integer     , intent(in), optional :: d
  integer     , intent(in), optional :: n
  character(*), intent(in), optional :: c_l, c_r
  logical     , intent(in), optional :: ismtx

  integer :: d_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_
  logical :: ismtx_

  integer :: clen_l, clen_r
  integer :: n1, n2, n3
  integer :: i3
  integer :: i1s(2), i1e(2), i2s(2), i2e(2), i3s(2), i3e(2)
  integer :: ib1, ib2, ib3

  n_ = NSHOW_DEFAULT
  ismtx_ = .false.
  if( present(n) ) n_ = n
  if( present(ismtx) ) ismtx_ = ismtx

  clen_l = 3
  clen_r = 3
  allocate(character(clen_l) :: c_l_)
  allocate(character(clen_r) :: c_r_)

  n1 = size(x,1)
  n2 = size(x,2)
  n3 = size(x,3)

  if( present(d) )then
    d_ = d
  else
    i1s(1) = 1; i1e(1) = 1; i1s(2) = 1; i1e(2) = n1
    i2s(1) = 1; i2e(1) = 1; i2s(2) = 1; i2e(2) = n2
    i3s(1) = 1; i3e(1) = 1; i3s(2) = 1; i3e(2) = n3
    if( n_*2+1 < n1 )then
      i1e(1) = n_
      i1s(2) = n1 - n_ + 1
    endif
    if( n_*2+1 < n2 )then
      i2e(1) = n_
      i2s(2) = n2 - n_ + 1
    endif
    if( n_*2+1 < n3 )then
      i3e(1) = n_
      i3s(2) = n3 - n_ + 1
    endif

    d_ = 0
    do ib3 = 1, 2
    do ib2 = 1, 2
    do ib1 = 1, 2
      d_ = max(d_, dgt(x(i1s(ib1):i1e(ib1),i2s(ib2):i2e(ib2),i3s(ib3):i3e(ib3)),dgt_opt_max))
    enddo
    enddo
    enddo
  endif

  if( present(nam) )then
    if( ismtx_ )then
      call show_basic_info(&
             nam, (/size(x,2),size(x,1)/), str(minval(x),d_), str(maxval(x),d_))
    else
      call show_basic_info(&
             nam, shape(x), str(minval(x),d_), str(maxval(x),d_))
    endif
  endif

  if( n_*2+1 >= n3 )then
    do i3 = 1, n3
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__int8_2d(x(:,:,i3), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  else
    do i3 = 1, n_
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__int8_2d(x(:,:,i3), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
      call edbg('')
    enddo
    call edbg(str('',clen_l-2)//'...')
    call edbg('')
    do i3 = n3-n_+1, n3
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__int8_2d(x(:,:,i3), d=d_, n=n_, c_l=c_l_, c_r=c_r_)
      if( i3/=n3 ) call edbg('')
    enddo
  endif
end subroutine showarr__int8_3d
!===============================================================
!
!===============================================================
subroutine showarr__real_1d(x, nam, fmt, n, c_l, c_r)
  implicit none
  real(4)     , intent(in) :: x(:)
  character(*), intent(in), optional :: nam
  character(*), intent(in), optional :: fmt
  integer     , intent(in), optional :: n
  character(*), intent(in), optional :: c_l, c_r

  character(CLEN_WFMT) :: fmt_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_

  integer :: n1

  fmt_ = get_wfmt_real()
  n_ = NSHOW_DEFAULT
  if( present(fmt) ) fmt_ = fmt
  if( present(n) ) n_ = n

  allocate(character(1) :: c_l_)
  allocate(character(1) :: c_r_)
  c_l_ = '['
  c_r_ = ']'
  if( present(c_l) ) c_l_ = c_l
  if( present(c_r) ) c_r_ = c_r

  n1 = size(x)

  if( present(nam) )then
    call show_basic_info(nam, shape(x), str(minval(x),fmt_), str(maxval(x),fmt_))
  endif

  if( n_*2+1 >= n1 )then
    call edbg(c_l_//str(x,fmt_)//c_r_)
  else
    call edbg(c_l_//str(x(:n_),fmt_)//' ... '//str(x(n1-n_+1:),fmt_)//c_r_)
  endif
end subroutine showarr__real_1d
!===============================================================
!
!===============================================================
subroutine showarr__real_2d(x, nam, fmt, n, c_l, c_r, ismtx)
  implicit none
  real(4)     , intent(in) :: x(:,:)
  character(*), intent(in), optional :: nam
  character(*), intent(in), optional :: fmt
  integer     , intent(in), optional :: n
  character(*), intent(in), optional :: c_l, c_r
  logical     , intent(in), optional :: ismtx

  character(CLEN_WFMT) :: fmt_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_
  logical :: ismtx_

  integer :: clen_l, clen_r
  character(:), allocatable :: c_l_tmp, c_r_tmp
  integer :: n1, n2
  integer :: i2

  fmt_ = get_wfmt_real()
  n_ = NSHOW_DEFAULT
  ismtx_ = .false.
  if( present(fmt) ) fmt_ = fmt
  if( present(n) ) n_ = n
  if( present(ismtx) ) ismtx_ = ismtx

  clen_l = 2
  clen_r = 2
  if( present(c_l) ) clen_l = len(c_l)
  if( present(c_r) ) clen_r = len(c_r)
  allocate(character(clen_l) :: c_l_)
  allocate(character(clen_r) :: c_r_)
  allocate(character(clen_l) :: c_l_tmp)
  allocate(character(clen_r) :: c_r_tmp)
  if( present(c_l) ) c_l_tmp = c_l
  if( present(c_r) ) c_r_tmp = c_r

  n1 = size(x,1)
  n2 = size(x,2)

  if( present(nam) )then
    if( ismtx_ )then
      call show_basic_info(&
             nam, (/n2,n1/), str(minval(x),fmt_), str(maxval(x),fmt_))
    else
      call show_basic_info(&
             nam, shape(x), str(minval(x),fmt_), str(maxval(x),fmt_))
    endif
  endif

  if( n_*2+1 >= n2 )then
    do i2 = 1, n2
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__real_1d(x(:,i2), fmt=fmt_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  else
    do i2 = 1, n_
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__real_1d(x(:,i2), fmt=fmt_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
    if( ismtx_ )then
      call edbg(get_str_omitted_row__2d(cl(fmt_), n_, n1))
    else
      call edbg(str('',clen_l-1)//'...')
    endif
    do i2 = n2-n_+1, n2
      call get_c_edge__2d(&
             present(c_l), c_l_tmp, present(c_r), c_r_tmp, clen_l, clen_r, i2, n2, &
             c_l_, c_r_)
      call showarr__real_1d(x(:,i2), fmt=fmt_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  endif
end subroutine showarr__real_2d
!===============================================================
!
!===============================================================
subroutine showarr__real_3d(x, nam, fmt, n)
  implicit none
  real(4)     , intent(in) :: x(:,:,:)
  character(*), intent(in), optional :: nam
  character(*), intent(in), optional :: fmt
  integer     , intent(in), optional :: n

  character(CLEN_WFMT) :: fmt_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_

  integer :: clen_l, clen_r
  integer :: n1, n2, n3
  integer :: i3

  fmt_ = get_wfmt_real()
  n_ = NSHOW_DEFAULT
  if( present(fmt) ) fmt_ = fmt
  if( present(n) ) n_ = n

  clen_l = 3
  clen_r = 3
  allocate(character(clen_l) :: c_l_)
  allocate(character(clen_r) :: c_r_)

  n1 = size(x,1)
  n2 = size(x,2)
  n3 = size(x,3)

  if( present(nam) )then
    call show_basic_info(&
           nam, shape(x), str(minval(x),fmt_), str(maxval(x),fmt_))
  endif

  if( n_*2+1 >= n3 )then
    do i3 = 1, n3
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__real_2d(x(:,:,i3), fmt=fmt_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  else
    do i3 = 1, n_
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__real_2d(x(:,:,i3), fmt=fmt_, n=n_, c_l=c_l_, c_r=c_r_)
      call edbg('')
    enddo
    call edbg(str('',clen_l-2)//'...')
    call edbg('')
    do i3 = n3-n_+1, n3
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__real_2d(x(:,:,i3), fmt=fmt_, n=n_, c_l=c_l_, c_r=c_r_)
      if( i3/=n3 ) call edbg('')
    enddo
  endif
end subroutine showarr__real_3d
!===============================================================
!
!===============================================================
subroutine showarr__dble_1d(x, nam, fmt, n, c_l, c_r)
  implicit none
  real(8)     , intent(in) :: x(:)
  character(*), intent(in), optional :: nam
  character(*), intent(in), optional :: fmt
  integer     , intent(in), optional :: n
  character(*), intent(in), optional :: c_l, c_r

  character(CLEN_WFMT) :: fmt_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_

  integer :: n1

  fmt_ = get_wfmt_real()
  n_ = NSHOW_DEFAULT
  if( present(fmt) ) fmt_ = fmt
  if( present(n) ) n_ = n

  allocate(character(1) :: c_l_)
  allocate(character(1) :: c_r_)
  c_l_ = '['
  c_r_ = ']'
  if( present(c_l) ) c_l_ = c_l
  if( present(c_r) ) c_r_ = c_r

  n1 = size(x)

  if( present(nam) )then
    call show_basic_info(nam, shape(x), str(minval(x),fmt_), str(maxval(x),fmt_))
  endif

  if( n_*2+1 >= n1 )then
    call edbg(c_l_//str(x,fmt_)//c_r_)
  else
    call edbg(c_l_//str(x(:n_),fmt_)//' ... '//str(x(n1-n_+1:),fmt_)//c_r_)
  endif
end subroutine showarr__dble_1d
!===============================================================
!
!===============================================================
subroutine showarr__dble_2d(x, nam, fmt, n, c_l, c_r, ismtx)
  implicit none
  real(8)     , intent(in) :: x(:,:)
  character(*), intent(in), optional :: nam
  character(*), intent(in), optional :: fmt
  integer     , intent(in), optional :: n
  character(*), intent(in), optional :: c_l, c_r
  logical     , intent(in), optional :: ismtx

  character(CLEN_WFMT) :: fmt_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_
  logical :: ismtx_

  integer :: clen_l, clen_r
  integer :: n1, n2
  integer :: i2

  fmt_ = get_wfmt_real()
  n_ = NSHOW_DEFAULT
  ismtx_ = .false.
  if( present(fmt) ) fmt_ = fmt
  if( present(n) ) n_ = n
  if( present(ismtx) ) ismtx_ = ismtx

  clen_l = 2
  clen_r = 2
  if( present(c_l) ) clen_l = len(c_l)
  if( present(c_r) ) clen_r = len(c_r)
  allocate(character(clen_l) :: c_l_)
  allocate(character(clen_r) :: c_r_)

  n1 = size(x,1)
  n2 = size(x,2)

  if( present(nam) )then
    if( ismtx_ )then
      call show_basic_info(&
             nam, (/n2,n1/), str(minval(x),fmt_), str(maxval(x),fmt_))
    else
      call show_basic_info(&
             nam, shape(x), str(minval(x),fmt_), str(maxval(x),fmt_))
    endif
  endif

  if( n_*2+1 >= n2 )then
    do i2 = 1, n2
      call get_c_edge()
      call showarr__dble_1d(x(:,i2), fmt=fmt_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  else
    do i2 = 1, n_
      call get_c_edge()
      call showarr__dble_1d(x(:,i2), fmt=fmt_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
    if( ismtx_ )then
      call edbg(get_str_omitted_row__2d(cl(fmt_), n_, n1))
    else
      call edbg(str('',clen_r-1)//'...')
    endif
    do i2 = n2-n_+1, n2
      call get_c_edge()
      call showarr__dble_1d(x(:,i2), fmt=fmt_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  endif

contains
subroutine get_c_edge()
  implicit none
  integer :: i

  if( present(c_l) )then
    if( i2 == 1 )then
      c_l_ = c_l
    else
      do i = 1, clen_l-1
        c_l_(i:i) = ' '
      enddo
      c_l_(clen_l:clen_l) = '['
    endif
  else
    if( i2 == 1 )then
      c_l_ = '[['
    else
      c_l_ = ' ['
    endif
  endif

  if( present(c_r) )then
    if( i2 == n2 )then
      c_r_ = c_r
    else
      c_r_(1:1) = ']'
      do i = 2, clen_r
        c_r_(i:i) = ' '
      enddo
    endif
  else
    if( i2 == n2 )then
      c_r_ = ']]'
    else
      c_r_ = '],'
    endif
  endif
end subroutine get_c_edge
end subroutine showarr__dble_2d
!===============================================================
!
!===============================================================
subroutine showarr__dble_3d(x, nam, fmt, n)
  implicit none
  real(8)     , intent(in) :: x(:,:,:)
  character(*), intent(in), optional :: nam
  character(*), intent(in), optional :: fmt
  integer     , intent(in), optional :: n

  character(CLEN_WFMT) :: fmt_
  integer :: n_
  character(:), allocatable :: c_l_, c_r_

  integer :: clen_l, clen_r
  integer :: n1, n2, n3
  integer :: i3

  fmt_ = get_wfmt_real()
  n_ = NSHOW_DEFAULT
  if( present(fmt) ) fmt_ = fmt
  if( present(n) ) n_ = n

  clen_l = 3
  clen_r = 3
  allocate(character(clen_l) :: c_l_)
  allocate(character(clen_r) :: c_r_)

  n1 = size(x,1)
  n2 = size(x,2)
  n3 = size(x,3)

  if( present(nam) )then
    call show_basic_info(&
           nam, shape(x), str(minval(x),fmt_), str(maxval(x),fmt_))
  endif

  if( n_*2+1 >= n3 )then
    do i3 = 1, n3
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__dble_2d(x(:,:,i3), fmt=fmt_, n=n_, c_l=c_l_, c_r=c_r_)
    enddo
  else
    do i3 = 1, n_
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__dble_2d(x(:,:,i3), fmt=fmt_, n=n_, c_l=c_l_, c_r=c_r_)
      call edbg('')
    enddo
    call edbg(str('',clen_l-2)//'...')
    call edbg('')
    do i3 = n3-n_+1, n3
      call get_c_edge__3d(&
             .false., '', .false., '', clen_l, clen_r, i3, n3, &
             c_l_, c_r_)
      call showarr__dble_2d(x(:,:,i3), fmt=fmt_, n=n_, c_l=c_l_, c_r=c_r_)
      if( i3/=n3 ) call edbg('')
    enddo
  endif
end subroutine showarr__dble_3d
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
subroutine showmat__int4(x, nam, d, n)
  implicit none
  integer(4)  , intent(in) :: x(:,:)
  character(*), intent(in), optional :: nam
  integer     , intent(in), optional :: d
  integer     , intent(in), optional :: n

  integer :: n_

  n_ = NSHOW_DEFAULT
  if( present(n) ) n_ = n

  if( present(nam) )then
    if( present(d) )then
      call showarr__int4_2d(transpose(x), nam, d, n_, ismtx=.true.)
    else
      call showarr__int4_2d(transpose(x), nam, n=n_, ismtx=.true.)
    endif
  else
    if( present(d) )then
      call showarr__int4_2d(transpose(x), d=d, n=n_, ismtx=.true.)
    else
      call showarr__int4_2d(transpose(x), n=n_, ismtx=.true.)
    endif
  endif
end subroutine showmat__int4
!===============================================================
!
!===============================================================
subroutine showmat__dble(x, nam, fmt, n)
  implicit none
  real(8)     , intent(in) :: x(:,:)
  character(*), intent(in), optional :: nam
  character(*), intent(in), optional :: fmt
  integer     , intent(in), optional :: n

  character(CLEN_WFMT) :: fmt_
  integer :: n_

  fmt_ = get_wfmt_real()
  n_ = NSHOW_DEFAULT
  if( present(fmt) ) fmt_ = fmt
  if( present(n) ) n_ = n

  if( present(nam) )then
    call showarr__dble_2d(transpose(x), nam, fmt_, n_, ismtx=.true.)
  else
    call showarr__dble_2d(transpose(x), fmt=fmt_, n=n_, ismtx=.true.)
  endif
end subroutine showmat__dble
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
subroutine get_c_edge__2d(&
    present_c_l, c_l, present_c_r, c_r, clen_l, clen_r, i, n, &
    c_l_, c_r_)
  implicit none
  logical, intent(in) :: present_c_l, present_c_r
  character(*), intent(in) :: c_l, c_r
  integer, intent(in) :: clen_l, clen_r
  integer, intent(in) :: i, n
  character(*), intent(out) :: c_l_, c_r_

  integer :: ic

  if( present_c_l )then
    if( i == 1 )then
      c_l_ = c_l
    else
      do ic = 1, clen_l-1
        c_l_(ic:ic) = ' '
      enddo
      c_l_(clen_l:clen_l) = '['
    endif
  else
    if( i == 1 )then
      c_l_ = '[['
    else
      c_l_ = ' ['
    endif
  endif

  if( present_c_r )then
    if( i == n )then
      c_r_ = c_r
    else
      c_r_(1:1) = ']'
      do ic = 2, clen_r
        c_r_(ic:ic) = ' '
      enddo
    endif
  else
    if( i == n )then
      c_r_ = ']]'
    else
      c_r_ = '] '
    endif
  endif
end subroutine get_c_edge__2d
!===============================================================
!
!===============================================================
subroutine get_c_edge__3d(&
    present_c_l, c_l, present_c_r, c_r, clen_l, clen_r, i, n, &
    c_l_, c_r_)
  implicit none
  logical, intent(in) :: present_c_l, present_c_r
  character(*), intent(in) :: c_l, c_r
  integer, intent(in) :: clen_l, clen_r
  integer, intent(in) :: i, n
  character(*), intent(out) :: c_l_, c_r_

  integer :: ic

  if( present_c_l )then
    if( i == 1 )then
      c_l_ = c_l
    else
      do ic = 1, clen_l-2
        c_l_(ic:ic) = ' '
      enddo
      c_l_(clen_l-1:clen_l) = '[['
    endif
  else
    if( i == 1 )then
      c_l_ = '[[['
    else
      c_l_ = ' [['
    endif
  endif

  if( present_c_r )then
    if( i == n )then
      c_r_ = c_r
    else
      c_r_(1:2) = ']]'
      do ic = 3, clen_r
        c_r_(ic:ic) = ' '
      enddo
    endif
  else
    if( i == n )then
      c_r_ = ']]]'
    else
      c_r_ = ']] '
    endif
  endif
end subroutine get_c_edge__3d
!===============================================================
!
!===============================================================
function get_str_omitted_row__2d(cl0, nshow, n1) result(sr)
  implicit none
  integer, intent(in) :: cl0
  integer, intent(in) :: nshow
  integer, intent(in) :: n1
  character(:), allocatable :: sr

  character(:), allocatable :: sr1
  integer :: i1

  allocate(character(1) :: sr1)
  if( mod(cl0,2) == 0 )then
    sr1 = str('',cl0/2)//':'//str('',cl0/2-1)
  else
    sr1 = str('',(cl0-1)/2)//':'//str('',(cl0-1)/2)
  endif

  allocate(character(1) :: sr)
  sr = ''
  if( nshow*2+1 < n1 )then
    do i1 = 1, nshow
      sr = sr//sr1//' '
    enddo
    do i1 = 1, 1  ! Loop for avoiding warning "maybe-uninitialized"
      sr = sr//str('',4)
    enddo
    do i1 = 1, nshow
      sr = sr//sr1//' '
    enddo
  else
    do i1 = 1, n1
      sr = sr//sr1//' '
    enddo
  endif
  sr = trim(sr)
end function get_str_omitted_row__2d
!===============================================================
!
!===============================================================
subroutine show_basic_info(nam, shp, smin, smax)
  implicit none
  character(*), intent(in) :: nam
  integer     , intent(in) :: shp(:)
  character(*), intent(in) :: smin, smax

  if( nam == '' )then
    call edbg('('//str(shp,', ')//')'//&
              ' (min,max): ('//smin//', '//smax//')')
  else
    call edbg(str(nam)//' ('//str(shp,', ')//')'//&
              ' (min,max): ('//smin//', '//smax//')')
  endif
end subroutine show_basic_info
!===============================================================
!
!===============================================================
end module lib_log_array
