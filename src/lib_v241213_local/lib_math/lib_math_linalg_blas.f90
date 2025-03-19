module lib_math_linalg_blas
  use lib_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: BLAS_SYMV
  public :: BLAS_GEMV
  public :: BLAS_GEVM
  public :: BLAS_GEMM
  public :: BLAS_GEMM1
  public :: BLAS_GEMM2
  !-------------------------------------------------------------
  ! External Procedures
  !-------------------------------------------------------------
  external :: DCOPY
  external :: DSYMV
  external :: DGEMV
  external :: DGEMM
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface BLAS_SYMV
    module procedure BLAS_SYMV__out_dble
  end interface

  interface BLAS_GEMV
    module procedure BLAS_GEMV__out_dble
  end interface

  interface BLAS_GEVM
    module procedure BLAS_GEVM__out_dble
  end interface

  interface BLAS_GEMM
    module procedure BLAS_GEMM__out_dble
  end interface

  interface BLAS_GEMM1
    module procedure BLAS_GEMM__inout1_dble
  end interface

  interface BLAS_GEMM2
    module procedure BLAS_GEMM__inout2_dble
  end interface
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(32), parameter :: name_mod = 'lib_math_linalg_blas'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine BLAS_SYMV__out_dble(uplo, A, x, y)
  implicit none
  character(1), intent(in)  :: uplo
  real(8)     , intent(in)  :: A(:,:)
  real(8)     , intent(in)  :: x(:)
  real(8)     , intent(out) :: y(:)

  integer :: n
  real(8), parameter :: alpha = 1.d0
  real(8), parameter :: beta  = 0.d0
  integer, parameter :: xinc = 1
  integer, parameter :: yinc = 1
  character(CLEN_PROC), parameter :: msg_proc = &
    '*** @ '//trim(name_mod)//' SUBROUTINE BLAS_SYMV__out_dble ***'

  n = size(A,1)

  call DSYMV(uplo, n, alpha, A, n, x, xinc, beta, y, yinc)
end subroutine BLAS_SYMV__out_dble
!===============================================================
!
!===============================================================
subroutine BLAS_GEMV__out_dble(trans, A, x, y)
  implicit none
  character(1), intent(in)  :: trans
  real(8)     , intent(in)  :: A(:,:)
  real(8)     , intent(in)  :: x(:)
  real(8)     , intent(out) :: y(:)

  integer :: m, n
  integer :: ldA
  real(8), parameter :: alpha = 1.d0
  real(8), parameter :: beta  = 0.d0
  integer, parameter :: xinc = 1
  integer, parameter :: yinc = 1
  character(CLEN_PROC), parameter :: msg_proc = &
    '*** @ '//trim(name_mod)//' SUBROUTINE BLAS_GEMV__out_dble ***'

  m = size(A,1)
  n = size(A,2)
  ldA = m

  selectcase( trans )
  case( 'N', 'n' )
    if( size(x) /= n )then
      write(0,*) trim(msg_proc)
      write(0,*) 'Incorrect shapes.'
      stop
    endif
  case( 'T', 't' )
    if( size(x) /= m )then
      write(0,*) trim(msg_proc)
      write(0,*) 'Incorrect shapes.'
      stop
    endif
  case default
    write(0,*) trim(msg_proc)
    write(0,*) 'Invalid value in $trans: '//trim(trans)
    stop
  endselect

  call DGEMV(trans, m, n, alpha, A, ldA, x, xinc, beta, y, yinc)
end subroutine BLAS_GEMV__out_dble
!===============================================================
! y <- x op(A)
!===============================================================
subroutine BLAS_GEVM__out_dble(trans, x, A, y)
  implicit none
  character(1), intent(in)  :: trans
  real(8)     , intent(in)  :: x(:)
  real(8)     , intent(in)  :: A(:,:)
  real(8)     , intent(out) :: y(:)

  integer :: m, n
  integer :: ldA
  character(1) :: trans_
  real(8), parameter :: alpha = 1.d0
  real(8), parameter :: beta  = 0.d0
  integer, parameter :: xinc = 1
  integer, parameter :: yinc = 1
  character(CLEN_PROC), parameter :: msg_proc = &
    '*** @ '//trim(name_mod)//' SUBROUTINE BLAS_GEVM__out_dble ***'

  m = size(A,1)
  n = size(A,2)
  ldA = m

  selectcase( trans )
  case( 'N', 'n' )
    trans_ = 'T'
    if( size(x) /= m )then
      write(0,*) trim(msg_proc)
      write(0,*) 'Incorrect shapes.'
      stop
    endif
  case( 'T', 't' )
    trans_ = 'N'
    if( size(x) /= n )then
      write(0,*) trim(msg_proc)
      write(0,*) 'Incorrect shapes.'
      stop
    endif
  case default
    write(0,*) trim(msg_proc)
    write(0,*) 'Invalid value in $trans: '//trim(trans)
    stop
  endselect

  call DGEMV(trans_, m, n, alpha, A, ldA, x, xinc, beta, y, yinc)
end subroutine BLAS_GEVM__out_dble
!===============================================================
! C <- op(A)op(B)
! op(A) = A    if transA is 'N' or 'n'
!       = A'   if transA is 'T' or 't'
! op(B) = B    if transB is 'N' or 'n'
!       = B'   if transB is 'T' or 't'
!===============================================================
subroutine BLAS_GEMM__out_dble(transA, transB, A, B, C)
  implicit none
  character(1), intent(in)  :: transA, transB
  real(8)     , intent(in)  :: A(:,:), B(:,:)
  real(8)     , intent(out) :: C(:,:)

  integer :: n, m, k
  integer :: ldA, ldB, ldC
  real(8), parameter :: alpha = 1.d0
  real(8), parameter :: beta  = 0.d0
  character(CLEN_PROC), parameter :: msg_proc = &
    '*** @ '//trim(name_mod)//' SUBROUTINE BLAS_GEMM__out_dble ***'

  selectcase( transA )
  case( 'N', 'n' )
    n = size(A,1)
    k = size(A,2)
  case( 'T', 't' )
    n = size(A,2)
    k = size(A,1)
  case default
    write(0,*) trim(msg_proc)
    write(0,*) 'Invalid value in $transA: '//trim(transA)
    stop
  endselect
  selectcase( transB )
  case( 'N', 'n' )
    m = size(B,2)
    if( k /= size(B,1) )then
      write(0,*) trim(msg_proc)
      write(0,*) 'Incorrect shapes.'
      write(0,*) 'transA: '//trim(transA)//', shape(A):',shape(A)
      write(0,*) 'transB: '//trim(transB)//', shape(B):',shape(B)
      stop
    endif
  case( 'T', 't' )
    m = size(B,1)
    if( k /= size(B,2) )then
      write(0,*) trim(msg_proc)
      write(0,*) 'Incorrect shapes.'
      write(0,*) 'transA: '//trim(transA)//', shape(A):',shape(A)
      write(0,*) 'transB: '//trim(transB)//', shape(B):',shape(B)
      stop
    endif
  case default
    write(0,*) trim(msg_proc)
    write(0,*) 'Invalid value in $transB: '//trim(transB)
    stop
  endselect
  ldA = size(A,1)
  ldB = size(B,1)
  ldC = size(C,1)

  call DGEMM(transA, transB, n, m, k, alpha, A, ldA, B, ldB, beta, C, ldC)
end subroutine BLAS_GEMM__out_dble
!===============================================================
! A <- op(A)op(B)  if which is 1
! op(A) = A    if transA is 'N' or 'n'
!       = A'   if transA is 'T' or 't'
! op(B) = B    if transB is 'N' or 'n'
!       = B'   if transB is 'T' or 't'
!===============================================================
subroutine BLAS_GEMM__inout1_dble(transA, transB, A, B)
  implicit none
  character(1), intent(in)    :: transA, transB
  real(8)     , intent(inout) :: A(:,:)
  real(8)     , intent(in)    :: B(:,:)

  integer :: n, m, k
  integer :: ldA, ldB, ldC
  real(8), parameter :: alpha = 1.d0
  real(8), parameter :: beta  = 0.d0
  integer, parameter :: xinc = 1
  integer, parameter :: yinc = 1
  real(8), allocatable :: C(:,:)
  character(CLEN_PROC), parameter :: msg_proc = &
    '*** @ '//trim(name_mod)//' SUBROUTINE BLAS_GEMM__out_dble ***'

  selectcase( transA )
  case( 'N', 'n' )
    n = size(A,1)
    k = size(A,2)
  case( 'T', 't' )
    n = size(A,2)
    k = size(A,1)
  case default
    write(0,*) trim(msg_proc)
    write(0,*) 'Invalid value in $transA: '//trim(transA)
    stop
  endselect
  selectcase( transB )
  case( 'N', 'n' )
    m = size(B,2)
    if( k /= size(B,1) )then
      write(0,*) trim(msg_proc)
      write(0,*) 'Incorrect shapes.'
      write(0,*) 'transA: '//trim(transA)//', shape(A):',shape(A)
      write(0,*) 'transB: '//trim(transB)//', shape(B):',shape(B)
      stop
    endif
  case( 'T', 't' )
    m = size(B,1)
    if( k /= size(B,2) )then
      write(0,*) trim(msg_proc)
      write(0,*) 'Incorrect shapes.'
      write(0,*) 'transA: '//trim(transA)//', shape(A):',shape(A)
      write(0,*) 'transB: '//trim(transB)//', shape(B):',shape(B)
      stop
    endif
  case default
    write(0,*) trim(msg_proc)
    write(0,*) 'Invalid value in $transB: '//trim(transB)
    stop
  endselect

  if( size(A,1) /= n .or. size(A,2) /= m )then
    write(0,*) trim(msg_proc)
    write(0,*) 'Incorrect shapes.'
    write(0,*) 'transA: '//trim(transA)//', shape(A):',shape(A)
    stop
  endif

  ldA = size(A,1)
  ldB = size(B,1)
  ldC = n

  allocate(C(n,m))

  call DGEMM(transA, transB, n, m, k, alpha, A, ldA, B, ldB, beta, C, ldC)
  call DCOPY(size(C), C, xinc, A, yinc)

  deallocate(C)
end subroutine BLAS_GEMM__inout1_dble
!===============================================================
! B <- op(A)op(B)  if which is 1
! op(A) = A    if transA is 'N' or 'n'
!       = A'   if transA is 'T' or 't'
! op(B) = B    if transB is 'N' or 'n'
!       = B'   if transB is 'T' or 't'
!===============================================================
subroutine BLAS_GEMM__inout2_dble(transA, transB, A, B)
  implicit none
  character(1), intent(in)    :: transA, transB
  real(8)     , intent(in)    :: A(:,:)
  real(8)     , intent(inout) :: B(:,:)

  real(8), allocatable :: C(:,:)
  integer :: n, m, k
  integer :: ldA, ldB, ldC
  real(8), parameter :: alpha = 1.d0
  real(8), parameter :: beta  = 0.d0
  integer, parameter :: xinc = 1
  integer, parameter :: yinc = 1
  character(CLEN_PROC), parameter :: msg_proc = &
    '*** @ '//trim(name_mod)//' SUBROUTINE BLAS_GEMM__out_dble ***'

  selectcase( transA )
  case( 'N', 'n' )
    n = size(A,1)
    k = size(A,2)
  case( 'T', 't' )
    n = size(A,2)
    k = size(A,1)
  case default
    write(0,*) trim(msg_proc)
    write(0,*) 'Invalid value in $transA: '//trim(transA)
    stop
  endselect
  selectcase( transB )
  case( 'N', 'n' )
    m = size(B,2)
    if( k /= size(B,1) )then
      write(0,*) trim(msg_proc)
      write(0,*) 'Incorrect shapes.'
      write(0,*) 'transA: '//trim(transA)//', shape(A):',shape(A)
      write(0,*) 'transB: '//trim(transB)//', shape(B):',shape(B)
      stop
    endif
  case( 'T', 't' )
    m = size(B,1)
    if( k /= size(B,2) )then
      write(0,*) trim(msg_proc)
      write(0,*) 'Incorrect shapes.'
      write(0,*) 'transA: '//trim(transA)//', shape(A):',shape(A)
      write(0,*) 'transB: '//trim(transB)//', shape(B):',shape(B)
      stop
    endif
  case default
    write(0,*) trim(msg_proc)
    write(0,*) 'Invalid value in $transB: '//trim(transB)
    stop
  endselect

  if( size(B,1) /= n .or. size(B,2) /= m )then
    write(0,*) trim(msg_proc)
    write(0,*) 'Incorrect shapes.'
    write(0,*) 'transB: '//trim(transB)//', shape(B):',shape(B)
    stop
  endif

  ldA = size(A,1)
  ldB = size(B,1)
  ldC = n

  allocate(C(n,m))

  call DGEMM(transA, transB, n, m, k, alpha, A, ldA, B, ldB, beta, C, ldC)
  call DCOPY(size(C), C, xinc, B, yinc)

  deallocate(C)
end subroutine BLAS_GEMM__inout2_dble
!===============================================================
!
!===============================================================
end module lib_math_linalg_blas
