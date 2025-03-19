module lib_math_linalg_lapack
  implicit none
  private
  !=============================================================
  ! Public Procedures
  !=============================================================
  public :: calc_inv

  public :: solve_eig_sym
  !=============================================================
  ! Interfaces
  !=============================================================
  interface calc_inv
    module procedure calc_inv__inout
    module procedure calc_inv__out
  end interface
!---------------------------------------------------------------
contains
!===============================================================
! Compute inverse matrix
!
! LAPACK: ILAENV, DGETRF, DGETRI
!===============================================================
subroutine calc_inv__inout(A, info)
  implicit none
  real(8), intent(inout) :: A(:,:)  !(n,n)
  integer, intent(out)   :: info

  integer, allocatable :: ipiv(:) !(n)
  real(8), allocatable :: work(:)
  integer :: n, nb, lwork

  integer :: ilaenv

  n = size(A,1)

  nb = ILAENV(1, 'DGETRI', ' ', n, -1, -1, -1)
  lwork = n * nb
  allocate(work(lwork))
  allocate(ipiv(n))

  ! Factorize A
  call DGETRF(n, n, A, n, ipiv, info)
  if( info /= 0 ) return

  ! Compute inverse of A
  call DGETRI(n, A, n, ipiv, work, lwork, info)
end subroutine calc_inv__inout
!===============================================================
!
!===============================================================
subroutine calc_inv__out(A, B, info)
  implicit none
  real(8), intent(in)  :: A(:,:)  !(n,n)
  real(8), intent(out) :: B(:,:)  !(n,n)
  integer, intent(out) :: info

  integer, allocatable :: ipiv(:) !(n)
  real(8), allocatable :: work(:)
  integer :: n, nb, lwork

  integer :: ilaenv

  n = size(A,1)

  nb = ILAENV(1, 'DGETRI', ' ', n, -1, -1, -1)
  lwork = n * nb
  allocate(work(lwork))
  allocate(ipiv(n))

  B = A

  ! Factorize A
  call DGETRF(n, n, B, n, ipiv, info)
  if( info /= 0 ) return

  ! Compute inverse of A
  call DGETRI(n, B, n, ipiv, work, lwork, info)
end subroutine calc_inv__out
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Solve the symmetric eigenvalue problem
!
! LAPACK: DSYEVR
! Reference: https://www.nag-j.co.jp/lapack/dsyevr.htm
!===============================================================
subroutine solve_eig_sym(a, m, w, z, info)
  implicit none
  real(8), intent(in)  :: a(:,:)  !(n,n)
  integer, intent(out) :: m
  real(8), intent(out) :: w(:)    !(n)
  real(8), intent(out) :: z(:,:)  !(n,n)
  integer, intent(out) :: info

  integer, parameter :: nb = 64
  character(1), parameter :: jobz = 'V'
  character(1), parameter :: range = 'I'
  character(1), parameter :: uplo = 'U'
  real(8), allocatable :: a_(:,:)
  real(8) :: abstol
  integer :: n, lda, ldz
  integer :: liwork, lwork
  integer :: il, iu
  real(8) :: vl, vu
  real(8), allocatable :: work(:)
  integer, allocatable :: iwork(:)
  integer, allocatable :: isuppz(:)
  real(8) :: dummy(1) !dummy of work
  integer :: idum(1)  !dummy of iwork

  n = size(a,1)
  allocate(a_(n,n))
  a_ = a

  il = 1
  iu = n

  lda = n
  ldz = n
  m = n
  allocate(isuppz(2*m))

  ! Use routine workspace query to get optimal workspace.
  lwork = -1
  liwork = -1
  call DSYEVR(jobz, range, uplo, &
              n, a_, lda, vl, vu, il, iu, abstol, &
              m, w, z, ldz, isuppz, &
              dummy, lwork, idum, liwork, info)

  ! Make sure that there is enough workspace for block size nb.
  lwork = max((nb+6)*n, nint(dummy(1)))
  liwork = max(10*n, idum(1))
  allocate(work(lwork), iwork(liwork))

  ! Set the absolute error tolerance for eigenvalue. With ABSTOL
  ! set to zero, the default value is used instead.
  abstol = 0.d0

  ! Solve the symmetric eigenvalue problem.
  call DSYEVR(jobz, range, uplo, &
              n, a_, lda, vl, vu, il, iu, abstol, &
              m, w, z, ldz, isuppz, &
              work, lwork, iwork, liwork, info)

  if( m < n )then
    w(m+1:n) = 0.d0
    z(:,m+1:n) = 0.d0
  endif
end subroutine solve_eig_sym
!===============================================================
!
!===============================================================
end module lib_math_linalg_lapack
