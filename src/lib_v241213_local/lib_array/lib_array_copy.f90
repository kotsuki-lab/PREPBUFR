module lib_array_copy
  use lib_array_realloc
  use lib_log
  implicit none
  !-------------------------------------------------------------
  private

  public :: copy
  !-------------------------------------------------------------
  interface copy
    module procedure copy_int1_1d
    module procedure copy_int1_2d
    module procedure copy_int1_3d
    module procedure copy_int2_1d
    module procedure copy_int2_2d
    module procedure copy_int2_3d
    module procedure copy_int4_1d
    module procedure copy_int4_2d
    module procedure copy_int4_3d
    module procedure copy_int8_1d
    module procedure copy_int8_2d
    module procedure copy_int8_3d
    module procedure copy_real_1d
    module procedure copy_real_2d
    module procedure copy_real_3d
    module procedure copy_dble_1d
    module procedure copy_dble_2d
    module procedure copy_dble_3d
  end interface
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine copy_int1_1d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndims = 1
  integer(byte), intent(in) :: arr_in(:)
  integer(byte), pointer    :: arr_out(:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_int1_1d
!===============================================================
!
!===============================================================
subroutine copy_int1_2d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndims = 2
  integer(byte), intent(in) :: arr_in(:,:)
  integer(byte), pointer    :: arr_out(:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_int1_2d
!===============================================================
!
!===============================================================
subroutine copy_int1_3d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 1
  integer, parameter :: ndims = 3
  integer(byte), intent(in) :: arr_in(:,:,:)
  integer(byte), pointer    :: arr_out(:,:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_int1_3d
!===============================================================
!
!===============================================================
subroutine copy_int2_1d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndims = 1
  integer(byte), intent(in) :: arr_in(:)
  integer(byte), pointer    :: arr_out(:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_int2_1d
!===============================================================
!
!===============================================================
subroutine copy_int2_2d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndims = 2
  integer(byte), intent(in) :: arr_in(:,:)
  integer(byte), pointer    :: arr_out(:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_int2_2d
!===============================================================
!
!===============================================================
subroutine copy_int2_3d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 2
  integer, parameter :: ndims = 3
  integer(byte), intent(in) :: arr_in(:,:,:)
  integer(byte), pointer    :: arr_out(:,:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_int2_3d
!===============================================================
!
!===============================================================
subroutine copy_int4_1d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndims = 1
  integer(byte), intent(in) :: arr_in(:)
  integer(byte), pointer    :: arr_out(:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_int4_1d
!===============================================================
!
!===============================================================
subroutine copy_int4_2d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndims = 2
  integer(byte), intent(in) :: arr_in(:,:)
  integer(byte), pointer    :: arr_out(:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_int4_2d
!===============================================================
!
!===============================================================
subroutine copy_int4_3d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndims = 3
  integer(byte), intent(in) :: arr_in(:,:,:)
  integer(byte), pointer    :: arr_out(:,:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_int4_3d
!===============================================================
!
!===============================================================
subroutine copy_int8_1d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndims = 1
  integer(byte), intent(in) :: arr_in(:)
  integer(byte), pointer    :: arr_out(:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_int8_1d
!===============================================================
!
!===============================================================
subroutine copy_int8_2d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndims = 2
  integer(byte), intent(in) :: arr_in(:,:)
  integer(byte), pointer    :: arr_out(:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_int8_2d
!===============================================================
!
!===============================================================
subroutine copy_int8_3d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndims = 3
  integer(byte), intent(in) :: arr_in(:,:,:)
  integer(byte), pointer    :: arr_out(:,:,:)
  integer(8)   , intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_int8_3d
!===============================================================
!
!===============================================================
subroutine copy_real_1d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndims = 1
  real(byte), intent(in) :: arr_in(:)
  real(byte), pointer    :: arr_out(:)
  integer(8), intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_real_1d
!===============================================================
!
!===============================================================
subroutine copy_real_2d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndims = 2
  real(byte), intent(in) :: arr_in(:,:)
  real(byte), pointer    :: arr_out(:,:)
  integer(8), intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_real_2d
!===============================================================
!
!===============================================================
subroutine copy_real_3d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 4
  integer, parameter :: ndims = 3
  real(byte), intent(in) :: arr_in(:,:,:)
  real(byte), pointer    :: arr_out(:,:,:)
  integer(8), intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_real_3d
!===============================================================
!
!===============================================================
subroutine copy_dble_1d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndims = 1
  real(byte), intent(in) :: arr_in(:)
  real(byte), pointer    :: arr_out(:)
  integer(8), intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_dble_1d
!===============================================================
!
!===============================================================
subroutine copy_dble_2d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndims = 2
  real(byte), intent(in) :: arr_in(:,:)
  real(byte), pointer    :: arr_out(:,:)
  integer(8), intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_dble_2d
!===============================================================
!
!===============================================================
subroutine copy_dble_3d(arr_in, arr_out, l)
  implicit none
  integer, parameter :: byte = 8
  integer, parameter :: ndims = 3
  real(byte), intent(in) :: arr_in(:,:,:)
  real(byte), pointer    :: arr_out(:,:,:)
  integer(8), intent(in), optional :: l(ndims)
  integer(8) :: l_(ndims)
  
  l_ = 1_8
  if( present(l) ) l_ = l

  call realloc(arr_out, l_, l_+ubound(arr_in)-lbound(arr_in), clear=.true.)
  arr_out = arr_in
end subroutine copy_dble_3d
!===============================================================
!
!===============================================================
end module lib_array_copy
