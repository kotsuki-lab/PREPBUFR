module lib_util_unit
  use lib_const
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: degC_to_K
  public :: K_to_degC
!---------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
real(8) function degC_to_K(vin) result(vout)
  implicit none
  real(8), intent(in) :: vin  ![degC]

  vout = vin - DC_0K
end function degC_to_K
!===============================================================
!
!===============================================================
real(8) function K_to_degC(vin) result(vout)
  implicit none
  real(8), intent(in) :: vin  ![K]

  vout = vin + DC_0K
end function K_to_degC
!===============================================================
!
!===============================================================
end module lib_util_unit
