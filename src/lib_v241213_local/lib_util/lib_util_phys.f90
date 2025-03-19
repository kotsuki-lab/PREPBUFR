module lib_util_phys
  use lib_const
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: calc_satWatVapPres
  public :: calc_watVapPres1
  public :: calc_watVapPres2
  public :: calc_specHumd
  public :: calc_relHumd
  public :: conv_RHerr_to_Qerr
  public :: conv_vtemp_to_temp
  public :: calc_T_tropo
  public :: calc_P_tropo
  !-------------------------------------------------------------
  ! Module Variables
  !-------------------------------------------------------------
  real(8), parameter :: EPS = 0.622d0  ! Ratio of molecular weight 
                                       ! of water vapor to that of dry air
  real(8), parameter :: GRAVITY = 9.80665d0  ! Standard gravity [m s-2]
  real(8), parameter :: R_DRY = 287.05d0  ! Air constant of dry air [m2 s-2 K-2]
  !-------------------------------------------------------------
contains
!===============================================================
! Compute saturated water vapor pressure Pws [hPa] over liquid water
! The Buck equation (Buck, 1981)
!===============================================================
real(8) function calc_satWatVapPres(T) result(Pws)
  implicit none
  real(8), intent(in) :: T  ! Temperature [K]

  real(8) :: Td  ! Temperature [degC]

  Td = T + DC_0K
  Pws = 6.1121d0 * exp((18.678d0-Td/234.5d0) * (Td / (257.14d0+Td)))
end function calc_satWatVapPres
!===============================================================
! Compute specific humidity Q [kg/kg]
!===============================================================
real(8) function calc_specHumd(P, Pw) result(Q)
  implicit none
  real(8), intent(in) :: P   ! Pressure [hPa]
  real(8), intent(in) :: Pw  ! Water vapor pressure [hPa]

  Q = eps*Pw / (P-(1.d0-eps)*Pw)
end function calc_specHumd
!===============================================================
! Compute water vapor pressure Pw [hPa]
!===============================================================
real(8) function calc_watVapPres1(P, Q) result(Pw)
  implicit none
  real(8), intent(in) :: P   ! Pressure [hPa]
  real(8), intent(in) :: Q   ! Specific humidity [kg/kg]

  Pw = P*Q / (eps + (1.d0-eps)*Q)
end function calc_watVapPres1
!===============================================================
! Compute water vapor pressure Pw [hPa]
!===============================================================
real(8) function calc_watVapPres2(Pws, RH) result(Pw)
  implicit none
  real(8), intent(in) :: Pws  ! Saturated water vapor pressure [hPa]
  real(8), intent(in) :: RH   ! Relative humidity [%]

  Pw = Pws * RH*1d-2
end function calc_watVapPres2
!===============================================================
! Compute relative humidity [%]
!===============================================================
real(8) function calc_relHumd(Pw, Pws) result(RH)
  implicit none
  real(8), intent(in) :: Pw   ! Water vapor pressure [hPa]
  real(8), intent(in) :: Pws  ! Saturated water vapor pressure [hPa]

  RH = Pw / Pws * 1d2
end function calc_relHumd
!===============================================================
! Convert RHerr [%] to Qerr [kg/kg]
!===============================================================
real(8) function conv_RHerr_to_Qerr(Q, P, T, RHerr) result(Qerr)
  implicit none
  real(8), intent(in) :: Q      ![kg/kg]
  real(8), intent(in) :: P      ![hPa]
  real(8), intent(in) :: T      ![K]
  real(8), intent(in) :: RHerr  ![%]

  real(8) :: Pws, Pw, Pwerr

  Pws = calc_satWatVapPres(T)  ![hPa]
  Pw  = calc_watVapPres1(P, Q)

  Pwerr = calc_watVapPres2(Pws, RHerr)  ! Linear
  Qerr = abs(calc_specHumd(P, Pw+Pwerr) &
              - calc_specHumd(P, Pw-Pwerr))*0.5d0  ![kg/kg] Non-linear
end function conv_RHerr_to_Qerr
!===============================================================
! Convert virtual temp. [K] to sensible temp. [K]
!===============================================================
real(8) function conv_vtemp_to_temp(Tv, Q) result(T)
  implicit none
  real(8), intent(in) :: Tv  ! Virtual temperature [K]
  real(8), intent(in) :: Q   ! Specific humidity [kg/kg]

  T = Tv / (1.d0 + (1.d0/EPS-1.d0)*Q)
end function conv_vtemp_to_temp
!===============================================================
! Calc. T in the troposphere
!===============================================================
real(8) function calc_T_tropo(z, zs, Ts, lapse) result(T)
  implicit none
  real(8), intent(in) :: z     ! Height [m] of target
  real(8), intent(in) :: zs    ! Height [m] of the base
  real(8), intent(in) :: Ts    ! Temp.[K] of the base
  real(8), intent(in) :: lapse ! Environmental lapse rate [K/m]

  T = Ts - lapse*(z-zs)
end function calc_T_tropo
!===============================================================
! Calc. P in the troposphere
!===============================================================
real(8) function calc_P_tropo(z, zs, Ts, Ps, lapse) result(P)
  implicit none
  real(8), intent(in) :: z     ! Height [m] of target
  real(8), intent(in) :: zs    ! Height [m] of the base
  real(8), intent(in) :: Ts    ! Temp.[K] of the base
  real(8), intent(in) :: Ps    ! P [hPa] of the base
  real(8), intent(in) :: lapse ! Environmental lapse rate [K/m]

  real(8), parameter :: g = GRAVITY
  real(8), parameter :: R = R_DRY

  real(8) :: Tm  ! Mean temp. [K] of the air in z-zs

  Tm = (calc_T_tropo(z, zs, Ts, lapse) + Ts) * 0.5d0
  P = Ps * exp(-g/(R*Tm)*(z-zs))
end function calc_P_tropo
!===============================================================
!
!===============================================================
end module lib_util_phys
