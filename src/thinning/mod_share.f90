module mod_share
  use lib_const
  use lib_log
  use lib_io
  use lib_math
  use def_const
  use def_type
  implicit none
  public
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: set_pb
  public :: set_vidx
  public :: set_model_grid
  public :: set_logplv
  !-------------------------------------------------------------
  ! Parameters in Practice (dependant on MODULE mod_io)
  !-------------------------------------------------------------
  integer :: nhdr, nobs, noer, nqmk, npcd, nrcd
  integer :: nvar

  integer :: vidx_P
  integer :: vidx_Q
  integer :: vidx_T
  integer :: vidx_Tv  ! ext.
  integer :: vidx_U
  integer :: vidx_V
  integer :: vidx_Z
  !-------------------------------------------------------------
  ! Parameters in Practice (dependant on MODULE mod_io)
  !-------------------------------------------------------------
  integer :: nlon, nlat, nlev
  integer :: nlay_all
  real(8), allocatable :: lon(:), lat(:), plv(:)
  real(8), allocatable :: lonb(:), latb(:)
  real(8), allocatable :: logplv(:)
  type(file_) :: f_elv
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_pb()
  use mod_pb, only: &
        get_num_key_all, &
        get_nvar
  implicit none
  !-------------------------------------------------------------
  ! Get $nhdr, $nobs etc.
  !-------------------------------------------------------------
  call get_num_key_all(nhdr, nobs, noer, nqmk, npcd, nrcd)
  nvar = get_nvar(nobs)
end subroutine set_pb
!===============================================================
!
!===============================================================
subroutine set_vidx()
  use mod_base, only: &
        var_to_idx_obs
  implicit none

  vidx_P = var_to_idx_obs(VAR_P, nobs)
  vidx_Z = var_to_idx_obs(VAR_Z, nobs)
  vidx_T = var_to_idx_obs(VAR_T, nobs)
  vidx_Q = var_to_idx_obs(VAR_Q, nobs)
  vidx_U = var_to_idx_obs(VAR_U, nobs)
  vidx_V = var_to_idx_obs(VAR_V, nobs)

  vidx_Tv = var_to_idx_obs(VAR_TV, nobs)
end subroutine set_vidx
!===============================================================
!
!===============================================================
subroutine set_model_grid()
  use common_XXXXXX, only: &
        nlon_ => nlon, &
        nlat_ => nlat, &
        nlev_ => nlev, &
        lon_ => lon, &
        lat_ => lat, &
        plv_ => plv
  implicit none
  integer :: ilon, ilat
  integer :: ilon_next

  call echo(code%bgn, 'set_model_grid')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nlon = nlon_
  nlat = nlat_
  nlev = nlev_
  allocate(lon(nlon))
  allocate(lat(nlat))
  allocate(plv(nlev))
  lon(:) = lon_(:)
  lat(:) = lat_(:)
  plv(:) = plv_(:)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  nlay_all = nlon * nlat * nlev

  allocate(lonb(0:nlon))
  allocate(latb(0:nlat))

  do ilon = 1, nlon
    ilon_next = ilon + 1
    if( ilon_next > nlon ) ilon_next = 1
    if( abs(lon(ilon_next) - lon(ilon)) < 1.8d2 )then
      lonb(ilon) = (lon(ilon) + lon(ilon_next))*0.5d0
    else
      lonb(ilon) = (lon(ilon) + lon(ilon_next) + 3.6d2) * 0.5d0
    endif
  enddo
  lonb(0) = lonb(nlon)
  do ilon = 0, nlon
    if( lonb(ilon) < 0.d0 ) call add(lonb(ilon), 3.6d2)
    if( lonb(ilon) > 3.6d2 ) call add(lonb(ilon), -3.6d2)
  enddo
  call edbg('lonb: '//str(lonb(:2),'f8.3',', ')//&
           ', ..., '//str(lonb(nlon-2:),'f8.3',', '))

  do ilat = 1, nlat-1
    latb(ilat) = (lat(ilat) + lat(ilat+1)) * 0.5d0
  enddo
  latb(0)    = -90.d0
  latb(nlat) =  90.d0
  call edbg('latb: '//str(latb(:2),'f8.3',', ')//&
           ', ..., '//str(latb(nlat-2:),'f8.3',', '))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  f_elv = file('orography_x64y32.grd', DTYPE_REAL, ENDIAN_BIG, 1)
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine set_model_grid
!===============================================================
!
!===============================================================
subroutine set_logplv()
  implicit none

  allocate(logplv(nlev))
  logplv(:) = log(plv(:))
end subroutine set_logplv
!===============================================================
!
!===============================================================
end module mod_share
