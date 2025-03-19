module lib_math_sphere
  use lib_const
  use lib_base
  use lib_log
  use lib_util, only: &
    str_arctyp_long, &
    str_arc_rel_lat, &
    str_convex_long
  use lib_math_linalg_util, only: &
    calc_cross_product
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: set_modvar_lib_math_sphere

  public :: which_is_western
  public :: western
  public :: eastern

  public :: londiff_rad
  public :: londiff_deg

  public :: dir_lon

  public :: bboxes_intersect
  public :: included_in_bbox

  public :: conv_spherical_to_cartesian_rad
  public :: conv_spherical_to_cartesian_deg
  public :: conv_cartesian_to_spherical_rad
  public :: conv_cartesian_to_spherical_deg

  public :: calc_coefs_large_arc

  public :: calc_intersection_sphere_normal_normal
  public :: calc_intersection_sphere_normal_meridian
  public :: calc_intersection_sphere_normal_parallel1
  public :: calc_intersection_sphere_normal_parallel2
  public :: calc_intersection_sphere_normal_parallel3

  public :: calc_lat_range_large_arc

  public :: calc_lon_range_shared

  public :: area_sphere_rect
  public :: area_ellips_rect

  public :: area_sphere_polarrect

  public :: area_sphere_tri

  public :: area_sphere_righttri_south_bottom
  public :: area_sphere_righttri_north_bottom

  public :: area_sphere_polartri

  public :: area_sphere_polygon

  public :: area_sphere_intersection_polygon_polygon
  public :: area_sphere_intersection_latlon_polygon

  public :: dist_sphere
  !-------------------------------------------------------------
  ! Interfaces
  !-------------------------------------------------------------
  interface londiff_rad
    module procedure londiff_rad_0d
    module procedure londiff_rad_1d
  end interface

  interface londiff_deg
    module procedure londiff_deg_0d
    module procedure londiff_deg_1d
  end interface

  interface conv_spherical_to_cartesian_rad
    module procedure conv_spherical_to_cartesian_rad_0d_miss_unspecified
    module procedure conv_spherical_to_cartesian_rad_0d_miss_specified
    module procedure conv_spherical_to_cartesian_rad_1d_miss_unspecified
    module procedure conv_spherical_to_cartesian_rad_1d_miss_specified
    module procedure conv_spherical_to_cartesian_rad_2d_miss_unspecified
    module procedure conv_spherical_to_cartesian_rad_2d_miss_specified
  end interface

  interface conv_spherical_to_cartesian_deg
    module procedure conv_spherical_to_cartesian_deg_0d_miss_unspecified
    module procedure conv_spherical_to_cartesian_deg_0d_miss_specified
    module procedure conv_spherical_to_cartesian_deg_1d_miss_unspecified
    module procedure conv_spherical_to_cartesian_deg_1d_miss_specified
    module procedure conv_spherical_to_cartesian_deg_2d_miss_unspecified
    module procedure conv_spherical_to_cartesian_deg_2d_miss_specified
  end interface

  interface conv_cartesian_to_spherical_rad
    module procedure conv_cartesian_to_spherical_rad_0d_miss_unspecified
    module procedure conv_cartesian_to_spherical_rad_0d_miss_specified
    module procedure conv_cartesian_to_spherical_rad_1d_miss_unspecified
    module procedure conv_cartesian_to_spherical_rad_1d_miss_specified
    module procedure conv_cartesian_to_spherical_rad_2d_miss_unspecified
    module procedure conv_cartesian_to_spherical_rad_2d_miss_specified
  end interface

  interface conv_cartesian_to_spherical_deg
    module procedure conv_cartesian_to_spherical_deg_0d_miss_unspecified
    module procedure conv_cartesian_to_spherical_deg_0d_miss_specified
    module procedure conv_cartesian_to_spherical_deg_1d_miss_unspecified
    module procedure conv_cartesian_to_spherical_deg_1d_miss_specified
    module procedure conv_cartesian_to_spherical_deg_2d_miss_unspecified
    module procedure conv_cartesian_to_spherical_deg_2d_miss_specified
  end interface

  interface calc_intersection_sphere_normal_normal
    module procedure calc_intersection_sphere_normal_normal_confirmed
  end interface

  interface calc_intersection_sphere_normal_meridian
    module procedure calc_intersection_sphere_normal_meridian_0d
    module procedure calc_intersection_sphere_normal_meridian_1d
  end interface

  interface calc_coefs_large_arc
    module procedure calc_coefs_large_arc_spherical
    module procedure calc_coefs_large_arc_cartesian
  end interface

  interface area_sphere_rect
    module procedure area_sphere_rect_lat0d
    module procedure area_sphere_rect_lat1d
  end interface

  interface area_ellips_rect
    module procedure area_ellips_rect_lat0d
    module procedure area_ellips_rect_lat1d
  end interface

  interface area_sphere_polartri
    module procedure area_sphere_polartri_spherical
  end interface

  interface area_sphere_polygon
    module procedure area_sphere_polygon_spherical
    module procedure area_sphere_polygon_cartesian
  end interface

  interface dist_sphere
    module procedure dist_sphere_0d
  end interface
  !-------------------------------------------------------------
  logical, save :: debug = .false.
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine set_modvar_lib_math_sphere(debug)
  implicit none
  logical, intent(in), optional :: debug

  if( present(debug) ) call set_var_debug(debug)
end subroutine set_modvar_lib_math_sphere
!===============================================================
!
!===============================================================
subroutine set_var_debug(val)
  implicit none
  logical, intent(in) :: val

  debug = val
end subroutine set_var_debug
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Return
!  0 when lon1 == lon2,
!  1 when lon1 is western of lon2,
!  2 when lon2 is western of lon1
!===============================================================
integer function which_is_western(lon1, lon2) result(res)
  implicit none
  real(8), intent(in) :: lon1, lon2

  if( lon1 == lon2 )then
    res = 0
  else
    if( abs(lon1 - lon2) < rad_180deg .eqv. lon1 < lon2 )then
      res = 1
    else
      res = 2
    endif
  endif
end function which_is_western
!===============================================================
!
!===============================================================
real(8) function western(lon1, lon2)
  implicit none
  real(8), intent(in) :: lon1, lon2

  if( abs(lon1-lon2) < rad_180deg )then
    western = min(lon1, lon2)
  else
    western = max(lon1, lon2)
  endif
end function western
!===============================================================
!
!===============================================================
real(8) function eastern(lon1, lon2)
  implicit none
  real(8), intent(in) :: lon1, lon2

  if( abs(lon1-lon2) < rad_180deg )then
    eastern = max(lon1, lon2)
  else
    eastern = min(lon1, lon2)
  endif
end function eastern
!===============================================================
!
!===============================================================
subroutine get_stat_lon_between(lon, west, east, stat)
  implicit none
  real(8)   , intent(in) :: lon
  real(8)   , intent(in) :: west, east
  integer(1), intent(out) :: stat

  if( lon == west .or. abs(west-lon) == rad_360deg )then
    stat = stat_lon_between_west
  elseif( lon == east .or. abs(east-lon) == rad_360deg )then
    stat = stat_lon_between_east
  else
    if( abs(east-west) < rad_180deg )then
      if( west < lon .and. lon < east )then
        stat = stat_lon_between_inside
      else
        stat = stat_lon_between_outside
      endif
    else
      if( west < lon .or. lon < east )then
        stat = stat_lon_between_inside
      else
        stat = stat_lon_between_outside
      endif
    endif
  endif
end subroutine get_stat_lon_between
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
real(8) function londiff_rad_0d(lon1, lon2) result(lon)
  implicit none
  real(8), intent(in) :: lon1, lon2

  lon = abs(lon1 - lon2)
  if( lon > rad_180deg ) lon = rad_360deg - lon
end function londiff_rad_0d
!===============================================================
!
!===============================================================
function londiff_rad_1d(lon1, lon2) result(lon)
  implicit none
  real(8), intent(in) :: lon1(:), lon2(:)
  real(8)             :: lon(size(lon1))

  integer :: i

  do i = 1, size(lon1)
    lon(i) = londiff_rad_0d(lon1(i), lon2(i))
  enddo
end function londiff_rad_1d
!===============================================================
!
!===============================================================
real(8) function londiff_deg_0d(lon1, lon2) result(lon)
  implicit none
  real(8), intent(in) :: lon1, lon2

  lon = abs(lon1 - lon2)
  if( lon > 1.8d2 ) lon = 3.6d2 - lon
end function londiff_deg_0d
!===============================================================
!
!===============================================================
function londiff_deg_1d(lon1, lon2) result(lon)
  implicit none
  real(8), intent(in) :: lon1(:), lon2(:)
  real(8)             :: lon(size(lon1))

  integer :: i

  do i = 1, size(lon1)
    lon(i) = londiff_deg_0d(lon1(i), lon2(i))
  enddo
end function londiff_deg_1d
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
integer function dir_lon(lon1, lon2) result(sgn)
  implicit none
  real(8), intent(in) :: lon1, lon2

  if( abs(lon1-lon2) < rad_180deg )then
    if( lon1 < lon2 )then
      sgn = 1
    else
      sgn = -1
    endif
  else
    if( lon1 > lon2 )then
      sgn = 1
    else
      sgn = -1
    endif
  endif
end function dir_lon
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
logical function bboxes_intersect(&
    ssouth, snorth, swest, seast, slon0, &
    tsouth, tnorth, twest, teast, tlon0) &
  result(res)
  implicit none
  real(8), intent(in) :: ssouth, snorth, swest, seast, &
                         tsouth, tnorth, twest, teast  ![rad]
  logical, intent(in) :: slon0, tlon0

  if( snorth <= tsouth .or. ssouth >= tnorth )then
    res = .false.
  elseif( .not. slon0 .and. .not. tlon0 )then
    res = ( .not. (swest >= teast .or. seast <= twest) )
  elseif( .not. tlon0 )then
    res = ( swest < teast .or. twest < seast )
  elseif( .not. slon0 )then
    res = ( twest < seast .or. swest < teast )
  else
    res = .true.
  endif
end function bboxes_intersect
!===============================================================
!
!===============================================================
logical function included_in_bbox(&
    west, east, south, north, lon0, plon, plat) &
  result(res)
  implicit none
  real(8), intent(in) :: west, east, south, north
  real(8), intent(in) :: plon, plat
  logical, intent(in) :: lon0

  if( plat < south .or. plat > north )then
    res = .false.
    return
  endif

  res = ( (lon0 .and. (west < plon .or. plon < east)) .or. &
          (.not. lon0 .and. (west < plon .and. plon < east)) )
end function included_in_bbox
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
subroutine conv_spherical_to_cartesian_deg_0d_miss_unspecified(lon, lat, x, y, z)
  implicit none
  real(8), intent(in)  :: lon, lat
  real(8), intent(out) :: x, y, z

  if( abs(lat) == 9.d1 )then
    x = 0.d0
    y = 0.d0
    z = sign(1.d0, lat)
  else
    x = cos(lat*d2r) * cos(lon*d2r)
    y = cos(lat*d2r) * sin(lon*d2r)
    z = sin(lat*d2r)
  endif
end subroutine conv_spherical_to_cartesian_deg_0d_miss_unspecified
!===============================================================
!
!===============================================================
subroutine conv_spherical_to_cartesian_deg_0d_miss_specified(lon, lat, x, y, z, miss_s, miss_c)
  implicit none
  real(8), intent(in)  :: lon, lat
  real(8), intent(out) :: x, y, z
  real(8), intent(in)  :: miss_s, miss_c

  if( lat == miss_s )then
    x = miss_c
    y = miss_c
    z = miss_c
  else
    call conv_spherical_to_cartesian_deg_0d_miss_unspecified(lon, lat, x, y, z)
  endif
end subroutine conv_spherical_to_cartesian_deg_0d_miss_specified
!===============================================================
!
!===============================================================
subroutine conv_spherical_to_cartesian_deg_1d_miss_unspecified(lon, lat, x, y, z)
  implicit none
  real(8), intent(in)  :: lon(:), lat(:)
  real(8), intent(out) :: x(:), y(:), z(:)

  integer :: i

  do i = 1, size(lon)
    call conv_spherical_to_cartesian_deg_0d_miss_unspecified(lon(i), lat(i), x(i), y(i), z(i))
  enddo
end subroutine conv_spherical_to_cartesian_deg_1d_miss_unspecified
!===============================================================
!
!===============================================================
subroutine conv_spherical_to_cartesian_deg_1d_miss_specified(lon, lat, x, y, z, miss_s, miss_c)
  implicit none
  real(8), intent(in)  :: lon(:), lat(:)
  real(8), intent(out) :: x(:), y(:), z(:)
  real(8), intent(in)  :: miss_s, miss_c

  integer :: i

  do i = 1, size(lon)
    if( lat(i) == miss_s )then
      x(i) = miss_c
      y(i) = miss_c
      z(i) = miss_c
    else
      call conv_spherical_to_cartesian_deg_0d_miss_unspecified(lon(i), lat(i), x(i), y(i), z(i))
    endif
  enddo
end subroutine conv_spherical_to_cartesian_deg_1d_miss_specified
!===============================================================
!
!===============================================================
subroutine conv_spherical_to_cartesian_deg_2d_miss_unspecified(lon, lat, x, y, z)
  implicit none
  real(8), intent(in)  :: lon(:,:), lat(:,:)
  real(8), intent(out) :: x(:,:), y(:,:), z(:,:)

  integer :: i, j

  do j = 1, size(lon,2)
    do i = 1, size(lon,1)
      call conv_spherical_to_cartesian_deg_0d_miss_unspecified(&
             lon(i,j), lat(i,j), x(i,j), y(i,j), z(i,j))
    enddo
  enddo
end subroutine conv_spherical_to_cartesian_deg_2d_miss_unspecified
!===============================================================
!
!===============================================================
subroutine conv_spherical_to_cartesian_deg_2d_miss_specified(lon, lat, x, y, z, miss_s, miss_c)
  implicit none
  real(8), intent(in)  :: lon(:,:), lat(:,:)
  real(8), intent(out) :: x(:,:), y(:,:), z(:,:)
  real(8), intent(in)  :: miss_s, miss_c

  integer :: i, j

  do j = 1, size(lon,2)
    do i = 1, size(lon,1)
      if( lat(i,j) == miss_s )then
        x(i,j) = miss_c
        y(i,j) = miss_c
        z(i,j) = miss_c
      else
        call conv_spherical_to_cartesian_deg_0d_miss_unspecified(&
               lon(i,j), lat(i,j), x(i,j), y(i,j), z(i,j))
      endif
    enddo
  enddo
end subroutine conv_spherical_to_cartesian_deg_2d_miss_specified
!===============================================================
!
!===============================================================
subroutine conv_spherical_to_cartesian_rad_0d_miss_unspecified(lon, lat, x, y, z)
  implicit none
  real(8), intent(in)  :: lon, lat
  real(8), intent(out) :: x, y, z

  if( abs(lat) == rad_90deg )then
    x = 0.d0
    y = 0.d0
    z = sign(1.d0, lat)
  else
    x = cos(lat) * cos(lon)
    y = cos(lat) * sin(lon)
    z = sin(lat)
  endif
end subroutine conv_spherical_to_cartesian_rad_0d_miss_unspecified
!===============================================================
!
!===============================================================
subroutine conv_spherical_to_cartesian_rad_0d_miss_specified(lon, lat, x, y, z, miss_s, miss_c)
  implicit none
  real(8), intent(in)  :: lon, lat
  real(8), intent(out) :: x, y, z
  real(8), intent(in)  :: miss_s, miss_c

  if( lat == miss_s )then
    x = miss_c
    y = miss_c
    z = miss_c
  else
    call conv_spherical_to_cartesian_rad_0d_miss_unspecified(lon, lat, x, y, z)
  endif
end subroutine conv_spherical_to_cartesian_rad_0d_miss_specified
!===============================================================
!
!===============================================================
subroutine conv_spherical_to_cartesian_rad_1d_miss_unspecified(lon, lat, x, y, z)
  implicit none
  real(8), intent(in)  :: lon(:), lat(:)
  real(8), intent(out) :: x(:), y(:), z(:)

  integer :: i

  do i = 1, size(lon)
    call conv_spherical_to_cartesian_rad_0d_miss_unspecified(lon(i), lat(i), x(i), y(i), z(i))
  enddo
end subroutine conv_spherical_to_cartesian_rad_1d_miss_unspecified
!===============================================================
!
!===============================================================
subroutine conv_spherical_to_cartesian_rad_1d_miss_specified(lon, lat, x, y, z, miss_s, miss_c)
  implicit none
  real(8), intent(in)  :: lon(:), lat(:)
  real(8), intent(out) :: x(:), y(:), z(:)
  real(8), intent(in)  :: miss_s, miss_c

  integer :: i

  do i = 1, size(lon)
    if( lat(i) == miss_s )then
      x(i) = miss_c
      y(i) = miss_c
      z(i) = miss_c
    else
      call conv_spherical_to_cartesian_rad_0d_miss_unspecified(lon(i), lat(i), x(i), y(i), z(i))
    endif
  enddo
end subroutine conv_spherical_to_cartesian_rad_1d_miss_specified
!===============================================================
!
!===============================================================
subroutine conv_spherical_to_cartesian_rad_2d_miss_unspecified(lon, lat, x, y, z)
  implicit none
  real(8), intent(in)  :: lon(:,:), lat(:,:)
  real(8), intent(out) :: x(:,:), y(:,:), z(:,:)

  integer :: i, j

  do j = 1, size(lon,2)
    do i = 1, size(lon,1)
      call conv_spherical_to_cartesian_rad_0d_miss_unspecified(&
             lon(i,j), lat(i,j), x(i,j), y(i,j), z(i,j))
    enddo
  enddo
end subroutine conv_spherical_to_cartesian_rad_2d_miss_unspecified
!===============================================================
!
!===============================================================
subroutine conv_spherical_to_cartesian_rad_2d_miss_specified(lon, lat, x, y, z, miss_s, miss_c)
  implicit none
  real(8), intent(in)  :: lon(:,:), lat(:,:)
  real(8), intent(out) :: x(:,:), y(:,:), z(:,:)
  real(8), intent(in)  :: miss_s, miss_c

  integer :: i, j

  do j = 1, size(lon,2)
    do i = 1, size(lon,1)
      if( lat(i,j) == miss_s )then
        x(i,j) = miss_c
        y(i,j) = miss_c
        z(i,j) = miss_c
      else
        call conv_spherical_to_cartesian_rad_0d_miss_unspecified(&
               lon(i,j), lat(i,j), x(i,j), y(i,j), z(i,j))
      endif
    enddo
  enddo
end subroutine conv_spherical_to_cartesian_rad_2d_miss_specified
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
subroutine conv_cartesian_to_spherical_deg_0d_miss_unspecified(x, y, z, lon, lat)
  implicit none
  real(8), intent(in)  :: x, y, z
  real(8), intent(out) :: lon, lat

  if( x == 0.d0 .and. y == 0.d0 )then
    lat = sign(9.d1, z)
    lon = 0.d0
  else
    lat = asin( z / sqrt(x**2+y**2+z**2) ) * r2d
    lon = atan2( y, x ) * r2d
    if( lon < 0.d0 ) lon = lon + 3.6d2
  endif
end subroutine conv_cartesian_to_spherical_deg_0d_miss_unspecified
!===============================================================
!
!===============================================================
subroutine conv_cartesian_to_spherical_deg_0d_miss_specified(x, y, z, lon, lat, miss_c, miss_s)
  implicit none
  real(8), intent(in)  :: x, y, z
  real(8), intent(out) :: lon, lat
  real(8), intent(in)  :: miss_c  ! missing value of cartesian
  real(8), intent(in)  :: miss_s  ! missing value of spherical

  if( x == miss_c )then
    lon = miss_s
    lat = miss_s
  else
    call conv_cartesian_to_spherical_deg_0d_miss_unspecified(x, y, z, lon, lat)
  endif
end subroutine conv_cartesian_to_spherical_deg_0d_miss_specified
!===============================================================
!
!===============================================================
subroutine conv_cartesian_to_spherical_deg_1d_miss_unspecified(x, y, z, lon, lat)
  implicit none
  real(8), intent(in)  :: x(:), y(:), z(:)
  real(8), intent(out) :: lon(:), lat(:)

  integer :: i

  do i = 1, size(x)
    call conv_cartesian_to_spherical_deg_0d_miss_unspecified(&
           x(i), y(i), z(i), lon(i), lat(i))
  enddo
end subroutine conv_cartesian_to_spherical_deg_1d_miss_unspecified
!===============================================================
!
!===============================================================
subroutine conv_cartesian_to_spherical_deg_1d_miss_specified(x, y, z, lon, lat, miss_c, miss_s)
  implicit none
  real(8), intent(in)  :: x(:), y(:), z(:)
  real(8), intent(out) :: lon(:), lat(:)
  real(8), intent(in)  :: miss_c  ! missing value of cartesian
  real(8), intent(in)  :: miss_s  ! missing value of spherical

  integer :: i

  do i = 1, size(x)
    if( x(i) == miss_c )then
      lon(i) = miss_s
      lat(i) = miss_s
    else
      call conv_cartesian_to_spherical_deg_0d_miss_unspecified(&
               x(i), y(i), z(i), lon(i), lat(i))
    endif
  enddo
end subroutine conv_cartesian_to_spherical_deg_1d_miss_specified
!===============================================================
!
!===============================================================
subroutine conv_cartesian_to_spherical_deg_2d_miss_unspecified(x, y, z, lon, lat)
  implicit none
  real(8), intent(in)  :: x(:,:), y(:,:), z(:,:)
  real(8), intent(out) :: lon(:,:), lat(:,:)

  integer :: i, j

  do j = 1, size(x,2)
    do i = 1, size(x,1)
      call conv_cartesian_to_spherical_deg_0d_miss_unspecified(&
             x(i,j), y(i,j), z(i,j), lon(i,j), lat(i,j))
    enddo
  enddo
end subroutine conv_cartesian_to_spherical_deg_2d_miss_unspecified
!===============================================================
!
!===============================================================
subroutine conv_cartesian_to_spherical_deg_2d_miss_specified(x, y, z, lon, lat, miss_c, miss_s)
  implicit none
  real(8), intent(in)  :: x(:,:), y(:,:), z(:,:)
  real(8), intent(out) :: lon(:,:), lat(:,:)
  real(8), intent(in)  :: miss_c  ! missing value of cartesian
  real(8), intent(in)  :: miss_s  ! missing value of spherical

  integer :: i, j

  do j = 1, size(x,2)
    do i = 1, size(x,1)
      if( x(i,j) == miss_c )then
        lon(i,j) = miss_s
        lat(i,j) = miss_s
      else
        call conv_cartesian_to_spherical_deg_0d_miss_unspecified(&
                 x(i,j), y(i,j), z(i,j), lon(i,j), lat(i,j))
      endif
    enddo
  enddo
end subroutine conv_cartesian_to_spherical_deg_2d_miss_specified
!===============================================================
!
!===============================================================
subroutine conv_cartesian_to_spherical_rad_0d_miss_unspecified(x, y, z, lon, lat)
  implicit none
  real(8), intent(in)  :: x, y, z
  real(8), intent(out) :: lon, lat

  real(8) :: r

  r = sqrt(x**2 + y**2 + z**2)
if( abs(z) > r .or. r == 0.d0 )then
  call eerr('x: '//str(x,'es20.13')//&
          '\ny: '//str(y,'es20.13')//&
          '\nz: '//str(z,'es20.13')//&
          '\nr: '//str(r,'es20.13'))
endif
  lon = atan2(y,x)
  lat = asin(z/r)

  if( x == 0.d0 .and. y == 0.d0 )then
    lat = sign(rad_90deg, z)
    lon = 0.d0
  else
    lat = asin( z / sqrt(x**2+y**2+z**2) )
    lon = atan2( y, x )
    if( lon < rad_0deg ) lon = lon + rad_360deg
  endif
end subroutine conv_cartesian_to_spherical_rad_0d_miss_unspecified
!===============================================================
!
!===============================================================
subroutine conv_cartesian_to_spherical_rad_0d_miss_specified(x, y, z, lon, lat, miss_c, miss_s)
  implicit none
  real(8), intent(in)  :: x, y, z
  real(8), intent(out) :: lon, lat
  real(8), intent(in)  :: miss_c  ! missing value of cartesian
  real(8), intent(in)  :: miss_s  ! missing value of spherical

  if( x == miss_c )then
    lon = miss_s
    lat = miss_s
  else
    call conv_cartesian_to_spherical_rad_0d_miss_unspecified(x, y, z, lon, lat)
  endif
end subroutine conv_cartesian_to_spherical_rad_0d_miss_specified
!===============================================================
!
!===============================================================
subroutine conv_cartesian_to_spherical_rad_1d_miss_unspecified(x, y, z, lon, lat)
  implicit none
  real(8), intent(in)  :: x(:), y(:), z(:)
  real(8), intent(out) :: lon(:), lat(:)

  integer :: i

  do i = 1, size(x)
    call conv_cartesian_to_spherical_rad_0d_miss_unspecified(&
             x(i), y(i), z(i), lon(i), lat(i))
  enddo
end subroutine conv_cartesian_to_spherical_rad_1d_miss_unspecified
!===============================================================
!
!===============================================================
subroutine conv_cartesian_to_spherical_rad_1d_miss_specified(x, y, z, lon, lat, miss_c, miss_s)
  implicit none
  real(8), intent(in)  :: x(:), y(:), z(:)
  real(8), intent(out) :: lon(:), lat(:)
  real(8), intent(in)  :: miss_c  ! missing value of cartesian
  real(8), intent(in)  :: miss_s  ! missing value of spherical

  integer :: i

  do i = 1, size(x)
    if( x(i) == miss_c )then
      lon(i) = miss_s
      lat(i) = miss_s
    else
      call conv_cartesian_to_spherical_rad_0d_miss_unspecified(&
               x(i), y(i), z(i), lon(i), lat(i))
    endif
  enddo
end subroutine conv_cartesian_to_spherical_rad_1d_miss_specified
!===============================================================
!
!===============================================================
subroutine conv_cartesian_to_spherical_rad_2d_miss_unspecified(x, y, z, lon, lat)
  implicit none
  real(8), intent(in)  :: x(:,:), y(:,:), z(:,:)
  real(8), intent(out) :: lon(:,:), lat(:,:)

  integer :: i, j

  do j = 1, size(x,2)
    do i = 1, size(x,1)
      call conv_cartesian_to_spherical_rad_0d_miss_unspecified(&
               x(i,j), y(i,j), z(i,j), lon(i,j), lat(i,j))
    enddo
  enddo
end subroutine conv_cartesian_to_spherical_rad_2d_miss_unspecified
!===============================================================
!
!===============================================================
subroutine conv_cartesian_to_spherical_rad_2d_miss_specified(x, y, z, lon, lat, miss_c, miss_s)
  implicit none
  real(8), intent(in)  :: x(:,:), y(:,:), z(:,:)
  real(8), intent(out) :: lon(:,:), lat(:,:)
  real(8), intent(in)  :: miss_c  ! missing value of cartesian
  real(8), intent(in)  :: miss_s  ! missing value of spherical

  integer :: i, j

  do j = 1, size(x,2)
    do i = 1, size(x,1)
      if( x(i,j) == miss_c )then
        lon(i,j) = miss_s
        lat(i,j) = miss_s
      else
        call conv_cartesian_to_spherical_rad_0d_miss_unspecified(&
                 x(i,j), y(i,j), z(i,j), lon(i,j), lat(i,j))
      endif
    enddo
  enddo
end subroutine conv_cartesian_to_spherical_rad_2d_miss_specified
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Calc. coefs. of plane OAB (ax + by + cz = 0)
!===============================================================
subroutine calc_coefs_large_arc_spherical(lon1, lat1, lon2, lat2, a, b, c)
  implicit none
  real(8), intent(in)  :: lon1, lat1, lon2, lat2
  real(8), intent(out) :: a, b, c
  real(8) :: x1, y1, z1, x2, y2, z2

  x1 = cos(lat1) * cos(lon1)
  y1 = cos(lat1) * sin(lon1)
  z1 = sin(lat1)

  x2 = cos(lat2) * cos(lon2)
  y2 = cos(lat2) * sin(lon2)
  z2 = sin(lat2)

  call calc_cross_product(x1, y1, z1, x2, y2, z2, a, b, c)
end subroutine calc_coefs_large_arc_spherical
!===============================================================
!
!===============================================================
subroutine calc_coefs_large_arc_cartesian(x1, y1, z1, x2, y2, z2, a, b, c)
  implicit none
  real(8), intent(in)  :: x1, y1, z1, x2, y2, z2
  real(8), intent(out) :: a, b, c

  call calc_cross_product(x1, y1, z1, x2, y2, z2, a, b, c)
end subroutine calc_coefs_large_arc_cartesian
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Calc. longit. and latit. of the intersection.
! Intersection is confirmed.
!===============================================================
subroutine calc_intersection_sphere_normal_normal_confirmed(&
    sx1, sy1, sz1, sx2, sy2, sz2, sa, sb, sc, &
    tx1, ty1, tz1, tx2, ty2, tz2, ta, tb, tc, &
    west, east, plon, plat, stat)
  implicit none
  real(8), intent(in) :: sx1, sy1, sz1, sx2, sy2, sz2
  real(8), intent(in) :: sa, sb, sc
  real(8), intent(in) :: tx1, ty1, tz1, tx2, ty2, tz2
  real(8), intent(in) :: ta, tb, tc
  real(8), intent(in) :: west, east
  real(8), intent(out) :: plon, plat
  integer, intent(out) :: stat

  real(8) :: dp_S1_IT, dp_S2_IT, dp_T1_IS, dp_T2_IS
  real(8) :: px, py, pz
  real(8) :: plon1, plat1, plon2, plat2
  real(8) :: diff_plon1_west, diff_plon1_east, diff_plon2_west, diff_plon2_east
  real(8) :: diff_min
  !-------------------------------------------------------------
  dp_S1_IT = sx1*ta + sy1*tb + sz1*tc  ! dot products
  dp_S2_IT = sx2*ta + sy2*tb + sz2*tc
  dp_T1_IS = tx1*sa + ty1*sb + tz1*sc
  dp_T2_IS = tx2*sa + ty2*sb + tz2*sc

!  call edbg('s1 ('//str((/sx1,sy1,sz1/),'es10.3',',')//')')
!  call edbg('s2 ('//str((/sx2,sy2,sz2/),'es10.3',',')//')')
!  call edbg('t1 ('//str((/tx1,ty1,tz1/),'es10.3',',')//')')
!  call edbg('t2 ('//str((/tx2,ty2,tz2/),'es10.3',',')//')')
!  call edbg('IS ('//str((/sa,sb,sc/),'es15.8',',')//')')
!  call edbg('IT ('//str((/ta,tb,tc/),'es15.8',',')//')')
!  call edbg('S1_IT '//str(dp_S1_IT,'es15.8')//' is zero: '//str(dp_S1_IT==0.d0))
!  call edbg('S2_IT '//str(dp_S2_IT,'es15.8')//' is zero: '//str(dp_S2_IT==0.d0))
!  call edbg('T1_IS '//str(dp_T1_IS,'es15.8')//' is zero: '//str(dp_T1_IS==0.d0))
!  call edbg('T2_IS '//str(dp_T2_IS,'es15.8')//' is zero: '//str(dp_T2_IS==0.d0))

  call calc_cross_product(sa, sb, sc, ta, tb, tc, px, py, pz)

!  call edbg('P  ('//str((/px,py,pz/),'es15.8',',')//')')
  !-------------------------------------------------------------
  ! Case: Two lines are parallel (same normal vector)
  !-------------------------------------------------------------
  if( px**2+py**2+pz**2 == 0.d0 )then
    plon = 0.d0
    plat = 0.d0
    stat = 1
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  stat = 0
  !-------------------------------------------------------------
  ! Case: P = IA x IB
  if( dp_S1_IT > 0.d0 .and. dp_S2_IT < 0.d0 .and. &
      dp_T1_IS < 0.d0 .and. dp_T2_IS > 0.d0 )then
    !call edbg('P = IA x IB')
    call conv_cartesian_to_spherical_rad(px, py, pz, plon, plat)
  !-------------------------------------------------------------
  ! Case: P = IB x IA
  elseif( dp_S1_IT < 0.d0 .and. dp_S2_IT > 0.d0 .and. &
          dp_T1_IS > 0.d0 .and. dp_T2_IS < 0.d0 )then
    !call edbg('P = IB x IA')
    call conv_cartesian_to_spherical_rad(-px, -py, -pz, plon, plat)
  !-------------------------------------------------------------
  ! Case: Others
  else
    call conv_cartesian_to_spherical_rad(px, py, pz, plon1, plat1)

    if( plon1 > rad_180deg )then
      plon2 = plon1 - rad_180deg
    else
      plon2 = plon1 + rad_180deg
    endif
    plat2 = -plat1

    !call edbg('west: '//str(west*r2d,'f12.8')//' east: '//str(east*r2d,'f12.8'))
    !call edbg('P1: ('//str((/plon1,plat1/)*r2d,'f12.8',', ')//')')
    !call edbg('P2: ('//str((/plon2,plat2/)*r2d,'f12.8',', ')//')')

    if( which_is_western(plon1,west) /= 1 .and. which_is_western(plon1,east) /= 2 )then
      !call edbg('P = P1')
      plon = plon1
      plat = plat1
    elseif( which_is_western(plon2,west) /= 1 .and. which_is_western(plon2,east) /= 2 )then
      !call edbg('P = P2')
      plon = plon2
      plat = plat2
    else
      diff_plon1_west = abs(londiff_rad(plon1,west))
      diff_plon1_east = abs(londiff_rad(plon1,east))
      diff_plon2_west = abs(londiff_rad(plon2,west))
      diff_plon2_east = abs(londiff_rad(plon2,east))
      diff_min = min(diff_plon1_west, diff_plon1_east, diff_plon2_west, diff_plon2_east)

      if( diff_min == diff_plon1_west )then
        !call edbg('P = (west,Plat1)')
        plon = west
        plat = plat1
      elseif( diff_min == diff_plon1_east )then
        !call edbg('P = (east,Plat1)')
        plon = east
        plat = plat1
      elseif( diff_min == diff_plon2_west )then
        !call edbg('P = (west,Plat2)')
        plon = west
        plat = plat2
      elseif( diff_min == diff_plon2_east )then
        !call edbg('P = (east,Plat2)')
        plon = east
        plat = plat2
      endif

      !call edbg('Calculated intersection is out of range.')
    endif
  endif
  !-------------------------------------------------------------
end subroutine calc_intersection_sphere_normal_normal_confirmed
!===============================================================
!
!===============================================================
subroutine calc_intersection_sphere_normal_meridian_0d(&
    wlon, wlat, elon, elat, lon, lat)
  implicit none
  real(8), intent(in)  :: wlon, wlat, elon, elat
  real(8), intent(in)  :: lon
  real(8), intent(out) :: lat

  if( wlon < elon )then
    lat = atan( (tan(wlat)*sin(elon-lon) + tan(elat)*sin(lon-wlon)) / sin(elon-wlon) )
  else
    lat = atan( (tan(wlat)*sin(londiff_rad(elon,lon)) + tan(elat)*sin(londiff_rad(wlon,lon))) &
                  / sin(rad_360deg-wlon+elon) )
  endif
end subroutine calc_intersection_sphere_normal_meridian_0d
!===============================================================
!
!===============================================================
subroutine calc_intersection_sphere_normal_meridian_1d(&
    wlon, wlat, elon, elat, lon, lat)
  implicit none
  real(8), intent(in)  :: wlon, wlat, elon, elat
  real(8), intent(in)  :: lon(:)
  real(8), intent(out) :: lat(:)

  integer :: i, imax

  imax = size(lon)

  if( wlon < elon )then
    do i = 1, imax
      lat(i) = atan( (tan(wlat)*sin(elon-lon(i)) + tan(elat)*sin(lon(i)-wlon)) / sin(elon-wlon) )
    enddo
  else
    do i = 1, imax
      lat(i) = atan( (tan(wlat)*sin(londiff_rad(elon,lon(i))) &
                        + tan(elat)*sin(londiff_rad(wlon,lon(i)))) &
                       / sin(rad_360deg-wlon+elon) )
    enddo
  endif
end subroutine calc_intersection_sphere_normal_meridian_1d
!===============================================================
! Calc. longit. of the intersection.
! 交差するかどうか事前に分かっていない場合に使う。
! 経度の範囲を指定しない。
!===============================================================
subroutine calc_intersection_sphere_normal_parallel1(&
    sa, sb, sc, ssgn, tz, lon, stat)
  implicit none
  real(8), intent(in)  :: sa, sb, sc
  integer, intent(in)  :: ssgn
  real(8), intent(in)  :: tz
  real(8), intent(out) :: lon
  integer, intent(out) :: stat

  real(8) :: d2
  real(8) :: x_numer, y_numer
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  d2 = (sa**2 + sb**2) - (sa**2 + sb**2 + sc**2) * tz**2

  if( d2 > 0.d0 )then
    stat = STAT_INTERSECTION_YES

    x_numer = -sa*sc*tz - ssgn*sb*sqrt(d2)
    y_numer = -sb*sc*tz + ssgn*sa*sqrt(d2)

    lon = atan2(y_numer, x_numer)
  else
    stat = STAT_INTERSECTION_NO
    lon = 0.d0
  endif
  !-------------------------------------------------------------
end subroutine  calc_intersection_sphere_normal_parallel1
!===============================================================
! Calc. longit. of the intersection.
! 交差するかどうか事前に分かっていない場合に使う。
! 経度の範囲を指定する。
!===============================================================
subroutine calc_intersection_sphere_normal_parallel2(&
    sa, sb, sc, ssgn, tz, wlon, elon, plon, stat)
  implicit none
  real(8), intent(in)  :: sa, sb, sc
  integer, intent(in)  :: ssgn
  real(8), intent(in)  :: tz
  real(8), intent(in)  :: wlon, elon
  real(8), intent(out) :: plon
  integer, intent(out) :: stat

  real(8) :: cos_lat
  real(8) :: tan_wlat, tan_elat
  real(8) :: d
  real(8) :: x_numer, y_numer
  logical :: intersect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  cos_lat = sqrt(1.d0 - tz**2)  ! > 0

  tan_wlat = - (sa*cos(wlon) + sb*sin(wlon)) / sc
  tan_elat = - (sa*cos(elon) + sb*sin(elon)) / sc

  selectcase( ssgn )
  case( 1 )
    ! tan_wlat < tan_lat .and. tan_lat < tan_elat
    intersect = tan_wlat*cos_lat < tz .and. tz < tan_elat*cos_lat
  case( -1 )
    ! tan_wlat > tan_lat .and. tan_lat > tan_elat
    intersect = tan_wlat*cos_lat > tz .and. tz > tan_elat*cos_lat
  case default
    call echo(code%bgn, 'calc_intersection_sphere_normal_parallel2')
    call eerr(str(msg_invalid_value())//&
            '\nssgn: '//str(ssgn))
    call echo(code%ret)
  endselect

  if( intersect )then
    stat = STAT_INTERSECTION_YES

    d = sqrt(max(0.d0, (sa**2+sb**2) - (sa**2+sb**2+sc**2) * tz**2))

    x_numer = -sa*sc*tz - ssgn*sb*d
    y_numer = -sb*sc*tz + ssgn*sa*d

    plon = atan2(y_numer, x_numer)

    if( which_is_western(plon,wlon) /= 2 )then
      plon = wlon
    elseif( which_is_western(plon,elon) /= 1 )then
      plon = elon
    endif
  else
    stat = STAT_INTERSECTION_NO
    plon = 0.d0
  endif
end subroutine calc_intersection_sphere_normal_parallel2
!===============================================================
! Calc. longit. of the intersection.
! 交差することが事前に分かっている場合に使う。
! 交点が (wlon, elon) の範囲外になった場合、
! その値を wlon または elon に補正する。
!===============================================================
subroutine calc_intersection_sphere_normal_parallel3(&
    sa, sb, sc, ssgn, tz, wlon, elon, plon)
  implicit none
  real(8), intent(in)  :: sa, sb, sc
  integer, intent(in)  :: ssgn
  real(8), intent(in)  :: tz
  real(8), intent(in)  :: wlon, elon
  real(8), intent(out) :: plon

  real(8) :: d
  real(8) :: x_numer, y_numer

  d = sqrt(max(0.d0, (sa**2+sb**2) - (sa**2+sb**2+sc**2) * tz**2))

  x_numer = -sa*sc*tz - ssgn*sb*d
  y_numer = -sb*sc*tz + ssgn*sa*d

  plon = atan2(y_numer, x_numer)
  if( plon < rad_0deg ) plon = plon + rad_360deg

  if( which_is_western(plon,wlon) /= 2 )then
    plon = wlon
  elseif( which_is_western(plon,elon) /= 1 )then
    plon = elon
  endif
end subroutine calc_intersection_sphere_normal_parallel3
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
subroutine calc_lat_range_large_arc(&
    lon1, lat1, lon2, lat2, &
    a, b, c, &
    south, north, convex, lontop, lattop)
  implicit none
  real(8)   , intent(in)  :: lon1, lat1, lon2, lat2
  real(8)   , intent(in)  :: a, b, c
  real(8)   , intent(out) :: south, north
  integer(1), intent(out) :: convex
  real(8)   , intent(out) :: lontop, lattop

  real(8) :: plon, plat
  real(8) :: wlon, wlat, elon, elat

!  call echo(code%bgn, 'calc_lat_range_large_arc')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  south = min(lat1, lat2)
  north = max(lat1, lat2)

  convex = arc_convex_monotone
  lontop = 0.d0
  lattop = 0.d0
  !-------------------------------------------------------------
  ! Case: Arc intersects with the equator
  if( lat1 == rad_0deg .or. lat2 == rad_0deg .or. &
      (lat1 > rad_0deg .neqv. lat2 > rad_0deg) )then
    continue
  !-------------------------------------------------------------
  ! Case: Arc is meridian
  elseif( lon1 == lon2 .or. c == 0.d0 )then
    continue
  !-------------------------------------------------------------
  ! Case: Others
  else
    !-----------------------------------------------------------
    ! Calc. lon. of the top supposing it is convex
    !-----------------------------------------------------------
    ! Case: The normal (a,b,c) is upward
    !   lat < 0 @ (a,b) and lat > 0 @ (-a,-b)
    if( c > 0.d0 )then
      if( lat1 < rad_0deg )then
        plon = atan2(b, a)
      else
        plon = atan2(-b, -a)
      endif
    !-----------------------------------------------------------
    ! Case: The normal (a,b,c) is downward
    !   lat > 0 @ (a,b) and lat < 0 @ (-a,-b)
    else
      if ( lat1 > rad_0deg )then
        plon = atan2(b, a)
      else
        plon = atan2(-b, -a)
      endif
    endif

    if( plon < rad_0deg ) plon = plon + rad_360deg

!    call edbg('A1: '//str((/lon1,lat1/)*r2d,'es20.13'))
!    call edbg('A2: '//str((/lon2,lat2/)*r2d,'es20.13'))
!    call edbg('plon: '//str(plon*r2d,'es20.13'))
    !-----------------------------------------------------------
    !
    !-----------------------------------------------------------
    if( abs(lon1-lon2) < rad_180deg )then
      if( lon1 < lon2 )then
        wlon = lon1
        wlat = lat1
        elon = lon2
        elat = lat2
      else
        wlon = lon2
        wlat = lat2
        elon = lon1
        elat = lat1
      endif

      if( wlon < plon .and. plon < elon )then
        call calc_intersection_sphere_normal_meridian(wlon, wlat, elon, elat, plon, plat)

        if( lat1 < rad_0deg )then
          if( plat < south )then
            convex = arc_convex_downward
            lontop = plon
            lattop = plat
            south  = plat
          endif
        else
          if( plat > north )then
            convex = arc_convex_upward
            lontop = plon
            lattop = plat
            north  = plat
          endif
        endif
      endif
    else
      if( lon1 > lon2 )then
        wlon = lon1
        wlat = lat1
        elon = lon2
        elat = lat2
      else
        wlon = lon2
        wlat = lat2
        elon = lon1
        elat = lat1
      endif

      if( wlon < plon .or. plon < elon )then
        call calc_intersection_sphere_normal_meridian(wlon, wlat, elon, elat, plon, plat)

        if( lat1 < rad_0deg )then
          if( plat < south )then
            convex = arc_convex_downward
            lontop = plon
            lattop = plat
            south  = plat
          endif
        else
          if( plat > north )then
            convex = arc_convex_upward
            lontop = plon
            lattop = plat
            north  = plat
          endif
        endif
      endif
    endif
  endif
  !-------------------------------------------------------------
!  call echo(code%ret)
end subroutine calc_lat_range_large_arc
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
subroutine calc_lon_range_shared(&
    slon1, slon2, tlon1, tlon2, &
    sdir, tdir, id_west, id_east, west, east)
  real(8), intent(in)  :: slon1, slon2
  real(8), intent(in)  :: tlon1, tlon2
  integer, intent(out) :: sdir, tdir
  integer, intent(out) :: id_west, id_east
  real(8), intent(out) :: west, east

  integer :: s_id_west, t_id_west
  !integer :: stwest_id_west, steast_id_west
  !integer :: seast_twest_id_west, teast_swest_id_west
  real(8) :: swest, seast, twest, teast

!  if( debug )then
!    call echo(code%bgn, 'calc_lon_range_shared', '-a')
!  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  id_west = 0
  id_east = 0
  west = 0.d0
  east = 0.d0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  s_id_west = which_is_western(slon1, slon2)
  t_id_west = which_is_western(tlon1, tlon2)

  !call edbg('s_id_west: '//str(s_id_west))
  !call edbg('t_id_west: '//str(t_id_west))

  selectcase( s_id_west )
  case( 1 )
    swest = slon1
    seast = slon2
    sdir = 1
  case( 2 )
    swest = slon2
    seast = slon1
    sdir = -1
  case default
    call eerr(str(msg_invalid_value())//&
           '\n  s_id_west: '//str(s_id_west))
  endselect

  selectcase( t_id_west )
  case( 1 )
    twest = tlon1
    teast = tlon2
    tdir = 1
  case( 2 )
    twest = tlon2
    teast = tlon1
    tdir = -1
  case default
    call eerr(str(msg_invalid_value())//&
           '\n  t_id_west: '//str(t_id_west))
  endselect

  !call edbg('s west '//str(swest*r2d,'f12.7')//' east '//str(seast*r2d,'f12.7'))
  !call edbg('t west '//str(twest*r2d,'f12.7')//' east '//str(teast*r2d,'f12.7'))
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( abs(tlon2-tlon1) > rad_180deg )then
    if( twest <= swest .or. swest <= teast )then
      west = swest
      if( sdir == 1 )then
        id_west = 1  ! west == swest == slon1
      else
        id_west = 2  ! west == swest == slon2
      endif
    endif

    if( twest <= seast .or. seast <= teast )then
      east = seast
      if( sdir == 1 )then
        id_east = 2  ! east = seast == slon2
      else
        id_east = 1  ! east == seast == slon1
      endif
    endif
  else
    if( twest <= swest .and. swest <= teast )then
      west = swest
      selectcase( sdir )
      case( 1 )
        id_west = 1  ! west == swest == slon1
      case( -1 )
        id_west = 2  ! west == swest == slon2
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  sdir: '//str(sdir))
      endselect
    endif

    if( twest <= seast .and. seast <= teast )then
      east = seast
      selectcase( sdir )
      case( 1 )
        id_east = 2  ! east = seast == slon2
      case( -1 )
        id_east = 1  ! east == seast == slon1
      case default
        call eerr(str(msg_invalid_value())//&
                '\n  sdir: '//str(sdir))
      endselect
    endif
  endif

  if( abs(slon2-slon1) > rad_180deg )then
    if( id_west == 0 )then
      if( swest <= twest .or. twest <= seast )then
        west = twest
        selectcase( tdir )
        case( 1 )
          id_west = 3  ! west == twest == tlon1
        case( -1 )
          id_west = 4  ! west == twest == tlon2
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  tdir: '//str(tdir))
        endselect
      endif
    endif

    if( id_east == 0 )then
      if( swest <= teast .or. teast <= seast )then
        east = teast
        selectcase( tdir )
        case( 1 )
          id_east = 4  ! east == teast == tlon2
        case( -1 )
          id_east = 3  ! east == teast == tlon1
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  tdir: '//str(tdir))
        endselect
      endif
    endif
  else
    if( id_west == 0 )then
      if( swest <= twest .and. twest <= seast )then
        west = twest
        selectcase( tdir )
        case( 1 )
          id_west = 3  ! west == twest == tlon1
        case( -1 )
          id_west = 4  ! west == twest == tlon2
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  tdir: '//str(tdir))
        endselect
      endif
    endif

    if( id_east == 0 )then
      if( swest <= teast .and. teast <= seast )then
        east = teast
        selectcase( tdir )
        case( 1 )
          id_east = 4  ! east == teast == tlon2
        case( -1 )
          id_east = 3  ! east == teast == tlon1
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  tdir: '//str(tdir))
        endselect
      endif
    endif
  endif
  !-------------------------------------------------------------
!  if( debug )then
!    call echo(code%ret)
!  endif
end subroutine calc_lon_range_shared
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Area of rectangular on the sphere
!===============================================================
function area_sphere_rect_lat0d(lat1, lat2) result(area)
  implicit none
  real(8), intent(in) :: lat1, lat2
  real(8)             :: area

  area = abs(sin(lat1) - sin(lat2))
end function area_sphere_rect_lat0d
!===============================================================
!
!===============================================================
function area_sphere_rect_lat1d(lat1, lat2) result(area)
  implicit none
  real(8), intent(in) :: lat1(:), lat2(:)
  real(8)             :: area(size(lat1))

  area(:) = abs(sin(lat1(:)) - sin(lat2(:)))
end function area_sphere_rect_lat1d
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Area of rectangular on the ellipsoid
!===============================================================
function area_ellips_rect_lat0d(lat1, lat2, e2) result(area)
  implicit none
  real(8), intent(in) :: lat1, lat2  ![rad]
  real(8), intent(in) :: e2   ! Square of the eccentricity
  real(8)             :: area
  real(8) :: e

  e = sqrt(e2)
  area = (1.d0-e2) * abs(s(lat1)-s(lat2))
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
function s(lat) result(res)
  implicit none
  real(8), intent(in) :: lat  ![rad]
  real(8)             :: res
  real(8) :: z

  z = sin(lat)
  res = 0.5d0 * z/(1.d0-e2*z**2) &
        + 0.25d0 / e * log(abs((1.d0+e*z)/(1.d0-e*z)))
end function
!---------------------------------------------------------------
end function area_ellips_rect_lat0d
!===============================================================
!
!===============================================================
function area_ellips_rect_lat1d(lat1, lat2, e2) result(area)
  implicit none
  real(8), intent(in) :: lat1(:), lat2(:)  ![rad]
  real(8), intent(in) :: e2   ! Square of the eccentricity
  real(8)             :: area(size(lat1))
  real(8) :: e

  e = sqrt(e2)
  area(:) = (1.d0-e2) * abs(s(lat1(:))-s(lat2(:)))
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
function s(lat) result(res)
  implicit none
  real(8), intent(in) :: lat(:)  ![rad]
  real(8)             :: res(size(lat))
  real(8) :: z(size(lat))

  z(:) = sin(lat(:))
  res(:) = 0.5d0 * z(:)/(1.d0-e2*z(:)**2) &
           + 0.25d0 / e * log(abs((1.d0+e*z(:))/(1.d0-e*z(:))))
end function
!---------------------------------------------------------------
end function area_ellips_rect_lat1d
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
real(8) function area_sphere_polarrect(lon, sin_lat, sgn_pole) result(area)
  implicit none
  real(8), intent(in) :: lon, sin_lat
  integer, intent(in) :: sgn_pole

  area = abs(sgn_pole - sin_lat) * lon
end function area_sphere_polarrect
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
function area_sphere_tri(lonA, latA, lonB, latB, lonC, latC) result(area)
  implicit none
  real(8), intent(in)  :: lonA, latA, lonB, latB, lonC, latC  ![rad]
  real(8)              :: area
  real(8) :: OA(3), OB(3), OC(3)
  real(8) :: OAxOB(3), OBxOC(3), OCxOA(3)
  real(8) :: cosA_denom2, cosB_denom2, cosC_denom2
  real(8) :: cosA_numer, cosB_numer, cosC_numer
  real(8) :: cosA, cosB, cosC

  OA(:) = (/cos(latA)*cos(lonA), cos(latA)*sin(lonA), sin(latA)/)
  OB(:) = (/cos(latB)*cos(lonB), cos(latB)*sin(lonB), sin(latB)/)
  OC(:) = (/cos(latC)*cos(lonC), cos(latC)*sin(lonC), sin(latC)/)

  ! cos
  call calc_cross_product(OA, OB, OAxOB)
  call calc_cross_product(OB, OC, OBxOC)
  call calc_cross_product(OC, OA, OCxOA)

  cosA_denom2 = sum(OCxOA(:)**2) * sum(OAxOB(:)**2)
  cosB_denom2 = sum(OAxOB(:)**2) * sum(OBxOC(:)**2)
  cosC_denom2 = sum(OBxOC(:)**2) * sum(OCxOA(:)**2)
  if( cosA_denom2 == 0.d0 .or. cosB_denom2 == 0.d0 .or. cosC_denom2 == 0.d0 )then
    area = 0.d0
    return
  endif

  cosA_numer = sum(-OCxOA(:)*OAxOB(:))
  cosB_numer = sum(-OAxOB(:)*OBxOC(:))
  cosC_numer = sum(-OBxOC(:)*OCxOA(:))

  cosA = cosA_numer / sqrt(cosA_denom2)
  cosB = cosB_numer / sqrt(cosB_denom2)
  cosC = cosC_numer / sqrt(cosC_denom2)

  area = max(0.d0, acos(cosA) + acos(cosB) + acos(cosC) - pi)
end function area_sphere_tri
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
real(8) function area_sphere_righttri_south_bottom(lat1, lat2) result(area)
  implicit none
  real(8), intent(in) :: lat1, lat2

  real(8) :: south, north
  real(8) :: clat, dlat

  south = min(lat1,lat2)
  north = max(lat1,lat2)
  dlat = (north - south) * 0.5d0
  clat = (north + south) * 0.5d0

  if( dlat < 1d-16 )then
    area = 0.d0
  else
    area = -sin(south) + sin(clat)*(sin(dlat)/dlat)
  endif
end function area_sphere_righttri_south_bottom
!===============================================================
!
!===============================================================
real(8) function area_sphere_righttri_north_bottom(lat1, lat2) result(area)
  implicit none
  real(8), intent(in) :: lat1, lat2

  real(8) :: south, north
  real(8) :: clat, dlat

  south = min(lat1,lat2)
  north = max(lat1,lat2)
  dlat = (north - south) * 0.5d0
  clat = (north + south) * 0.5d0

  if( dlat < 1d-16 )then
    area = 0.d0
  else
    area = sin(north) - sin(clat)*(sin(dlat)/dlat)
  endif
end function area_sphere_righttri_north_bottom
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
real(8) function area_sphere_polartri_spherical(&
    lon, lat1, lat2, sgn_pole) result(area)
  implicit none
  real(8), intent(in) :: lon, lat1, lat2
  integer, intent(in) :: sgn_pole

  integer :: imax, i
  real(8) :: c1lon, c1lat, c2lon, c2lat
  real(8) :: area_add

  if( lon > rad_30deg )then
    imax = ceiling(lon / rad_30deg)

    area = 0.d0
    c1lon = 0.d0
    c1lat = lat1
    do i = 1, imax
      if( i == imax )then
        c2lon = lon
      else
        c2lon = lon * i / imax
      endif

      call calc_intersection_sphere_normal_meridian(&
             0.d0, lat1, lon, lat2, c2lon, c2lat)

      area_add = area_sphere_polartri_lt90deg(lon/imax, c1lat, c2lat, sgn_pole)
      area = area + area_add

      c1lon = c2lon
      c1lat = c2lat
    enddo
  else
    area = area_sphere_polartri_lt90deg(lon, lat1, lat2, sgn_pole)
  endif
end function area_sphere_polartri_spherical
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
! Fixed version
!===============================================================
real(8) function area_sphere_polartri_lt90deg(&
    lon, lat1, lat2, sgn_pole)&
  result(area)
  implicit none
  real(8), intent(in) :: lon, lat1, lat2
  integer, intent(in) :: sgn_pole

  real(8) :: k, eta2, kappa, chi
  real(8), parameter :: eta2_thresh = 0.d0
  real(8), parameter :: chi_diff_thresh = 1d-10

  k = cos(lat1)*cos(lat2)*sin(lon*0.5d0)**2
  eta2 = sin(lat1-lat2)**2 + 4*k*(cos(lat1-lat2) - k)

  kappa = sin(lon) * (sin(lat1)+sin(lat2)) &
          * 2.d0 * (sin((lat1-lat2)*0.5d0)**2 + cos(lat1)*cos(lat2)*sin(lon*0.5d0)**2)

  if( eta2 <= eta2_thresh )then
    chi = 0.d0
  else
    chi = sgn_pole * kappa / eta2

    if( abs(chi) -1.d0 > chi_diff_thresh )then
      call eerr(str(msg_unexpected_condition())//&
              '\n  abs(chi) - 1.0 > '//str(chi_diff_thresh)//&
              '\n  chi  : '//str(chi,'es20.13')//&
              '\n  eta2 : '//str(eta2,'es20.13')//&
              '\n  kappa: '//str(kappa,'es20.13'))
    endif
  endif

  area = lon - asin(chi)
end function area_sphere_polartri_lt90deg
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
real(8) function area_sphere_polygon_spherical(lon, lat, arctyp) result(area)
  implicit none
  real(8)   , intent(in) :: lon(:), lat(:)
  integer(1), intent(in) :: arctyp(:)

  integer :: nmax, n1, n2
  integer :: sgn_pole
  real(8) :: area_add

  call echo(code%bgn, 'area_sphere_polygon_spherical', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( lat(1) > rad_0deg )then
    sgn_pole = 1
  else
    sgn_pole = -1
  endif

  nmax = size(lon)

  n1 = nmax
  n2 = 1

  area = 0.d0

  do while( n2 <= nmax )

    selectcase( arctyp(n1) )
    case( arc_type_normal )
      area_add = area_sphere_polartri(londiff_rad(lon(n1),lon(n2)), lat(n1), lat(n2), sgn_pole)
      area = area + area_add * dir_lon(lon(n1),lon(n2)) * sgn_pole
    case( arc_type_parallel )
      area_add = area_sphere_polarrect(londiff_rad(lon(n1),lon(n2)), sin(lat(n1)), sgn_pole)
      area = area + area_add * dir_lon(lon(n1),lon(n2)) * sgn_pole
    case( arc_type_meridian )
      continue
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  arctyp('//str(n1)//'): '//str(arctyp(n1)))
    endselect

    n1 = n2
    n2 = n2 + 1
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function area_sphere_polygon_spherical
!===============================================================
!
!===============================================================
real(8) function area_sphere_polygon_cartesian(x, y, z, arctyp) result(area)
  implicit none
  real(8)   , intent(in) :: x(:), y(:), z(:)
  integer(1), intent(in) :: arctyp(:)

  integer :: nmax, n1, n2
  real(8) :: lon1, lon2
  real(8) :: lat1, lat2
  integer :: sgn_pole

  call echo(code%bgn, 'area_sphere_polygon_cartesian', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( z(1) > 0.d0 )then
    sgn_pole = 1
  else
    sgn_pole = -1
  endif

  nmax = size(x)

  n1 = nmax
  n2 = 1

  lon1 = atan2(y(n1), x(n1))
  lon2 = atan2(y(n2), x(n2))

  lat1 = asin(z(n1))
  lat2 = asin(z(n2))

  area = 0.d0

  do while( n2 <= nmax )

    selectcase( arctyp(n1) )
    case( arc_type_normal )
      area = area + area_sphere_polartri(londiff_rad(lon1,lon2), lat1, lat2, sgn_pole)&
                      * dir_lon(lon1,lon2) * sgn_pole
    case( arc_type_parallel )
      area = area + area_sphere_polarrect(londiff_rad(lon1,lon2), z(n1), sgn_pole)&
                      * dir_lon(lon1,lon2) * sgn_pole
    case( arc_type_meridian )
      continue
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  arctyp('//str(n1)//'): '//str(arctyp(n1)))
    endselect

    n1 = n2
    n2 = n2 + 1

    lon1 = lon2
    lon2 = atan2(y(n2), x(n2))

    lat1 = lat2
    lat2 = asin(z(n2))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function area_sphere_polygon_cartesian
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
subroutine calc_area_sphere_parallel_to_parallel(&
    slon1, slon2, slat, tlon1, tlon2, tlat, &
    sgn_pole, &
    area, arc_rel)
  implicit none
  real(8)   , intent(in)  :: slon1, slon2, slat
  real(8)   , intent(in)  :: tlon1, tlon2, tlat
  integer   , intent(in)  :: sgn_pole
  real(8)   , intent(out) :: area
  integer(1), intent(out) :: arc_rel

  integer :: id_west, id_east
  integer :: sgn
  integer :: sdir_lon, tdir_lon
  real(8) :: west, east
  real(8) :: lon

  character(clen_var), parameter :: proc = 'calc_area_sphere_parallel_to_parallel'

  if( debug )then
    call echo(code%bgn, proc)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  area = 0.d0
  arc_rel = arc_rel_lat_para_para_undef
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( sgn_pole )
  case( 1 )
    if( slat >= tlat )then
      arc_rel = arc_rel_lat_para_para_above

      if( debug )then
        call edbg('s is above t')
        call echo(code%ret)
      endif
      return
    endif
  case( -1 )
    if( slat <= tlat )then
      arc_rel = arc_rel_lat_para_para_below

      if( debug )then
        call edbg('s is below t')
        call echo(code%ret)
      endif
      return
    endif
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  sgn_pole: '//str(sgn_pole))
  endselect
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call calc_lon_range_shared(&
         slon1, slon2, tlon1, tlon2, &
         sdir_lon, tdir_lon, id_west, id_east, west, east)

  sgn = -1 * sdir_lon * tdir_lon

  if( id_west == 0 )then
    if( debug )then
      call edbg('Ranges of longit. not intersect')
      call echo(code%ret)
    endif
    return
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( sgn_pole )
  case( 1 )
    arc_rel = arc_rel_lat_para_para_below
  case( -1 )
    arc_rel = arc_rel_lat_para_para_above
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  sgn_pole: '//str(sgn_pole))
  endselect

  lon = londiff_rad(west, east)
  area = area_sphere_rect(slat, tlat) * lon * sgn
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ret)
  endif
end subroutine calc_area_sphere_parallel_to_parallel
!===============================================================
!
!===============================================================
subroutine calc_area_sphere_normal_to_parallel(&
    slon1, slat1, slon2, slat2, &
    sa, sb, sc, sconvex, slontop, slattop, &
    tlon1, tlon2, tlat, &
    sgn_pole, &
    area, arc_rel)
  implicit none
  real(8)   , intent(in)  :: slon1, slat1, slon2, slat2
  real(8)   , intent(in)  :: sa, sb, sc
  integer(1), intent(in)  :: sconvex
  real(8)   , intent(in)  :: slontop, slattop
  real(8)   , intent(in)  :: tlon1, tlon2, tlat
  integer   , intent(in)  :: sgn_pole
  real(8)   , intent(out) :: area
  integer(1), intent(out) :: arc_rel

  real(8)    :: ssouth, snorth
  integer(1) :: sconvex_this
  integer(1) :: stat_lon_between
  integer    :: sdir_lon, tdir_lon
  integer    :: sgn
  integer    :: id_west, id_east
  real(8)    :: west, east
  real(8)    :: clon, clon1, clon2
  real(8)    :: lon
  real(8)    :: slat_west, slat_east
  real(8)    :: sarea, sarea1, sarea2
  real(8)    :: tarea, tarea1, tarea2
  real(8)    :: area1, area2

  character(clen_var), parameter :: proc = 'calc_area_sphere_normal_to_parallel'

  if( debug )then
    call echo(code%bgn, proc)
  endif
  !-------------------------------------------------------------
  ! Return if area == 0
  !-------------------------------------------------------------
  area = 0.d0
  arc_rel = arc_rel_lat_norm_para_undef

  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  !       Return if s is above t
  case( 1 )
    selectcase( sconvex )
    case( arc_convex_monotone, &
          arc_convex_upward )
      ssouth = min(slat1,slat2)
    case( arc_convex_downward )
      ssouth = slattop
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  sconvex: '//str(sconvex))
    endselect

    if( ssouth >= tlat )then
      arc_rel = arc_rel_lat_norm_para_above

      if( debug )then
        call edbg('s is above t')
        call echo(code%ret)
      endif
      return
    endif
  !-------------------------------------------------------------
  ! Case: Southward
  !       Return if s is below t
  case( -1 )
    selectcase( sconvex )
    case( arc_convex_monotone, &
          arc_convex_downward )
      snorth = max(slat1,slat2)
    case( arc_convex_upward )
      snorth = slattop
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  sconvex: '//str(sconvex))
    endselect

    if( snorth <= tlat )then
      arc_rel = arc_rel_lat_norm_para_below

      if( debug )then
        call edbg('s is below t')
        call echo(code%ret)
      endif
      return
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  sgn_pole: '//str(sgn_pole))
  endselect
  !-------------------------------------------------------------
  ! Calc. shared range of longit.
  !-------------------------------------------------------------
  call calc_lon_range_shared(&
         slon1, slon2, tlon1, tlon2, &
         sdir_lon, tdir_lon, id_west, id_east, west, east)

  sgn = -1 * sdir_lon * tdir_lon

  if( id_west == 0 )then
    if( debug )then
      call edbg('Ranges of longit. not intersect')
      call echo(code%ret)
    endif
    return
  endif

  selectcase( id_west )
  case( 1 )
    slat_west = slat1
  case( 2 )
    slat_west = slat2
  case( 3, 4 )
    selectcase( sdir_lon )
    case( 1 )
      call calc_intersection_sphere_normal_meridian(slon1, slat1, slon2, slat2, west, slat_west)
    case( -1 )
      call calc_intersection_sphere_normal_meridian(slon2, slat2, slon1, slat1, west, slat_west)
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  sdir_lon: '//str(sdir_lon))
    endselect
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  id_west: '//str(id_west))
  endselect

  selectcase( id_east )
  case( 1 )
    slat_east = slat1
  case( 2 )
    slat_east = slat2
  case( 3, 4 )
    selectcase( sdir_lon )
    case( 1 )
      call calc_intersection_sphere_normal_meridian(slon1, slat1, slon2, slat2, east, slat_east)
    case( -1 )
      call calc_intersection_sphere_normal_meridian(slon2, slat2, slon1, slat1, east, slat_east)
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  sdir_lon: '//str(sdir_lon))
    endselect
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  id_east: '//str(id_east))
  endselect

  if( debug )then
    call edbg('  west: ('//str((/west,slat_west/)*r2d,'f12.7',', ')//')')
    call edbg('  east: ('//str((/east,slat_east/)*r2d,'f12.7',', ')//')')
  endif
  !-------------------------------------------------------------
  ! Judge if convex in the shared range of longit.
  !-------------------------------------------------------------
  selectcase( sconvex )
  case( arc_convex_monotone )
    sconvex_this = sconvex
  case( arc_convex_upward, &
        arc_convex_downward )
    call get_stat_lon_between(slontop, west, east, stat_lon_between)

    selectcase( stat_lon_between )
    case( stat_lon_between_inside )
      sconvex_this = sconvex
    case( stat_lon_between_west, &
          stat_lon_between_east, &
          stat_lon_between_outside )
      sconvex_this = arc_convex_monotone
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  stat_lon_between: '//str(stat_lon_between))
    endselect
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  sconvex: '//str(sconvex))
  endselect
  !-------------------------------------------------------------
  ! Judge relations of arcs
  !-------------------------------------------------------------
  selectcase( sconvex_this )
  case( arc_convex_monotone )
    if( min(slat_west, slat_east) >= tlat )then
      arc_rel = arc_rel_lat_norm_para_above
    elseif( max(slat_west, slat_east) <= tlat )then
      arc_rel = arc_rel_lat_norm_para_below
    elseif( slat_west < tlat .and. tlat < slat_east )then
      arc_rel = arc_rel_lat_norm_para_one_intersection_upward
    elseif( slat_west > tlat .and. tlat > slat_east )then
      arc_rel = arc_rel_lat_norm_para_one_intersection_downward
    else
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_unexpected_condition())//&
              '\nNot in any case.')
    endif
  case( arc_convex_upward )
    if( min(slat_west, slat_east) >= tlat )then
      arc_rel = arc_rel_lat_norm_para_above
    elseif( slattop <= tlat )then
      arc_rel = arc_rel_lat_norm_para_below
    elseif( slat_west < tlat .and. tlat <= slat_east )then
      arc_rel = arc_rel_lat_norm_para_one_intersection_upward
    elseif( slat_west >= tlat .and. tlat > slat_east )then
      arc_rel = arc_rel_lat_norm_para_one_intersection_downward
    elseif( slat_west < tlat .and. tlat > slat_east )then
      arc_rel = arc_rel_lat_norm_para_two_intersections_convex_upward
    else
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_unexpected_condition())//&
              '\nNot in any case.')
    endif
  case( arc_convex_downward )
    if( slattop >= tlat )then
      arc_rel = arc_rel_lat_norm_para_above
    elseif( max(slat_west, slat_east) <= tlat )then
      arc_rel = arc_rel_lat_norm_para_below
    elseif( slat_west <= tlat .and. tlat < slat_east )then
      arc_rel = arc_rel_lat_norm_para_one_intersection_upward
    elseif( slat_west > tlat .and. tlat >= slat_east )then
      arc_rel = arc_rel_lat_norm_para_one_intersection_downward
    elseif( slat_west > tlat .and. tlat < slat_east )then
      arc_rel = arc_rel_lat_norm_para_two_intersections_convex_downward
    else
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_unexpected_condition())//&
              '\nNot in any case.')
    endif
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  sconvex_this: '//str(sconvex_this))
  endselect

  if( debug )then
    call edbg('arc_rel: '//str(str_arc_rel_lat(arc_rel)))
  endif
  !-------------------------------------------------------------
  ! Calc. area
  !-------------------------------------------------------------
  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  !       Calc. area(s) of the zone(s) above s and below t
  case( 1 )
    selectcase( arc_rel )
    !-----------------------------------------------------------
    ! Case: s is above t
    !       No area
    case( arc_rel_lat_norm_para_above )
      area = 0.d0

      call pdbg_no('Case: s is above t')
    !-----------------------------------------------------------
    ! Case: s is below t
    !       Area is west to east
    case( arc_rel_lat_norm_para_below )
      lon = londiff_rad(west, east)
      sarea = area_sphere_polartri(lon, slat_west, slat_east, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct0('Case: s is below t', &
                      sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s intersects with t upward
    !       Area is west to intersection
    case( arc_rel_lat_norm_para_one_intersection_upward )
      call calc_intersection_sphere_normal_parallel3(&
               sa, sb, sc, +sdir_lon, sin(tlat), west, east, clon)
      lon = londiff_rad(west, clon)
      sarea = area_sphere_polartri(lon, slat_west, tlat, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (upward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s intersects with t downward
    !       Area is intersection to east
    case( arc_rel_lat_norm_para_one_intersection_downward )
      call calc_intersection_sphere_normal_parallel3(&
               sa, sb, sc, -sdir_lon, sin(tlat), west, east, clon)
      lon = londiff_rad(east, clon)
      sarea = area_sphere_polartri(lon, slat_east, tlat, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (downward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s (convex upward) intersects with t twice
    !       Area is between intersections
    case( arc_rel_lat_norm_para_two_intersections_convex_downward )
      call calc_intersection_sphere_normal_parallel3(&
               sa, sb, sc, -sdir_lon, sin(tlat), west, east, clon1)
      call calc_intersection_sphere_normal_parallel3(&
               sa, sb, sc, +sdir_lon, sin(tlat), west, east, clon2)
      lon = londiff_rad(clon1, clon2)
      sarea = area_sphere_polartri(lon, tlat, tlat, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct2_centered('Case: Two intersections (convex upward)', &
                               clon1, clon2, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s (convex downward) intersects with t twice
    !       Area is edges to intersections
    case( arc_rel_lat_norm_para_two_intersections_convex_upward )
      call calc_intersection_sphere_normal_parallel3(&
               sa, sb, sc, +sdir_lon, sin(tlat), west, east, clon1)
      lon = londiff_rad(west, clon1)
      sarea1 = area_sphere_polartri(lon, slat_west, tlat, sgn_pole)
      tarea1 = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area1 = (sarea1 - tarea1) * sgn

      call calc_intersection_sphere_normal_parallel3(&
               sa, sb, sc, -sdir_lon, sin(tlat), west, east, clon2)
      lon = londiff_rad(east, clon2)
      sarea2 = area_sphere_polartri(lon, slat_east, tlat, sgn_pole)
      tarea2 = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area2 = (sarea2 - tarea2) * sgn

      area = area1 + area2

      call pdbg_isct2_splitted('Case: Two intersections (convex downward)', &
                               clon1, sarea1, tarea1, area1, &
                               clon2, sarea2, tarea2, area2, area)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  arc_rel: '//str(arc_rel))
    endselect
  !-------------------------------------------------------------
  ! Case: Southward
  !       Calc. area(s) of the zone(s) below s and above t
  case( -1 )
    selectcase( arc_rel )
    !-----------------------------------------------------------
    ! Case: s is below t
    !       No area
    case( arc_rel_lat_norm_para_below )
      area = 0.d0

      call pdbg_no('Case: s is below t')
    !-----------------------------------------------------------
    ! Case: s is above t
    !       Area is west to east
    case( arc_rel_lat_norm_para_above )
      lon = londiff_rad(west, east)
      sarea = area_sphere_polartri(lon, slat_west, slat_east, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct0('Case: s is above t', &
                      sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s intersects with t downward
    !       Area is west to intersection
    case( arc_rel_lat_norm_para_one_intersection_downward )
      call calc_intersection_sphere_normal_parallel3(&
               sa, sb, sc, -sdir_lon, sin(tlat), west, east, clon)
      lon = londiff_rad(west, clon)
      sarea = area_sphere_polartri(lon, slat_west, tlat, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (downward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s intersects with t upward
    !       Area is intersection to east
    case( arc_rel_lat_norm_para_one_intersection_upward )
      call calc_intersection_sphere_normal_parallel3(&
               sa, sb, sc, +sdir_lon, sin(tlat), west, east, clon)
      lon = londiff_rad(east, clon)
      sarea = area_sphere_polartri(lon, slat_east, tlat, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (upward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s (convex upward) intersects with t twice
    !       Area is between intersections
    case( arc_rel_lat_norm_para_two_intersections_convex_upward )
      call calc_intersection_sphere_normal_parallel3(&
               sa, sb, sc, +sdir_lon, sin(tlat), west, east, clon1)
      call calc_intersection_sphere_normal_parallel3(&
               sa, sb, sc, -sdir_lon, sin(tlat), west, east, clon2)
      lon = londiff_rad(clon1, clon2)
      sarea = area_sphere_polartri(lon, tlat, tlat, sgn_pole)
      tarea = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct2_centered('Case: Two intersections (convex upward)', &
                               clon1, clon2, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: Two intersections (downward)
    !       Area is edges to intersections
    case( arc_rel_lat_norm_para_two_intersections_convex_downward )
      call calc_intersection_sphere_normal_parallel3(&
               sa, sb, sc, -sdir_lon, sin(tlat), west, east, clon1)
      lon = londiff_rad(west, clon1)
      sarea1 = area_sphere_polartri(lon, slat_west, tlat, sgn_pole)
      tarea1 = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area1 = (sarea1 - tarea1) * sgn

      call calc_intersection_sphere_normal_parallel3(&
               sa, sb, sc, +sdir_lon, sin(tlat), west, east, clon2)
      lon = londiff_rad(east, clon2)
      sarea2 = area_sphere_polartri(lon, slat_east, tlat, sgn_pole)
      tarea2 = area_sphere_polarrect(lon, sin(tlat), sgn_pole)
      area2 = (sarea2 - tarea2) * sgn

      area = area1 + area2

      call pdbg_isct2_splitted('Case: Two intersections (convex downward)', &
                               clon1, sarea1, tarea1, area1, &
                               clon2, sarea2, tarea2, area2, area)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  arc_rel: '//str(arc_rel))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  sgn_pole: '//str(sgn_pole))
  endselect
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ret)
  endif
end subroutine calc_area_sphere_normal_to_parallel
!===============================================================
!
!===============================================================
subroutine calc_area_sphere_parallel_to_normal(&
    slon1, slon2, slat, &
    tlon1, tlat1, tlon2, tlat2, &
    ta, tb, tc, tconvex, tlontop, tlattop, &
    sgn_pole, &
    area, arc_rel)
  implicit none
  real(8)   , intent(in)  :: slon1, slon2, slat
  real(8)   , intent(in)  :: tlon1, tlat1, tlon2, tlat2
  real(8)   , intent(in)  :: ta, tb, tc
  integer(1), intent(in)  :: tconvex
  real(8)   , intent(in)  :: tlontop, tlattop
  integer   , intent(in)  :: sgn_pole
  real(8)   , intent(out) :: area
  integer(1), intent(out) :: arc_rel

  real(8)    :: tsouth, tnorth
  integer(1) :: tconvex_this
  integer(1) :: stat_lon_between
  integer    :: sdir_lon, tdir_lon
  integer    :: sgn
  integer    :: id_west, id_east
  real(8)    :: west, east
  real(8)    :: clon, clon1, clon2
  real(8)    :: lon
  real(8)    :: tlat_west, tlat_east
  real(8)    :: sarea, sarea1, sarea2
  real(8)    :: tarea, tarea1, tarea2
  real(8)    :: area1, area2

  character(clen_var) :: proc = 'calc_area_sphere_parallel_to_normal'

  if( debug )then
    call echo(code%bgn, proc)
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  area = 0.d0
  arc_rel = arc_rel_lat_para_norm_undef
  !-------------------------------------------------------------
  !
  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  !       Return if s is above t
  case( 1 )
    selectcase( tconvex )
    case( arc_convex_monotone, &
          arc_convex_downward )
      tnorth = max(tlat1,tlat2)
    case( arc_convex_upward )
      tnorth = tlattop
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  tconvex: '//str(tconvex))
    endselect

    if( slat >= tnorth )then
      arc_rel = arc_rel_lat_para_norm_above

      if( debug )then
        call edbg('s is above t')
        call echo(code%ret)
      endif
      return
    endif
  !-------------------------------------------------------------
  ! Case: Southward
  !       Return if s is below t
  case( -1 )
    selectcase( tconvex )
    case( arc_convex_monotone, &
          arc_convex_upward )
      tsouth = min(tlat1,tlat2)
    case( arc_convex_downward )
      tsouth = tlattop
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  tconvex: '//str(tconvex))
    endselect

    if( slat <= tsouth )then
      arc_rel = arc_rel_lat_para_norm_below

      if( debug )then
        call edbg('t is above s')
        call echo(code%ret)
      endif
      return
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  sgn_pole: '//str(sgn_pole))
  endselect
  !-------------------------------------------------------------
  ! Calc. shared range of longit.
  !-------------------------------------------------------------
  call calc_lon_range_shared(&
         slon1, slon2, tlon1, tlon2, &
         sdir_lon, tdir_lon, id_west, id_east, west, east)

  sgn = -1 * sdir_lon * tdir_lon

  if( id_west == 0 )then
    if( debug )then
      call edbg('Ranges of longit. not intersect')
      call echo(code%ret)
    endif
    return
  endif

  selectcase( id_west )
  case( 1, 2 )
    selectcase( tdir_lon )
    case( 1 )
      call calc_intersection_sphere_normal_meridian(tlon1, tlat1, tlon2, tlat2, west, tlat_west)
    case( -1 )
      call calc_intersection_sphere_normal_meridian(tlon2, tlat2, tlon1, tlat1, west, tlat_west)
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  tdir_lon: '//str(tdir_lon))
    endselect
  case( 3 )
    tlat_west = tlat1
  case( 4 )
    tlat_west = tlat2
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  id_west: '//str(id_west))
  endselect

  selectcase( id_east )
  case( 1, 2 )
    selectcase( tdir_lon )
    case( 1 )
      call calc_intersection_sphere_normal_meridian(tlon1, tlat1, tlon2, tlat2, east, tlat_east)
    case( -1 )
      call calc_intersection_sphere_normal_meridian(tlon2, tlat2, tlon1, tlat1, east, tlat_east)
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  tdir_lon: '//str(tdir_lon))
    endselect
  case( 3 )
    tlat_east = tlat1
  case( 4 )
    tlat_east = tlat2
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  id_east: '//str(id_east))
  endselect

  if( debug )then
    call edbg('  west: ('//str((/west,tlat_west/)*r2d,'f12.7',', ')//')')
    call edbg('  east: ('//str((/east,tlat_east/)*r2d,'f12.7',', ')//')')
  endif
  !-------------------------------------------------------------
  ! Judge if convex in the shared range of longit.
  !-------------------------------------------------------------
  selectcase( tconvex )
  case( arc_convex_monotone )
    tconvex_this = tconvex
  case( arc_convex_upward, &
        arc_convex_downward )
    call get_stat_lon_between(tlontop, west, east, stat_lon_between)

    selectcase( stat_lon_between )
    case( stat_lon_between_inside )
      tconvex_this = tconvex
    case( stat_lon_between_west, &
          stat_lon_between_east, &
          stat_lon_between_outside )
      tconvex_this = arc_convex_monotone
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  stat_lon_between: '//str(stat_lon_between))
    endselect
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  tconvex: '//str(tconvex))
  endselect
  !-------------------------------------------------------------
  ! Judge relations of arcs
  !-------------------------------------------------------------
  selectcase( tconvex_this )
  case( arc_convex_monotone )
    if( min(tlat_west, tlat_east) >= slat )then
      arc_rel = arc_rel_lat_para_norm_below
    elseif( max(tlat_west, tlat_east) <= slat )then
      arc_rel = arc_rel_lat_para_norm_above
    elseif( tlat_west < slat .and. slat < tlat_east )then
      arc_rel = arc_rel_lat_para_norm_one_intersection_upward
    elseif( tlat_west > slat .and. slat > tlat_east )then
      arc_rel = arc_rel_lat_para_norm_one_intersection_downward
    else
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_unexpected_condition())//&
              '\nNot in any case.')
    endif
  case( arc_convex_upward )
    if( min(tlat_west, tlat_east) >= slat )then
      arc_rel = arc_rel_lat_para_norm_below
    elseif( tlattop <= slat )then
      arc_rel = arc_rel_lat_para_norm_above
    elseif( tlat_west < slat .and. slat <= tlat_east )then
      arc_rel = arc_rel_lat_para_norm_one_intersection_upward
    elseif( tlat_west >= slat .and. slat > tlat_east )then
      arc_rel = arc_rel_lat_para_norm_one_intersection_downward
    elseif( tlat_west < slat .and. slat > tlat_east )then
      arc_rel = arc_rel_lat_para_norm_two_intersections_convex_upward
    else
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_unexpected_condition())//&
              '\nNot in any case.')
    endif
  case( arc_convex_downward )
    if( tlattop >= slat )then
      arc_rel = arc_rel_lat_para_norm_below
    elseif( max(tlat_west, tlat_east) <= slat )then
      arc_rel = arc_rel_lat_para_norm_above
    elseif( tlat_west <= slat .and. slat < tlat_east )then
      arc_rel = arc_rel_lat_para_norm_one_intersection_upward
    elseif( tlat_west > slat .and. slat >= tlat_east )then
      arc_rel = arc_rel_lat_para_norm_one_intersection_downward
    elseif( tlat_west > slat .and. slat < tlat_east )then
      arc_rel = arc_rel_lat_para_norm_two_intersections_convex_downward
    else
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_unexpected_condition())//&
              '\nNot in any case.')
    endif
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  tconvex_this: '//str(tconvex_this))
  endselect
  !-------------------------------------------------------------
  ! Calc. area
  !-------------------------------------------------------------
  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  !       Calc. area(s) of the zone(s) above s and below t
  case( 1 )
    selectcase( arc_rel )
    !-----------------------------------------------------------
    ! Case: s is above t
    !       No area
    case( arc_rel_lat_para_norm_above )
      area = 0.d0

      call pdbg_no('Case: s is above t')
    !-----------------------------------------------------------
    ! Case: s is below t
    !       Area is west to east
    case( arc_rel_lat_para_norm_below )
      lon = londiff_rad(west, east)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_west, tlat_east, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct0('Case: s is below t', &
                      sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t intersects with s downward
    !       Area is west to intersection
    case( arc_rel_lat_para_norm_one_intersection_downward )
      call calc_intersection_sphere_normal_parallel3(&
               ta, tb, tc, -tdir_lon, sin(slat), west, east, clon)
      lon = londiff_rad(west, clon)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_west, slat, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (downward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t intersects with s upward
    !       Area is intersection to east
    case( arc_rel_lat_para_norm_one_intersection_upward )
      call calc_intersection_sphere_normal_parallel3(&
               ta, tb, tc, +tdir_lon, sin(slat), west, east, clon)
      lon = londiff_rad(east, clon)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_east, slat, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (upward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t (convex upward) intersects with s twice
    !       Area is between intersections
    case( arc_rel_lat_para_norm_two_intersections_convex_upward )
      call calc_intersection_sphere_normal_parallel3(&
               ta, tb, tc, +tdir_lon, sin(slat), west, east, clon1)
      call calc_intersection_sphere_normal_parallel3(&
               ta, tb, tc, -tdir_lon, sin(slat), west, east, clon2)
      lon = londiff_rad(clon1, clon2)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, slat, slat, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct2_centered('Case: Two intersections (convex upward)', &
                               clon1, clon2, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t (convex downward) intersects with s twice
    !       Area is edges to intersections
    case( arc_rel_lat_para_norm_two_intersections_convex_downward )
      call calc_intersection_sphere_normal_parallel3(&
               ta, tb, tc, -tdir_lon, sin(slat), west, east, clon1)
      lon = londiff_rad(west, clon1)
      sarea1 = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea1 = area_sphere_polartri(lon, tlat_west, slat, sgn_pole)
      area1 = (sarea1 - tarea1) * sgn

      call calc_intersection_sphere_normal_parallel3(&
               ta, tb, tc, +tdir_lon, sin(slat), west, east, clon2)
      lon = londiff_rad(east, clon2)
      sarea2 = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea2 = area_sphere_polartri(lon, tlat_east, slat, sgn_pole)
      area2 = (sarea2 - tarea2) * sgn

      area = area1 + area2

      call pdbg_isct2_splitted('Case: Two intersections (convex downward)', &
                               clon1, sarea1, tarea1, area1, &
                               clon2, sarea2, tarea2, area2, area)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  arc_rel: '//str(arc_rel))
    endselect
  !-------------------------------------------------------------
  ! Case: Southward
  !       Calc. area(s) of the zone(s) below s and above t
  case( -1 )
    selectcase( arc_rel )
    !-----------------------------------------------------------
    ! Case: s is below t 
    !       No area
    case( arc_rel_lat_para_norm_below )
      area = 0.d0

      call pdbg_no('Case: s is below t')
    !-----------------------------------------------------------
    ! Case: s is above t
    !       Area is west to east
    case( arc_rel_lat_para_norm_above )
      lon = londiff_rad(west, east)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_west, tlat_east, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct0('Case: s is above t', &
                      sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t intersects with s upward
    !       Area is west to intersection
    case( arc_rel_lat_para_norm_one_intersection_upward )
      call calc_intersection_sphere_normal_parallel3(&
               ta, tb, tc, +tdir_lon, sin(slat), west, east, clon)
      lon = londiff_rad(west, clon)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_west, slat, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (upward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t intersects with s downward
    !       Area is intersection to east
    case( arc_rel_lat_para_norm_one_intersection_downward )
      call calc_intersection_sphere_normal_parallel3(&
               ta, tb, tc, -tdir_lon, sin(slat), west, east, clon)
      lon = londiff_rad(east, clon)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_east, slat, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct1('Case: One intersection (downward)', &
                      clon, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t (convex downward) intersects with s twice
    !       Area is between intersections
    case( arc_rel_lat_para_norm_two_intersections_convex_downward )
      call calc_intersection_sphere_normal_parallel3(&
               ta, tb, tc, -tdir_lon, sin(slat), west, east, clon1)
      call calc_intersection_sphere_normal_parallel3(&
               ta, tb, tc, +tdir_lon, sin(slat), west, east, clon2)
      lon = londiff_rad(clon1, clon2)
      sarea = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea = area_sphere_polartri(lon, slat, slat, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct2_centered('Case: Two intersections (convex downward)', &
                               clon1, clon2, sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: t (convex upward) intersects with s twice
    !       Area is edges to intersections
    case( arc_rel_lat_para_norm_two_intersections_convex_upward )
      call calc_intersection_sphere_normal_parallel3(&
               ta, tb, tc, +tdir_lon, sin(slat), west, east, clon1)
      lon = londiff_rad(west, clon1)
      sarea1 = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea1 = area_sphere_polartri(lon, tlat_west, slat, sgn_pole)
      area1 = (sarea1 - tarea1) * sgn

      call calc_intersection_sphere_normal_parallel3(&
               ta, tb, tc, -tdir_lon, sin(slat), west, east, clon2)
      lon = londiff_rad(east, clon2)
      sarea2 = area_sphere_polarrect(lon, sin(slat), sgn_pole)
      tarea2 = area_sphere_polartri(lon, tlat_east, slat, sgn_pole)
      area2 = (sarea2 - tarea2) * sgn

      area = area1 + area2

      call pdbg_isct2_splitted('Two intersections (convex upward)', &
                               clon1, sarea1, tarea1, area1, &
                               clon2, sarea2, tarea2, area2, area)
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  arc_rel: '//str(arc_rel))
    endselect
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  sgn_pole: '//str(sgn_pole))
  endselect
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ret)
  endif
end subroutine calc_area_sphere_parallel_to_normal
!===============================================================
! Calc. area above s and below t
!===============================================================
subroutine calc_area_sphere_normal_to_normal(&
    sx1, sy1, sz1, sx2, sy2, sz2, &
    slon1, slat1, slon2, slat2, &
    sa, sb, sc, &
    sconvex, slattop, &
    tx1, ty1, tz1, tx2, ty2, tz2, &
    tlon1, tlat1, tlon2, tlat2, &
    ta, tb, tc, &
    tconvex, tlattop, &
    sgn_pole, &
    area, arc_rel)
  implicit none
  real(8)   , intent(in)  :: sx1, sy1, sz1, sx2, sy2, sz2
  real(8)   , intent(in)  :: tx1, ty1, tz1, tx2, ty2, tz2
  real(8)   , intent(in)  :: slon1, slat1, slon2, slat2
  real(8)   , intent(in)  :: tlon1, tlat1, tlon2, tlat2
  real(8)   , intent(in)  :: sa, sb, sc
  real(8)   , intent(in)  :: ta, tb, tc
  integer(1), intent(in)  :: sconvex
  real(8)   , intent(in)  :: slattop
  integer(1), intent(in)  :: tconvex
  real(8)   , intent(in)  :: tlattop
  integer   , intent(in)  :: sgn_pole
  real(8)   , intent(out) :: area
  integer(1), intent(out) :: arc_rel

  real(8) :: ssouth, snorth, tsouth, tnorth
  real(8) :: west, east
  real(8) :: lon
  integer :: id_west, id_east
  real(8) :: slat_west, slat_east
  real(8) :: tlat_west, tlat_east
  integer :: sdir_lon, tdir_lon
  integer :: sgn
  real(8) :: clon, clat
  real(8) :: sarea, tarea
  integer :: stat

  character(clen_var), parameter :: proc = 'calc_area_sphere_normal_to_normal'

  if( debug )then
    call echo(code%bgn, 'calc_area_sphere_normal_to_normal')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  area = 0.d0
  arc_rel = arc_rel_lat_norm_norm_undef
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  !       Return if s is above t
  case( 1 )
    selectcase( sconvex )
    case( arc_convex_monotone, &
          arc_convex_upward )
      ssouth = min(slat1,slat2)
    case( arc_convex_downward )
      ssouth = slattop
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  sconvex: '//str(sconvex))
    endselect

    selectcase( tconvex )
    case( arc_convex_monotone, &
          arc_convex_downward )
      tnorth = max(tlat1,tlat2)
    case( arc_convex_upward )
      tnorth = tlattop
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  tconvex: '//str(tconvex))
    endselect

    if( ssouth >= tnorth )then
      arc_rel = arc_rel_lat_norm_norm_above

      if( debug )then
        call edbg('s is above t')
        call echo(code%ret)
      endif
      return
    endif
  !-------------------------------------------------------------
  ! Case: Southward
  !   Return if s is below t
  case( -1 )
    selectcase( sconvex )
    case( arc_convex_monotone, &
          arc_convex_downward )
      snorth = max(slat1,slat2)
    case( arc_convex_upward )
      snorth = slattop
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  sconvex: '//str(sconvex))
    endselect

    selectcase( tconvex )
    case( arc_convex_monotone, &
          arc_convex_upward )
      tsouth = min(tlat1,tlat2)
    case( arc_convex_downward )
      tsouth = tlattop
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  tconvex: '//str(tconvex))
    endselect

    if( snorth <= tsouth )then
      arc_rel = arc_rel_lat_norm_norm_below

      if( debug )then
        call edbg('s is below t')
        call echo(code%ret)
      endif
      return
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  sgn_pole: '//str(sgn_pole))
  endselect
  !-------------------------------------------------------------
  ! Calc. shared range of longit.
  !-------------------------------------------------------------
  call calc_lon_range_shared(&
        slon1, slon2, tlon1, tlon2, &
        sdir_lon, tdir_lon, id_west, id_east, west, east)

  sgn = -1 * sdir_lon * tdir_lon

  if( id_west == 0 )then
    if( debug )then
      call edbg('Ranges of longit. not intersect')
      call echo(code%ret)
    endif
    return
  endif

  selectcase( id_west )
  case( 1, 2 )
    if( id_west == 1 )then
      slat_west = slat1
    else
      slat_west = slat2
    endif

    selectcase( tdir_lon )
    case( 1 )  ! tlon1 < tlon2
      call calc_intersection_sphere_normal_meridian(tlon1, tlat1, tlon2, tlat2, west, tlat_west)
    case( -1 )  ! tlon2 < tlon1
      call calc_intersection_sphere_normal_meridian(tlon2, tlat2, tlon1, tlat1, west, tlat_west)
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  tdir_lon: '//str(tdir_lon))
    endselect
  case( 3, 4 )
    if( id_west == 3 )then
      tlat_west = tlat1
    else
      tlat_west = tlat2
    endif

    selectcase( sdir_lon )
    case( 1 )  ! slon1 < slon2
      call calc_intersection_sphere_normal_meridian(slon1, slat1, slon2, slat2, west, slat_west)
    case( -1 )  ! slon2 < slon1
      call calc_intersection_sphere_normal_meridian(slon2, slat2, slon1, slat1, west, slat_west)
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  sdir_lon: '//str(sdir_lon))
    endselect
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  id_west: '//str(id_west))
  endselect

  selectcase( id_east )
  case( 1, 2 )
    if( id_east == 1 )then
      slat_east = slat1
    else
      slat_east = slat2
    endif

    selectcase( tdir_lon )
    case( 1 )  ! tlon1 < tlon2
      call calc_intersection_sphere_normal_meridian(tlon1, tlat1, tlon2, tlat2, east, tlat_east)
    case( -1 )  ! tlon2 < tlon1
      call calc_intersection_sphere_normal_meridian(tlon2, tlat2, tlon1, tlat1, east, tlat_east)
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  tdir_lon: '//str(tdir_lon))
    endselect
  case( 3, 4 )
    if( id_east == 3 )then
      tlat_east = tlat1
    else
      tlat_east = tlat2
    endif

    selectcase( sdir_lon )
    case( 1 )  ! slon1 < slon2
      call calc_intersection_sphere_normal_meridian(slon1, slat1, slon2, slat2, east, slat_east)
    case( -1 )  ! slon2 < slon1
      call calc_intersection_sphere_normal_meridian(slon2, slat2, slon1, slat1, east, slat_east)
    case default
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_invalid_value())//&
              '\n  sdir_lon: '//str(sdir_lon))
    endselect
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  id_east: '//str(id_east))
  endselect

  if( debug )then
    call edbg('id_west: '//str(id_west)//' id_east: '//str(id_east))
    call edbg('west: '//str(west*r2d,'f12.7')//' east: '//str(east*r2d,'f12.7'))
    call edbg('slat: '//str((/slat_west,slat_east/)*r2d,'f12.7',', '))
    call edbg('tlat: '//str((/tlat_west,tlat_east/)*r2d,'f12.7',', '))
    call edbg('dir_lon s: '//str(sdir_lon)//' t: '//str(tdir_lon))
  endif
  !-------------------------------------------------------------
  ! Calc. area
  !-------------------------------------------------------------
  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  !       Calc. area of the zone above s and below t
  case( 1 )
    !-----------------------------------------------------------
    ! Case: s is above t
    !       No area
    if( slat_west >= tlat_west .and. slat_east >= tlat_east )then
      arc_rel = arc_rel_lat_norm_norm_above

      call pdbg_no('Case: s is above t')
    !-----------------------------------------------------------
    ! Case: s is below t
    !       Area is west to east
    elseif( slat_west <= tlat_west .and. slat_east <= tlat_east )then
      arc_rel = arc_rel_lat_norm_norm_below

      lon = londiff_rad(west, east)
      sarea = area_sphere_polartri(lon, slat_west, slat_east, sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_west, tlat_east, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct0('Case: s is below t', &
                      sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s intersects with t upward
    !       Area is west to intersection
    elseif( slat_west < tlat_west .and. slat_east > tlat_east )then
      arc_rel = arc_rel_lat_norm_norm_intersection_upward

      call calc_intersection_sphere_normal_normal(&
             sx1, sy1, sz1, sx2, sy2, sz2, sa, sb, sc, &
             tx1, ty1, tz1, tx2, ty2, tz2, ta, tb, tc, &
             west, east, clon, clat, stat)

      if( stat == 0 )then
        lon = londiff_rad(west, clon)
        sarea = area_sphere_polartri(lon, slat_west, clat, sgn_pole)
        tarea = area_sphere_polartri(lon, tlat_west, clat, sgn_pole)
        area = (sarea - tarea) * sgn

        call pdbg_isct1('Case: One intersection (upward)', &
                        clon, sarea, tarea, area)
      else
        area = 0.d0
      endif
    !-----------------------------------------------------------
    ! Case: s intersects with s downward
    !       Area is intersection to east
    elseif( slat_west > tlat_west .and. slat_east < tlat_east )then
      arc_rel = arc_rel_lat_norm_norm_intersection_downward

      call calc_intersection_sphere_normal_normal(&
             sx1, sy1, sz1, sx2, sy2, sz2, sa, sb, sc, &
             tx1, ty1, tz1, tx2, ty2, tz2, ta, tb, tc, &
             west, east, clon, clat, stat)

      if( stat == 0 )then
        lon = londiff_rad(east, clon)
        sarea = area_sphere_polartri(lon, slat_east, clat, sgn_pole)
        tarea = area_sphere_polartri(lon, tlat_east, clat, sgn_pole)
        area = (sarea - tarea) * sgn

        call pdbg_isct1('Case: One intersection (downward)', &
                        clon, sarea, tarea, area)
      else
        area = 0.d0
      endif
    !-----------------------------------------------------------
    ! Case: ERROR
    else
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_unexpected_condition())//&
              '\nNot in any case.')
    endif
  !-------------------------------------------------------------
  ! Case: Southward
  !       Calc. area of the zone below s and above t
  case( -1 )
    !-----------------------------------------------------------
    ! Case: s is below t
    !       No area
    if( slat_west <= tlat_west .and. slat_east <= tlat_east )then
      arc_rel = arc_rel_lat_norm_norm_below

      call pdbg_no('Case: s is below t')
    !-----------------------------------------------------------
    ! Case: s is above t
    !       Area is west to east
    elseif( slat_west >= tlat_west .and. slat_east >= tlat_east )then
      arc_rel = arc_rel_lat_norm_norm_above

      lon = londiff_rad(west, east)
      sarea = area_sphere_polartri(lon, slat_west, slat_east, sgn_pole)
      tarea = area_sphere_polartri(lon, tlat_west, tlat_east, sgn_pole)
      area = (sarea - tarea) * sgn

      call pdbg_isct0('Case: s is above t', &
                      sarea, tarea, area)
    !-----------------------------------------------------------
    ! Case: s intersects with t downward
    !       Area is west to intersection
    elseif( slat_west > tlat_west .and. slat_east < tlat_east )then
      arc_rel = arc_rel_lat_norm_norm_intersection_downward

      call calc_intersection_sphere_normal_normal(&
             sx1, sy1, sz1, sx2, sy2, sz2, sa, sb, sc, &
             tx1, ty1, tz1, tx2, ty2, tz2, ta, tb, tc, &
             west, east, clon, clat, stat)

      if( stat == 0 )then
        lon = londiff_rad(west, clon)
        sarea = area_sphere_polartri(lon, slat_west, clat, sgn_pole)
        tarea = area_sphere_polartri(lon, tlat_west, clat, sgn_pole)
        area = (sarea - tarea) * sgn

        call pdbg_isct1('Case: One intersection (downward)', &
                        clon, sarea, tarea, area)
      else
        area = 0.d0
      endif
    !-----------------------------------------------------------
    ! Case: s intersects with t upward
    !       Area is intersection to east
    elseif( slat_west < tlat_west .and. slat_east > tlat_east )then
      arc_rel = arc_rel_lat_norm_norm_intersection_upward

      call calc_intersection_sphere_normal_normal(&
             sx1, sy1, sz1, sx2, sy2, sz2, sa, sb, sc, &
             tx1, ty1, tz1, tx2, ty2, tz2, ta, tb, tc, &
             west, east, clon, clat, stat)

      if( stat == 0 )then
        lon = londiff_rad(east, clon)
        sarea = area_sphere_polartri(lon, slat_east, clat, sgn_pole)
        tarea = area_sphere_polartri(lon, tlat_east, clat, sgn_pole)
        area = (sarea - tarea) * sgn

        call pdbg_isct1('Case: One intersection (upward)', &
                        clon, sarea, tarea, area)
      else
        area = 0.d0
      endif
    !-----------------------------------------------------------
    ! Case: ERROR
    else
      if( .not. debug ) call echo(code%bgn, proc)
      call eerr(str(msg_unexpected_condition())//&
              '\nNot in any case.')
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    if( .not. debug ) call echo(code%bgn, proc)
    call eerr(str(msg_invalid_value())//&
            '\n  sgn_pole: '//str(sgn_pole))
  endselect
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ret)
  endif
end subroutine calc_area_sphere_normal_to_normal
!===============================================================
!
!===============================================================
subroutine pdbg_no(msg_case)
  implicit none
  character(*), intent(in) :: msg_case

  if( debug )then
    call edbg(msg_case)
  endif
end subroutine pdbg_no
!===============================================================
!
!===============================================================
subroutine pdbg_isct0(msg_case, sarea, tarea, area)
  implicit none
  character(*), intent(in) :: msg_case
  real(8)     , intent(in) :: sarea, tarea
  real(8)     , intent(in) :: area

  if( debug )then
    call edbg(msg_case//&
            '\n  sarea: '//str(sarea,'es20.13')//&
            '\n  tarea: '//str(tarea,'es20.13')//&
            '\n  area : '//str(area,'es20.13'))
  endif
end subroutine pdbg_isct0
!===============================================================
!
!===============================================================
subroutine pdbg_isct1(msg_case, clon, sarea, tarea, area)
  implicit none
  character(*), intent(in) :: msg_case
  real(8)     , intent(in) :: clon
  real(8)     , intent(in) :: sarea, tarea
  real(8)     , intent(in) :: area

  if( debug )then
    call edbg(msg_case//&
            '\n  clon : '//str(clon*r2d,'f12.7')//&
            '\n  sarea: '//str(sarea,'es20.13')//&
            '\n  tarea: '//str(tarea,'es20.13')//&
            '\n  area : '//str(area,'es20.13'))
  endif
end subroutine pdbg_isct1
!===============================================================
!
!===============================================================
subroutine pdbg_isct2_centered(&
    msg_case, clon1, clon2, sarea, tarea, area)
  implicit none
  character(*), intent(in) :: msg_case
  real(8)     , intent(in) :: clon1, clon2
  real(8)     , intent(in) :: sarea, tarea
  real(8)     , intent(in) :: area

  if( debug )then
    call edbg(msg_case//&
            '\n  clon1: '//str(clon1*r2d,'12.7')//&
            '\n  clon2: '//str(clon2*r2d,'12.7')//&
            '\n  sarea: '//str(sarea,'es20.13')//&
            '\n  tarea: '//str(tarea,'es20.13')//&
            '\n  area : '//str(area,'es20.13'))
  endif
end subroutine pdbg_isct2_centered
!===============================================================
!
!===============================================================
subroutine pdbg_isct2_splitted(&
    msg_case, &
    clon1, sarea1, tarea1, area1, &
    clon2, sarea2, tarea2, area2, area)
  implicit none
  character(*), intent(in) :: msg_case
  real(8)     , intent(in) :: clon1, clon2
  real(8)     , intent(in) :: sarea1, sarea2, tarea1, tarea2
  real(8)     , intent(in) :: area1, area2
  real(8)     , intent(in) :: area

  if( debug )then
    call edbg(msg_case//&
            '\n  clon1 : '//str(clon1*r2d,'12.7')//&
            '\n  sarea1: '//str(sarea1,'es20.13')//&
            '\n  tarea1: '//str(tarea1,'es20.13')//&
            '\n  area1 : '//str(area1,'es20.13')//&
            '\n  clon2 : '//str(clon2*r2d,'12.7')//&
            '\n  sarea2: '//str(sarea2,'es20.13')//&
            '\n  tarea2: '//str(tarea2,'es20.13')//&
            '\n  area2 : '//str(area2,'es20.13')//&
            '\n  area  : '//str(area,'es20.13'))
  endif
end subroutine pdbg_isct2_splitted
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
real(8) function area_sphere_intersection_polygon_polygon(&
    spos, sx, sy, sz, slon, slat, styp, sa, sb, sc, &
    sn_pole, sconvex, slontop, slattop, sarea, &
    tpos, tx, ty, tz, tlon, tlat, ttyp, ta, tb, tc, &
    tn_pole, tconvex, tlontop, tlattop, tarea) result(area)
  implicit none
  integer(1), intent(in) :: spos
  real(8)   , intent(in) :: sx(:), sy(:), sz(:)
  real(8)   , intent(in) :: slon(:), slat(:)
  integer(1), intent(in) :: styp(:)
  real(8)   , intent(in) :: sa(:), sb(:), sc(:)
  integer(4), intent(in) :: sn_pole
  integer(1), intent(in) :: sconvex(:)
  real(8)   , intent(in) :: slontop(:), slattop(:)
  real(8)   , intent(in) :: sarea
  integer(1), intent(in) :: tpos
  real(8)   , intent(in) :: tx(:), ty(:), tz(:)
  real(8)   , intent(in) :: tlon(:), tlat(:)
  integer(1), intent(in) :: ttyp(:)
  real(8)   , intent(in) :: ta(:), tb(:), tc(:)
  integer(4), intent(in) :: tn_pole
  integer(1), intent(in) :: tconvex(:)
  real(8)   , intent(in) :: tlontop(:), tlattop(:)
  real(8)   , intent(in) :: tarea

  integer    :: sgn_pole
  integer    :: snmax, sn, sn_
  integer    :: tnmax, tn, tn_
  real(8)    :: slon1, slon2, slat_pole
  real(8)    :: tlon1, tlon2, tlat_pole
  integer(1) :: arc_rel_lat
  real(8)    :: area_add
  logical    :: is_s_above_t, is_s_below_t
  logical    :: is_confirmed

  if( debug )then
    call echo(code%bgn, 'area_sphere_intersection_polygon_polygon')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  if( slat(1) > rad_0deg )then
    sgn_pole = 1
  else
    sgn_pole = -1
  endif

  area = 0.d0
  is_s_above_t = .true.
  is_s_below_t = .true.

  tnmax = size(tlon)
  snmax = size(slon)

  if( debug )then
    call edbg('sgn_pole: '//str(sgn_pole))
  endif
  !-------------------------------------------------------------
  ! Calc. intersection area
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ent, 'Calculating intersection area')
  endif

  tn_ = tnmax
  tn  = 1
  do while( tn <= tnmax )
    call pdbg_ent_arc('t', tn_, ttyp(tn_), tconvex(tn_), &
                      tlon(tn_), tlat(tn_), tlon(tn), tlat(tn))

    selectcase( ttyp(tn_) )
    !-----------------------------------------------------------
    ! Case: t is normal
    case( arc_type_normal )
      sn_ = snmax
      sn  = 1
      do while( sn <= snmax )
        call pdbg_ent_arc('s', sn_, styp(sn_), sconvex(sn_), &
                          slon(sn_), slat(sn_), slon(sn), slat(sn))

        selectcase( styp(sn_) )
        !-------------------------------------------------------
        ! Case: s is normal
        case( arc_type_normal )
          call calc_area_sphere_normal_to_normal(&
                   sx(sn_), sy(sn_), sz(sn_), sx(sn), sy(sn), sz(sn), &
                   slon(sn_), slat(sn_), slon(sn), slat(sn), &
                   sa(sn_), sb(sn_), sc(sn_), &
                   sconvex(sn_), slattop(sn_), &
                   tx(tn_), ty(tn_), tz(tn_), tx(tn), ty(tn), tz(tn), &
                   tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
                   ta(tn_), tb(tn_), tc(tn_), &
                   tconvex(tn_), tlattop(tn_), &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          call update_rel_lat_polygons_norm_norm(&
                   arc_rel_lat, is_s_below_t, is_s_above_t)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is parallel
        case( arc_type_parallel )
          call calc_area_sphere_parallel_to_normal(&
                   slon(sn_), slon(sn), slat(sn_), &
                   tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
                   ta(tn_), tb(tn_), tc(tn_), &
                   tconvex(tn_), tlontop(tn_), tlattop(tn_), &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          call update_rel_lat_polygons_para_norm(&
                   arc_rel_lat, is_s_below_t, is_s_above_t)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is meridian
        case( arc_type_meridian )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  styp(sn_): '//str(styp(sn_)))
        endselect
        !-------------------------------------------------------
        sn_ = sn
        sn  = sn + 1

        call pdbg_ext_arc()
      enddo  ! sn/
    !-----------------------------------------------------------
    ! Case: t is parallel
    case( arc_type_parallel )
      sn_ = snmax
      sn  = 1
      do while( sn <= snmax )
        call pdbg_ent_arc('s', sn_, styp(sn_), sconvex(sn_), &
                          slon(sn_), slat(sn_), slon(sn), slat(sn))

        selectcase( styp(sn_) )
        !-------------------------------------------------------
        ! Case: s is normal
        case( arc_type_normal )
          call calc_area_sphere_normal_to_parallel(&
                   slon(sn_), slat(sn_), slon(sn), slat(sn), &
                   sa(sn_), sb(sn_), sc(sn_), &
                   sconvex(sn_), slontop(sn_), slattop(sn_), &
                   tlon(tn_), tlon(tn), tlat(tn_), &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          call update_rel_lat_polygons_norm_para(&
                   arc_rel_lat, is_s_below_t, is_s_above_t)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is parallel
        case( arc_type_parallel )
          call calc_area_sphere_parallel_to_parallel(&
                   slon(sn_), slon(sn), slat(sn_), &
                   tlon(tn_), tlon(tn), tlat(tn_), &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          call update_rel_lat_polygons_para_para(&
                   arc_rel_lat, is_s_below_t, is_s_above_t)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is meridian
        case( arc_type_meridian )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  styp(sn_): '//str(styp(sn_)))
        endselect
        !-------------------------------------------------------
        sn_ = sn
        sn  = sn + 1

        call pdbg_ext_arc()
      enddo  ! sn/
    !-----------------------------------------------------------
    ! Case: t is meridian
    case( arc_type_meridian )
      continue
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  ttyp(tn_): '//str(ttyp(tn_)))
    endselect
    !-----------------------------------------------------------
    tn_ = tn
    tn  = tn + 1

    call pdbg_ext_arc()
  enddo  ! tn/

  if( debug )then
    call edbg('is_s_below_t: '//str(is_s_below_t))
    call edbg('is_s_above_t: '//str(is_s_above_t))
    call edbg('area: '//str(area,'es20.13'))
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Confirm area for special cases that 
  !   one is above, below, inside or outside the other
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ent, 'Confirming intersection area for special cases'//&
              ' that arcs do not intersect')
  endif

  is_confirmed = .false.

  if( is_s_below_t )then
    !-----------------------------------------------------------
    ! Case: s has north pole
    if( sn_pole /= 0 .and. slat(1) > rad_0deg )then
      continue
    !-----------------------------------------------------------
    ! Case: s has south pole
    elseif( sn_pole /= 0 .and. slat(1) < rad_0deg )then
      !---------------------------------------------------------
      ! Case: t has south pole
      if( tn_pole /= 0 .and. tlat(1) < rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: t includes south pole
      !       t includes s
      elseif( tpos == polygon_position_polar .and. tlat(1) < rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    !-----------------------------------------------------------
    ! Case: s includes north pole
    !       s includes t
    elseif( spos == polygon_position_polar .and. slat(1) > rad_0deg )then
      area = tarea
      is_confirmed = .true.
    !-----------------------------------------------------------
    ! Case: s includes south pole
    elseif( spos == polygon_position_polar .and. slat(1) < rad_0deg )then
      !---------------------------------------------------------
      ! Case: t has south pole
      if( tn_pole /= 0 .and. tlat(1) < rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: t includes south pole
      !       t includes s
      elseif( tpos == polygon_position_polar .and. tlat(1) < rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    !-----------------------------------------------------------
    ! Case: Others
    else
      !---------------------------------------------------------
      ! Case: t has south pole
      if( tn_pole /= 0 .and. tlat(1) < rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: t includes south pole
      !       t includes s
      elseif( tpos == polygon_position_polar .and. tlat(1) < rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
      !---------------------------------------------------------
    endif

  elseif( is_s_above_t )then
    !-----------------------------------------------------------
    ! Case: s has north pole
    if( sn_pole /= 0 .and. slat(1) > rad_0deg )then
      !---------------------------------------------------------
      ! Case: t has north pole
      if( tn_pole /= 0 .and. tlat(1) > rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: t includes north pole
      !       t includes s
      elseif( tpos == polygon_position_polar .and. tlat(1) > rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    !-----------------------------------------------------------
    ! Case: s has south pole
    elseif( sn_pole /= 0 .and. slat(1) < rad_0deg )then
      continue
    !-----------------------------------------------------------
    ! Case: s includes north pole
    elseif( spos == polygon_position_polar .and. slat(1) > rad_0deg )then
      !---------------------------------------------------------
      ! Case: t has north pole
      if( tn_pole /= 0 .and. tlat(1) > rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: t includes north pole
      !       t includes s
      elseif( tpos == polygon_position_polar .and. tlat(1) > rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    !-----------------------------------------------------------
    ! Case: s includes south pole
    elseif( spos == polygon_position_polar .and. slat(1) < rad_0deg )then
      area = tarea
      is_confirmed = .true.
    !-----------------------------------------------------------
    ! Case: Others
    else
      !---------------------------------------------------------
      ! Case: t has north pole
      if( tn_pole /= 0 .and. tlat(1) > rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: t includes north pole
      !       t includes s
      elseif( tpos == polygon_position_polar .and. tlat(1) > rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    endif
  endif

  if( debug )then
    if( is_confirmed )then
      call edbg('Confirmed: '//str(area,'es20.13'))
    else
      call edbg('Not confirmed')
    endif
    call echo(code%ext)
  endif

  if( is_confirmed )then
    if( debug )then
      call echo(code%ret)
    endif
    return
  endif
  !-------------------------------------------------------------
  ! Update area for special case that 
  ! grid includes pole or has pole on the vertex
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ent, 'Updating intersection area for special case that'//&
              ' pole is on the polygon')
  endif

  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  case( 1 )
    !-----------------------------------------------------------
    ! Case: t has north pole on its vertex
    if( tn_pole /= 0 .and. tlat(1) > rad_0deg )then
      if( debug )then
        call echo(code%ent, 't has north pole on its vertex')
      endif

      tlat_pole = rad_90deg

      if( tn_pole == 1 )then
        tlon1 = tlon(tnmax)
        tlon2 = tlon(tn_pole+1)
      elseif( tn_pole == tnmax )then
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(1)
      else
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(tn_pole+1)
      endif

      sn_ = snmax
      sn  = 1
      do while( sn <= snmax )
        call pdbg_ent_arc('s', sn_, styp(sn_), sconvex(sn_), &
                          slon(sn_), slat(sn_), slon(sn), slat(sn))

        selectcase( styp(sn_) )
        !-------------------------------------------------------
        ! Case: s is normal
        case( arc_type_normal )
          call calc_area_sphere_normal_to_parallel(&
                   slon(sn_), slat(sn_), slon(sn), slat(sn), &
                   sa(sn_), sb(sn_), sc(sn_), &
                   sconvex(sn_), slontop(sn_), slattop(sn_), &
                   tlon1, tlon2, tlat_pole, &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is parallel
        case( arc_type_parallel )
          call calc_area_sphere_parallel_to_parallel(&
                   slon(sn_), slon(sn), slat(sn_), &
                   tlon1, tlon2, tlat_pole, &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is meridian
        case( arc_type_meridian )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  styp('//str(sn_)//'): '//str(styp(sn_)))
        endselect
        !-------------------------------------------------------
        sn_ = sn
        sn  = sn + 1

        call pdbg_ext_arc()
      enddo  ! sn/

      if( debug )then
        call echo(code%ext)
      endif
    endif
    !-----------------------------------------------------------
    ! Case: t includes north pole
    if( tpos == polygon_position_polar .and. tlat(1) > rad_0deg )then
      if( debug )then
        call echo(code%ent, 't includes north pole')
      endif

      tlat_pole = rad_90deg

      sn_ = snmax
      sn  = 1
      do while( sn <= snmax )
        call pdbg_ent_arc('s', sn_, styp(sn_), sconvex(sn_), &
                          slon(sn_), slat(sn_), slon(sn), slat(sn))

        tlon1 = eastern(slon(sn_), slon(sn))
        tlon2 = western(slon(sn_), slon(sn))

        selectcase( styp(sn_) )
        !-------------------------------------------------------
        ! Case: s is normal
        case( arc_type_normal )
          call calc_area_sphere_normal_to_parallel(&
                   slon(sn_), slat(sn_), slon(sn), slat(sn), &
                   sa(sn_), sb(sn_), sc(sn_), &
                   sconvex(sn_), slontop(sn_), slattop(sn_), &
                   tlon1, tlon2, tlat_pole, &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is parallel
        case( arc_type_parallel )
          call calc_area_sphere_parallel_to_parallel(&
                   slon(sn_), slon(sn), slat(sn_), &
                   tlon1, tlon2, tlat_pole, &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is meridian
        case( arc_type_meridian )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  styp('//str(sn_)//'): '//str(styp(sn_)))
        endselect
        !-------------------------------------------------------
        sn_ = sn
        sn  = sn + 1

        call pdbg_ext_arc()
      enddo  ! sn/

      if( debug )then
        call echo(code%ext)
      endif
    endif
    !-----------------------------------------------------------
    ! Case: s has south pole
    if( sn_pole /= 0 .and. slat(1) < rad_0deg )then
      if( debug )then
        call echo(code%ent, 's has south pole on its vertex')
      endif

      slat_pole = -rad_90deg

      if( sn_pole == 1 )then
        slon1 = slon(snmax)
        slon2 = slon(sn_pole+1)
      elseif( sn_pole == snmax )then
        slon1 = slon(sn_pole-1)
        slon2 = slon(1)
      else
        slon1 = slon(sn_pole-1)
        slon2 = slon(sn_pole+1)
      endif

      tn_ = tnmax
      tn  = 1
      do while( tn <= tnmax )
        call pdbg_ent_arc('t', tn_, ttyp(tn_), tconvex(tn_), &
                          tlon(tn_), tlat(tn_), tlon(tn), tlat(tn))

        selectcase( ttyp(tn_) )
        !-------------------------------------------------------
        ! Case: t is normal
        case( arc_type_normal )
          call calc_area_sphere_parallel_to_normal(&
                   slon1, slon2, slat_pole, &
                   tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
                   ta(tn_), tb(tn_), tc(tn_), &
                   tconvex(tn_), tlontop(tn_), tlattop(tn_), &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is parallel
        case( arc_type_parallel )
          call calc_area_sphere_parallel_to_parallel(&
                   slon1, slon2, slat_pole, &
                   tlon(tn_), tlon(tn), tlat(tn_), &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is meridian
        case( arc_type_meridian )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  ttyp(tn_): '//str(ttyp(tn_)))
        endselect
        !-------------------------------------------------------
        tn_ = tn
        tn  = tn + 1

        call pdbg_ext_arc()
      enddo ! tn/

      if( debug )then
        call echo(code%ext)
      endif
    endif
    !-----------------------------------------------------------
    ! Case: s includes south pole
    if( spos == polygon_position_polar .and. slat(1) < rad_0deg )then
      if( debug )then
        call echo(code%ent, 's includes south pole')
      endif

      slat_pole = -rad_90deg

      tn_ = tnmax
      tn  = 1
      do while( tn <= tnmax )
        call pdbg_ent_arc('t', tn_, ttyp(tn_), tconvex(tn_), &
                          tlon(tn_), tlat(tn_), tlon(tn), tlat(tn))

        slon1 = western(tlon(tn_), tlon(tn))
        slon2 = eastern(tlon(tn_), tlon(tn))

        selectcase( ttyp(tn_) )
        !-------------------------------------------------------
        ! Case: t is normal
        case( arc_type_normal )
          call calc_area_sphere_parallel_to_normal(&
                   slon1, slon2, slat_pole, &
                   tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
                   ta(tn_), tb(tn_), tc(tn_), &
                   tconvex(tn_), tlontop(tn_), tlattop(tn_), &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is parallel
        case( arc_type_parallel )
          call calc_area_sphere_parallel_to_parallel(&
                   slon1, slon2, slat_pole, &
                   tlon(tn_), tlon(tn), tlat(tn_), &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is meridian
        case( arc_type_meridian )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  ttyp(tn_): '//str(ttyp(tn_)))
        endselect
        !-------------------------------------------------------
        tn_ = tn
        tn  = tn + 1

        call pdbg_ext_arc()
      enddo  ! tn/

      if( debug )then
        call echo(code%ext)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Southward
  case( -1 )
    !-----------------------------------------------------------
    ! Case: t has south pole
    if( tn_pole /= 0 .and. tlat(1) < rad_0deg )then
      if( debug )then
        call echo(code%ent, 't has south pole on its vertex')
      endif

      tlat_pole = -rad_90deg

      if( tn_pole == 1 )then
        tlon1 = tlon(tnmax)
        tlon2 = tlon(tn_pole+1)
      elseif( tn_pole == tnmax )then
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(1)
      else
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(tn_pole+1)
      endif

      sn_ = snmax
      sn  = 1
      do while( sn <= snmax )
        call pdbg_ent_arc('s', sn_, styp(sn_), sconvex(sn_), &
                          slon(sn_), slat(sn_), slon(sn), slat(sn))

        selectcase( styp(sn_) )
        !-------------------------------------------------------
        ! Case: s is normal
        case( arc_type_normal )
          call calc_area_sphere_normal_to_parallel(&
                   slon(sn_), slat(sn_), slon(sn), slat(sn), &
                   sa(sn_), sb(sn_), sc(sn_), &
                   sconvex(sn_), slontop(sn_), slattop(sn_), &
                   tlon1, tlon2, tlat_pole, &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is parallel
        case( arc_type_parallel )
          call calc_area_sphere_parallel_to_parallel(&
                   slon(sn_), slon(sn), slat(sn_), &
                   tlon1, tlon2, tlat_pole, &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is meridian
        case( arc_type_meridian )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  styp('//str(sn_)//'): '//str(styp(sn_)))
        endselect
        !-------------------------------------------------------
        sn_ = sn
        sn  = sn + 1

        call pdbg_ext_arc()
      enddo  ! sn/

      if( debug )then
        call echo(code%ext)
      endif
    endif
    !-----------------------------------------------------------
    ! Case: t includes south pole
    if( tpos == polygon_position_polar .and. tlat(1) < rad_0deg )then
      if( debug )then
        call echo(code%ent, 't includes south pole')
      endif

      tlat_pole = -rad_90deg

      sn_ = snmax
      sn  = 1
      do while( sn <= snmax )
        call pdbg_ent_arc('s', sn_, styp(sn_), sconvex(sn_), &
                          slon(sn_), slat(sn_), slon(sn), slat(sn))

        tlon1 = western(slon(sn_), slon(sn))
        tlon2 = eastern(slon(sn_), slon(sn))

        selectcase( styp(sn_) )
        !-------------------------------------------------------
        ! Case: s is normal
        case( arc_type_normal )
          call calc_area_sphere_normal_to_parallel(&
                   slon(sn_), slat(sn_), slon(sn), slat(sn), &
                   sa(sn_), sb(sn_), sc(sn_), &
                   sconvex(sn_), slontop(sn_), slattop(sn_), &
                   tlon1, tlon2, tlat_pole, &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is parallel
        case( arc_type_parallel )
          call calc_area_sphere_parallel_to_parallel(&
                   slon(sn_), slon(sn), slat(sn_), &
                   tlon1, tlon2, tlat_pole, &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: s is meridian
        case( arc_type_meridian )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  styp('//str(sn_)//'): '//str(styp(sn_)))
        endselect
        !-------------------------------------------------------
        sn_ = sn
        sn  = sn + 1

        call pdbg_ext_arc()
      enddo  ! sn/

      if( debug )then
        call echo(code%ext)
      endif
    endif
    !-----------------------------------------------------------
    ! Case: s has north pole
    if( sn_pole /= 0 .and. slat(1) > rad_0deg )then
      if( debug )then
        call echo(code%ent, 's has north pole on its vertex')
      endif

      slat_pole = rad_90deg

      if( sn_pole == 1 )then
        slon1 = slon(snmax)
        slon2 = slon(sn_pole+1)
      elseif( sn_pole == snmax )then
        slon1 = slon(sn_pole-1)
        slon2 = slon(1)
      else
        slon1 = slon(sn_pole-1)
        slon2 = slon(sn_pole+1)
      endif

      tn_ = tnmax
      tn  = 1
      do while( tn <= tnmax )
        call pdbg_ent_arc('t', tn_, ttyp(tn_), tconvex(tn_), &
                          tlon(tn_), tlat(tn_), tlon(tn), tlat(tn))

        selectcase( ttyp(tn_) )
        !-------------------------------------------------------
        ! Case: t is normal
        case( arc_type_normal )
          call calc_area_sphere_parallel_to_normal(&
                   slon1, slon2, slat_pole, &
                   tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
                   ta(tn_), tb(tn_), tc(tn_), &
                   tconvex(tn_), tlontop(tn_), tlattop(tn_), &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is parallel
        case( arc_type_parallel )
          call calc_area_sphere_parallel_to_parallel(&
                   slon1, slon2, slat_pole, &
                   tlon(tn_), tlon(tn), tlat(tn_), &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is meridian
        case( arc_type_meridian )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  ttyp(tn_): '//str(ttyp(tn_)))
        endselect
        !-------------------------------------------------------
        tn_ = tn
        tn  = tn + 1

        call pdbg_ext_arc()
      enddo ! tn/

      if( debug )then
        call echo(code%ext)
      endif
    endif
    !-----------------------------------------------------------
    ! Case: s includes north pole
    if( spos == polygon_position_polar .and. slat(1) > rad_0deg )then
      if( debug )then
        call echo(code%ent, 's includes north pole')
      endif

      slat_pole = rad_90deg

      tn_ = tnmax
      tn  = 1
      do while( tn <= tnmax )
        call pdbg_ent_arc('t', tn_, ttyp(tn_), tconvex(tn_), &
                          tlon(tn_), tlat(tn_), tlon(tn), tlat(tn))

        slon1 = eastern(tlon(tn_), tlon(tn))
        slon2 = western(tlon(tn_), tlon(tn))

        selectcase( ttyp(tn_) )
        !-------------------------------------------------------
        ! Case: t is normal
        case( arc_type_normal )
          call calc_area_sphere_parallel_to_normal(&
                   slon1, slon2, slat_pole, &
                   tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
                   ta(tn_), tb(tn_), tc(tn_), &
                   tconvex(tn_), tlontop(tn_), tlattop(tn_), &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is parallel
        case( arc_type_parallel )
          call calc_area_sphere_parallel_to_parallel(&
                   slon1, slon2, slat_pole, &
                   tlon(tn_), tlon(tn), tlat(tn_), &
                   sgn_pole, &
                   area_add, arc_rel_lat)

          area = area + area_add
        !-------------------------------------------------------
        ! Case: t is meridian
        case( arc_type_meridian )
          continue
        !-------------------------------------------------------
        ! Case: ERROR
        case default
          call eerr(str(msg_invalid_value())//&
                  '\n  ttyp(tn_): '//str(ttyp(tn_)))
        endselect
        !-------------------------------------------------------
        tn_ = tn
        tn  = tn + 1

        call pdbg_ext_arc()
      enddo  ! tn/

      if( debug )then
        call echo(code%ext)
      endif
    endif
  endselect

  if( debug )then
    call edbg('area: '//str(area,'es20.13'))
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ret)
  endif
end function area_sphere_intersection_polygon_polygon
!===============================================================
!
!===============================================================
real(8) function area_sphere_intersection_latlon_polygon(&
    swest, seast, ssouth, snorth, sarea, &
    tpos, tlon, tlat, ttyp, ta, tb, tc, &
    tn_pole, tconvex, tlontop, tlattop) result(area)
  implicit none
  real(8)   , intent(in) :: swest, seast, ssouth, snorth
  real(8)   , intent(in) :: sarea
  integer(1), intent(in) :: tpos
  real(8)   , intent(in) :: tlon(:), tlat(:)  ! (tnmax)
  integer(1), intent(in) :: ttyp(:)
  real(8)   , intent(in) :: ta(:), tb(:), tc(:)
  integer(4), intent(in) :: tn_pole
  integer(1), intent(in) :: tconvex(:)
  real(8)   , intent(in) :: tlontop(:), tlattop(:)

  integer    :: sgn_pole
  integer    :: tnmax, tn, tn_
  real(8)    :: tlon1, tlon2, tlat_pole
  integer(1) :: arc_rel_lat
  real(8)    :: area_add_south, area_add_north
  logical    :: is_s_above_t, is_s_below_t
  logical    :: is_confirmed


  if( debug )then
    call echo(code%bgn, 'area_sphere_intersection_latlon_polygon')
  endif
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  sgn_pole = 1

  area = 0.d0
  is_s_below_t = .true.
  is_s_above_t = .true.

  tnmax = size(tlon)

  if( debug )then
    call edbg('sgn_pole: '//str(sgn_pole))
  endif
  !-------------------------------------------------------------
  ! Calc. intersection area
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ent, 'Calculating intersection area')
  endif

  tn_ = tnmax
  tn  = 1
  do while( tn <= tnmax )
    call pdbg_ent_arc('t', tn_, ttyp(tn_), tconvex(tn_), &
                      tlon(tn_), tlat(tn_), tlon(tn), tlat(tn))

    selectcase( ttyp(tn_) )
    !-----------------------------------------------------------
    ! Case: t is normal
    case( arc_type_normal )
      !---------------------------------------------------------
      ! South
      !---------------------------------------------------------
      call pdbg_ent_arc('s', 1, arc_type_parallel, arc_convex_monotone, &
                        swest, ssouth, seast, ssouth)

      call calc_area_sphere_parallel_to_normal(&
               swest, seast, ssouth, &
               tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
               ta(tn_), tb(tn_), tc(tn_), &
               tconvex(tn_), tlontop(tn_), tlattop(tn_), &
               sgn_pole, &
               area_add_south, arc_rel_lat)

      call update_rel_lat_polygons_para_norm(&
               arc_rel_lat, is_s_below_t, is_s_above_t)

      call pdbg_ext_arc()
      !---------------------------------------------------------
      ! North
      !---------------------------------------------------------
      call pdbg_ent_arc('s', 3, arc_type_parallel, arc_convex_monotone, &
                        seast, snorth, swest, snorth)

      call calc_area_sphere_parallel_to_normal(&
               seast, swest, snorth, &
               tlon(tn_), tlat(tn_), tlon(tn), tlat(tn), &
               ta(tn_), tb(tn_), tc(tn_), &
               tconvex(tn_), tlontop(tn_), tlattop(tn_), &
               sgn_pole, &
               area_add_north, arc_rel_lat)

      call update_rel_lat_polygons_para_norm(&
               arc_rel_lat, is_s_below_t, is_s_above_t)

      call pdbg_ext_arc()
      !---------------------------------------------------------
      area = area + (area_add_south + area_add_north)
    !-----------------------------------------------------------
    ! Case: Parallel
    case( arc_type_parallel )
      call calc_area_sphere_parallel_to_parallel(&
               swest, seast, ssouth, &
               tlon(tn_), tlon(tn), tlat(tn_), &
               sgn_pole, &
               area_add_south, arc_rel_lat)

      call update_rel_lat_polygons_para_para(&
               arc_rel_lat, is_s_below_t, is_s_above_t)

      call calc_area_sphere_parallel_to_parallel(&
               seast, swest, snorth, &
               tlon(tn_), tlon(tn), tlat(tn_), &
               sgn_pole, &
               area_add_north, arc_rel_lat)

      call update_rel_lat_polygons_para_para(&
               arc_rel_lat, is_s_below_t, is_s_above_t)

      area = area + (area_add_south + area_add_north)
    !-----------------------------------------------------------
    ! Case: Meridian
    case( arc_type_meridian )
      continue
    !-----------------------------------------------------------
    ! Case: ERROR
    case default
      call eerr(str(msg_invalid_value())//&
              '\n  ttyp(tn_): '//str(ttyp(tn_))//&
              '\n  tn_: '//str(tn_))
    endselect
    !-----------------------------------------------------------
    tn_ = tn
    tn  = tn + 1

    call pdbg_ext_arc()
  enddo  ! tn/

  if( debug )then
    call edbg('area: '//str(area,'es20.13'))
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  ! Confirm area for special cases that 
  !   one is above, below, inside or outside the other
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ent, 'Confirming intersection area for special cases'//&
              ' that arcs do not intersect')
  endif

  is_confirmed = .false.

  if( is_s_below_t )then
    !-----------------------------------------------------------
    ! Case: s has north pole
    if( snorth == rad_90deg )then
      continue
    !-----------------------------------------------------------
    ! Case: Others
    else
      !---------------------------------------------------------
      ! Case: t includes south pole
      !       t includes s
      if( tpos == polygon_position_polar .and. tlat(1) < rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: t has south pole
      elseif( tn_pole /= 0 .and. tlat(1) < rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    endif
  elseif( is_s_above_t )then
    !-----------------------------------------------------------
    ! Case: s has south pole
    if( ssouth == -rad_90deg )then
      continue
    !-----------------------------------------------------------
    ! Case: Others
    else
      !---------------------------------------------------------
      ! Case: t includes north pole
      !       t includes s
      if( tpos == polygon_position_polar .and. tlat(1) > rad_0deg )then
        area = sarea
        is_confirmed = .true.
      !---------------------------------------------------------
      ! Case: t has north pole
      elseif( tn_pole /= 0 .and. tlat(1) > rad_0deg )then
        continue
      !---------------------------------------------------------
      ! Case: Others
      !       Not intersect
      else
        area = 0.d0
        is_confirmed = .true.
      endif
    endif
  endif

  if( debug )then
    if( is_confirmed )then
      call edbg('Confirmed: '//str(area,'es20.13'))
    else
      call edbg('Not confirmed')
    endif
    call echo(code%ext)
  endif

  if( is_confirmed )then
    if( debug )then
      call echo(code%ret)
    endif
    return
  endif
  !-------------------------------------------------------------
  ! Update area for special case that 
  ! grid includes pole or has pole on the vertex
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ent, 'Updating intersection area for special case that'//&
              ' pole is on the polygon')
  endif

  selectcase( sgn_pole )
  !-------------------------------------------------------------
  ! Case: Northward
  case( 1 )
    !-----------------------------------------------------------
    ! Case: t has north pole
    if( tn_pole /= 0 .and. tlat(1) > rad_0deg )then
      if( debug )then
        call echo(code%ent, 'Case: t has north pole on its vertex')
      endif

      tlat_pole = rad_90deg

      if( tn_pole == 1 )then
        tlon1 = tlon(tnmax)
        tlon2 = tlon(tn_pole+1)
      elseif( tn_pole == tnmax )then
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(1)
      else
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(tn_pole+1)
      endif

      call calc_area_sphere_parallel_to_parallel(&
               swest, seast, ssouth, &
               tlon1, tlon2, tlat_pole, &
               sgn_pole, &
               area_add_south, arc_rel_lat)

      call calc_area_sphere_parallel_to_parallel(&
               seast, swest, snorth, &
               tlon1, tlon2, tlat_pole, &
               sgn_pole, &
               area_add_north, arc_rel_lat)

      area = area + (area_add_south + area_add_north)

      if( debug )then
        call echo(code%ext)
      endif
    !-----------------------------------------------------------
    ! Case: t includes north pole
    elseif( tpos == polygon_position_polar .and. tlat(1) > rad_0deg )then
      if( debug )then
        call echo(code%ent, 't includes north pole')
      endif

      area = area + sarea

      if( debug )then
        call echo(code%ext)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: Southward
  case( -1 )
    !-----------------------------------------------------------
    ! Case: t has south pole
    if( tn_pole /= 0 .and. tlat(1) < rad_0deg )then
      if( debug )then
        call echo(code%ent, 't has south pole on its vertex')
      endif

      tlat_pole = -rad_90deg

      if( tn_pole == 1 )then
        tlon1 = tlon(tnmax)
        tlon2 = tlon(tn_pole+1)
      elseif( tn_pole == tnmax )then
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(1)
      else
        tlon1 = tlon(tn_pole-1)
        tlon2 = tlon(tn_pole+1)
      endif

      call calc_area_sphere_parallel_to_parallel(&
               swest, seast, ssouth, &
               tlon1, tlon2, tlat_pole, &
               sgn_pole, &
               area_add_south, arc_rel_lat)

      call calc_area_sphere_parallel_to_parallel(&
               seast, swest, snorth, &
               tlon1, tlon2, tlat_pole, &
               sgn_pole, &
               area_add_north, arc_rel_lat)

      area = area + (area_add_south + area_add_north)

      if( debug )then
        call echo(code%ext)
      endif
    !-----------------------------------------------------------
    ! Case: t includes south pole
    elseif( tpos == polygon_position_polar .and. tlat(1) < rad_0deg )then
      if( debug )then
        call echo(code%ent, 't includes south pole')
      endif

      area = area + sarea

      if( debug )then
        call echo(code%ext)
      endif
    endif
  !-------------------------------------------------------------
  ! Case: ERROR
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  sgn_pole: '//str(sgn_pole))
  endselect

  if( debug )then
    call echo(code%ext)
  endif
  !-------------------------------------------------------------
  if( debug )then
    call echo(code%ret)
  endif
end function area_sphere_intersection_latlon_polygon
!===============================================================
!
!===============================================================
subroutine update_rel_lat_polygons_norm_norm(&
    arc_rel_lat_norm_norm, is_former_below_latter, is_former_above_latter)
  implicit none
  integer(1), intent(in)    :: arc_rel_lat_norm_norm
  logical   , intent(inout) :: is_former_below_latter, is_former_above_latter

  selectcase( arc_rel_lat_norm_norm )
  case( arc_rel_lat_norm_norm_undef )
    continue
  case( arc_rel_lat_norm_norm_below )  ! s is below t
    is_former_above_latter = .false.
  case( arc_rel_lat_norm_norm_above )  ! s is above t
    is_former_below_latter = .false.
  case( arc_rel_lat_norm_norm_intersection_upward, &
        arc_rel_lat_norm_norm_intersection_downward )
    is_former_below_latter = .false.
    is_former_above_latter = .false.
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  arc_rel_lat_norm_norm: '//str(arc_rel_lat_norm_norm))
  endselect
end subroutine update_rel_lat_polygons_norm_norm
!===============================================================
!
!===============================================================
subroutine update_rel_lat_polygons_para_norm(&
    arc_rel_lat_para_norm, is_former_below_latter, is_former_above_latter)
  implicit none
  integer(1), intent(in)    :: arc_rel_lat_para_norm
  logical   , intent(inout) :: is_former_below_latter, is_former_above_latter

  selectcase( arc_rel_lat_para_norm )
  case( arc_rel_lat_para_norm_undef )
    continue
  case( arc_rel_lat_para_norm_below )
    is_former_above_latter = .false.
  case( arc_rel_lat_para_norm_above )
    is_former_below_latter = .false.
  case( arc_rel_lat_para_norm_one_intersection_upward, &
        arc_rel_lat_para_norm_one_intersection_downward, &
        arc_rel_lat_para_norm_two_intersections_convex_upward, &
        arc_rel_lat_para_norm_two_intersections_convex_downward )
    is_former_below_latter = .false.
    is_former_above_latter = .false.
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  arc_rel_lat_para_norm: '//str(arc_rel_lat_para_norm))
  endselect
end subroutine update_rel_lat_polygons_para_norm
!===============================================================
!
!===============================================================
subroutine update_rel_lat_polygons_norm_para(&
    arc_rel_lat_norm_para, is_former_below_latter, is_former_above_latter)
  implicit none
  integer(1), intent(in)    :: arc_rel_lat_norm_para
  logical   , intent(inout) :: is_former_below_latter, is_former_above_latter

  selectcase( arc_rel_lat_norm_para )
  case( arc_rel_lat_norm_para_undef )
    continue
  case( arc_rel_lat_norm_para_below )
    is_former_above_latter = .false.
  case( arc_rel_lat_norm_para_above )
    is_former_below_latter = .false.
  case( arc_rel_lat_norm_para_one_intersection_upward, &
        arc_rel_lat_norm_para_one_intersection_downward, &
        arc_rel_lat_norm_para_two_intersections_convex_upward, &
        arc_rel_lat_norm_para_two_intersections_convex_downward )
    is_former_below_latter = .false.
    is_former_above_latter = .false.
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  arc_rel_lat_norm_para: '//str(arc_rel_lat_norm_para))
  endselect
end subroutine update_rel_lat_polygons_norm_para
!===============================================================
!
!===============================================================
subroutine update_rel_lat_polygons_para_para(&
    arc_rel_lat_para_para, is_former_below_latter, is_former_above_latter)
  implicit none
  integer(1), intent(in)    :: arc_rel_lat_para_para
  logical   , intent(inout) :: is_former_below_latter, is_former_above_latter

  selectcase( arc_rel_lat_para_para )
  case( arc_rel_lat_para_para_undef )
    continue
  case( arc_rel_lat_para_para_below )
    is_former_above_latter = .false.
  case( arc_rel_lat_para_para_above )
    is_former_below_latter = .false.
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  arc_rel_lat_para_para: '//str(arc_rel_lat_para_para))
  endselect
end subroutine update_rel_lat_polygons_para_para
!===============================================================
!
!===============================================================
subroutine pdbg_ent_arc(nam, n, arctyp, convex, lon1, lat1, lon2, lat2)
  implicit none
  character(*), intent(in) :: nam
  integer     , intent(in) :: n
  integer(1)  , intent(in) :: arctyp
  integer(1)  , intent(in) :: convex
  real(8)     , intent(in) :: lon1, lat1
  real(8)     , intent(in) :: lon2, lat2
  character(26) :: s1, s2

  if( debug )then
    call echo(code%ent, nam//'('//str(n)//') type '//str(str_arctyp_long(arctyp))//&
              ' convex '//str(str_convex_long(convex)))

    if( abs(lat1) == rad_90deg )then
      s1 = str('-',12)//', '//str(lat1*r2d,'f12.7')
    else
      s1 = str((/lon1,lat1/)*r2d,'f12.7',', ')
    endif

    if( abs(lat2) == rad_90deg )then
      s2 = str('-',12)//', '//str(lat2*r2d,'f12.7')
    else
      s2 = str((/lon2,lat2/)*r2d,'f12.7',', ')
    endif

    call edbg('('//s1//') - ('//s2//')')
  endif
end subroutine pdbg_ent_arc
!===============================================================
!
!===============================================================
subroutine pdbg_ext_arc()
  implicit none

  if( debug )then
    call echo(code%ext)
  endif
end subroutine pdbg_ext_arc
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
real(8) function dist_sphere_0d(lon1, lat1, lon2, lat2) result(dist)
  implicit none
  real(8), intent(in) :: lon1, lat1, lon2, lat2  ![rad]

  dist = acos(min(1.d0, max(-1.d0, &
              sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(londiff_rad(lon1, lon2))))) * 2.d0
end function dist_sphere_0d
!===============================================================
!
!===============================================================
end module lib_math_sphere
