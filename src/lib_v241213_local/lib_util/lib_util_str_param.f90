module lib_util_str_param
  use lib_const
  use lib_base
  use lib_log
  implicit none
  private
  !-------------------------------------------------------------
  public :: str_arctyp_long
  public :: str_arctyp_short

  public :: str_arcpos_long
  public :: str_arcpos_short

  public :: str_arc_rel_lat

  public :: str_convex_long
  public :: str_convex_short

  public :: str_polygon_pos_long

  public :: str_zone_type_long
  !-------------------------------------------------------------
  interface str_arctyp_long
    module procedure str_arctyp_long_0d
    module procedure str_arctyp_long_1d
  end interface

  interface str_arctyp_short
    module procedure str_arctyp_short_0d
    module procedure str_arctyp_short_1d
  end interface

  interface str_arcpos_long
    module procedure str_arcpos_long_0d
    module procedure str_arcpos_long_1d
  end interface

  interface str_arcpos_short
    module procedure str_arcpos_short_0d
    module procedure str_arcpos_short_1d
  end interface

  interface str_convex_long
    module procedure str_convex_long_0d
    module procedure str_convex_long_1d
  end interface

  interface str_convex_short
    module procedure str_convex_short_0d
    module procedure str_convex_short_1d
  end interface

  interface str_polygon_pos_long
    module procedure str_polygon_pos_long_0d
  end interface

  interface str_zone_type_long
    module procedure str_zone_type_long_0d
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
function str_arctyp_long_0d(typ) result(res)
  implicit none
  integer(1), intent(in) :: typ
  character(clen_key) :: res

  call echo(code%bgn, 'str_arctyp_long__MP__str_arctyp_long_0d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( typ )
  case( arc_type_undef )
    res = 'undef'
  case( arc_type_normal )
    res = 'normal'
  case( arc_type_parallel )
    res = 'parallel'
  case( arc_type_meridian )
    res = 'meridian'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  typ: '//str(typ))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arctyp_long_0d
!===============================================================
!
!===============================================================
function str_arctyp_long_1d(typ) result(res)
  implicit none
  integer(1), intent(in) :: typ(:)
  character(clen_key) :: res(size(typ))

  integer :: i

  call echo(code%bgn, 'str_arctyp_long__MP__str_arctyp_long_1d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do i = 1, size(typ)
    res(i) = str_arctyp_long_0d(typ(i))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arctyp_long_1d
!===============================================================
!
!===============================================================
function str_arctyp_short_0d(typ) result(res)
  implicit none
  integer(1), intent(in) :: typ
  character(1) :: res

  call echo(code%bgn, 'str_arctyp_short__MP__str_arctyp_short_0d', '-p -x2')
  !-------------------------------------------------------------
  selectcase( typ )
  case( arc_type_undef )
    res = 'u'
  case( arc_type_normal )
    res = 'n'
  case( arc_type_parallel )
    res = 'p'
  case( arc_type_meridian )
    res = 'm'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  typ: '//str(typ))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arctyp_short_0d
!===============================================================
!
!===============================================================
function str_arctyp_short_1d(typ) result(res)
  implicit none
  integer(1), intent(in) :: typ(:)
  character(1) :: res(size(typ))

  integer :: i

  call echo(code%bgn, 'str_arctyp_short__MP__str_arctyp_short_1d', '-p -x2')
  !-------------------------------------------------------------
  do i = 1, size(typ)
    res(i) = str_arctyp_short_0d(typ(i))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arctyp_short_1d
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
function str_arcpos_long_0d(pos) result(res)
  implicit none
  integer(1), intent(in) :: pos
  character(clen_key) :: res

  call echo(code%bgn, 'str_arcpos_long__MP__str_arcpos_long_0d', '-p -x2')
  !-------------------------------------------------------------
  selectcase( pos )
  case( arc_position_undef )
    res = 'undef'
  case( arc_position_normal )
    res = 'normal'
  case( arc_position_lon0 )
    res = 'lon0'
  case( arc_position_polar )
    res = 'polar'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  pos: '//str(pos))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arcpos_long_0d
!===============================================================
!
!===============================================================
function str_arcpos_long_1d(pos) result(res)
  implicit none
  integer(1), intent(in) :: pos(:)
  character(clen_key) :: res(size(pos))

  integer :: i

  call echo(code%bgn, 'str_arcpos_long__MP__str_arcpos_long_1d', '-p -x2')
  !-------------------------------------------------------------
  do i = 1, size(pos)
    res(i) = str_arcpos_long_0d(pos(i))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arcpos_long_1d
!===============================================================
!
!===============================================================
function str_arcpos_short_0d(pos) result(res)
  implicit none
  integer(1), intent(in) :: pos
  character(1) :: res

  call echo(code%bgn, 'str_arcpos_short__MP__str_arcpos_short_0d', '-p -x2')
  !-------------------------------------------------------------
  selectcase( pos )
  case( arc_position_undef )
    res = 'u'
  case( arc_position_normal )
    res = 'n'
  case( arc_position_lon0 )
    res = 'l'
  case( arc_position_polar )
    res = 'p'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  pos: '//str(pos))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arcpos_short_0d
!===============================================================
!
!===============================================================
function str_arcpos_short_1d(pos) result(res)
  implicit none
  integer(1), intent(in) :: pos(:)
  character(1) :: res(size(pos))

  integer :: i

  call echo(code%bgn, 'str_arcpos_short__MP__str_arcpos_short_1d', '-p -x2')
  !-------------------------------------------------------------
  do i = 1, size(pos)
    res(i) = str_arcpos_short_0d(pos(i))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arcpos_short_1d
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
function str_arc_rel_lat(arc_rel) result(res)
  implicit none
  integer(1)   , intent(in) :: arc_rel
  character(64)             :: res

  call echo(code%bgn, 'str_arc_rel_lat', '-p -x2')
  !-------------------------------------------------------------
  selectcase( arc_rel )
  case( arc_rel_lat_para_para_undef )
    res = 'para_para_undef'
  case( arc_rel_lat_para_para_below )
    res = 'para_para_below'
  case( arc_rel_lat_para_para_above )
    res = 'para_para_above'
  case( arc_rel_lat_para_norm_undef )
    res = 'para_norm_undef'
  case( arc_rel_lat_para_norm_below )
    res = 'para_norm_below'
  case( arc_rel_lat_para_norm_above )
    res = 'para_norm_above'
  case( arc_rel_lat_para_norm_one_intersection_upward )
    res = 'para_norm_one_intersection_upward'
  case( arc_rel_lat_para_norm_one_intersection_downward )
    res = 'para_norm_one_intersection_downward'
  case( arc_rel_lat_para_norm_two_intersections_convex_upward )
    res = 'para_norm_two_intersections_convex_upward'
  case( arc_rel_lat_para_norm_two_intersections_convex_downward )
    res = 'para_norm_two_intersections_convex_downward'
  case( arc_rel_lat_norm_para_undef )
    res = 'norm_para_undef'
  case( arc_rel_lat_norm_para_below )
    res = 'norm_para_below'
  case( arc_rel_lat_norm_para_above )
    res = 'norm_para_above'
  case( arc_rel_lat_norm_para_one_intersection_upward )
    res = 'norm_para_one_intersection_upward'
  case( arc_rel_lat_norm_para_one_intersection_downward )
    res = 'norm_para_one_intersection_downward'
  case( arc_rel_lat_norm_para_two_intersections_convex_upward )
    res = 'norm_para_two_intersections_convex_upward'
  case( arc_rel_lat_norm_para_two_intersections_convex_downward )
    res = 'norm_para_two_intersections_convex_downward'
  case( arc_rel_lat_norm_norm_undef )
    res = 'norm_norm_undef'
  case( arc_rel_lat_norm_norm_below )
    res = 'norm_norm_below'
  case( arc_rel_lat_norm_norm_above )
    res = 'norm_norm_above'
  case( arc_rel_lat_norm_norm_intersection_upward )
    res = 'norm_norm_intersection_upward'
  case( arc_rel_lat_norm_norm_intersection_downward )
    res = 'norm_norm_intersection_downward'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  arc_rel: '//str(arc_rel))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_arc_rel_lat
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
function str_convex_long_0d(convex) result(res)
  implicit none
  integer(1), intent(in) :: convex
  character(clen_key) :: res

  call echo(code%bgn, 'str_convex_long__MP__str_convex_long_0d', '-p -x2')
  !-------------------------------------------------------------
  selectcase( convex )
  case( arc_convex_undef )
    res = 'undef'
  case( arc_convex_monotone )
    res = 'monotone'
  case( arc_convex_upward )
    res = 'upward'
  case( arc_convex_downward )
    res = 'downward'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  convex: '//str(convex))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_convex_long_0d
!===============================================================
!
!===============================================================
function str_convex_long_1d(convex) result(res)
  implicit none
  integer(1), intent(in) :: convex(:)
  character(clen_key) :: res(size(convex))

  integer :: i

  call echo(code%bgn, 'str_convex_long__MP__str_convex_long_1d', '-p -x2')
  !-------------------------------------------------------------
  do i = 1, size(convex)
    res(i) = str_convex_long_0d(convex(i))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_convex_long_1d
!===============================================================
!
!===============================================================
function str_convex_short_0d(convex) result(res)
  implicit none
  integer(1), intent(in) :: convex
  character(clen_key) :: res

  call echo(code%bgn, 'str_convex_short__MP__str_convex_short_0d', '-p -x2')
  !-------------------------------------------------------------
  selectcase( convex )
  case( arc_convex_undef )
    res = '-'
  case( arc_convex_monotone )
    res = 'm'
  case( arc_convex_downward )
    res = 'd'
  case( arc_convex_upward )
    res = 'u'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  convex: '//str(convex))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_convex_short_0d
!===============================================================
!
!===============================================================
function str_convex_short_1d(convex) result(res)
  implicit none
  integer(1), intent(in) :: convex(:)
  character(clen_key) :: res(size(convex))

  integer :: i

  call echo(code%bgn, 'str_convex_short__MP__str_convex_short_1d', '-p -x2')
  !-------------------------------------------------------------
  do i = 1, size(convex)
    res(i) = str_convex_short_0d(convex(i))
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_convex_short_1d
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
function str_polygon_pos_long_0d(pos) result(res)
  implicit none
  integer(1), intent(in) :: pos
  character(clen_key) :: res

  call echo(code%bgn, 'str_polygon_pos_long__MP__str_polygon_pos_long_0d', '-p -x2')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  selectcase( pos )
  case( polygon_position_undef )
    res = 'undef'
  case( polygon_position_normal )
    res = 'normal'
  case( polygon_position_lon0 )
    res = 'lon0'
  case( polygon_position_polar )
    res = 'polar'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  pos: '//str(pos))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_polygon_pos_long_0d
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
function str_zone_type_long_0d(typ) result(res)
  implicit none
  integer(1), intent(in) :: typ
  character(clen_key) :: res

  call echo(code%bgn, 'str_zone_type_long__MP__str_zone_type_long_0d', '-p -x2')
  !-------------------------------------------------------------
  selectcase( typ )
  case( zone_type_undef )
    res = 'undef'
  case( zone_type_global )
    res = 'global'
  case( zone_type_cyclic )
    res = 'cyclic'
  case( zone_type_regional )
    res = 'regional'
  case default
    call eerr(str(msg_invalid_value())//&
            '\n  typ: '//str(typ))
  endselect
  !-------------------------------------------------------------
  call echo(code%ret)
end function str_zone_type_long_0d
!===============================================================
!
!===============================================================
end module lib_util_str_param
