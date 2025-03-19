module lib_const
  implicit none
  !=============================================================
  ! Environmental parameters
  !=============================================================
  integer, parameter :: stdin  = 5
  integer, parameter :: stdout = 6
  integer, parameter :: stderr = 0
  !=============================================================
  ! Length of string
  !=============================================================
  integer, parameter :: clen_path = 1024
  integer, parameter :: clen_key  = 64
  integer, parameter :: clen_msg  = 32
  integer, parameter :: clen_proc = 128
  integer, parameter :: clen_line = 1024
  integer, parameter :: clen_var  = 64
  integer, parameter :: clen_wfmt = 64
  integer, parameter :: clen_opt_error = 16
  !=============================================================
  ! Max. depth of procedures
  !=============================================================
  integer, parameter :: procdepth = 20
  !=============================================================
  ! Data attributes
  !=============================================================
  character(clen_path), parameter :: id_undef = 'id_undef'

  character(clen_key), parameter :: endian_big          = 'big_endian'
  character(clen_key), parameter :: endian_little       = 'little_endian'
  character(clen_key), parameter :: endian_big_short    = 'big'
  character(clen_key), parameter :: endian_little_short = 'little'
  character(clen_key), parameter :: endian_default = endian_little
  character(clen_key), parameter :: endian_undef = 'endian_undef'

  integer, parameter :: int_endian_little = 0
  integer, parameter :: int_endian_big    = 1

  character(clen_key), parameter :: dtype_char  = 'string'
  character(clen_key), parameter :: dtype_log   = 'logical'
  character(clen_key), parameter :: dtype_log1  = 'logical1'
  character(clen_key), parameter :: dtype_log2  = 'logical2'
  character(clen_key), parameter :: dtype_log4  = 'logical4'
  character(clen_key), parameter :: dtype_log8  = 'logical8'
  character(clen_key), parameter :: dtype_int1  = 'int1'
  character(clen_key), parameter :: dtype_int2  = 'int2'
  character(clen_key), parameter :: dtype_int4  = 'int4'
  character(clen_key), parameter :: dtype_int8  = 'int8'
  character(clen_key), parameter :: dtype_real  = 'real'
  character(clen_key), parameter :: dtype_dble  = 'dble'
  character(clen_key), parameter :: dtype_undef = 'undef'

  character(clen_key), parameter :: dtype_group_int     = 'int'
  character(clen_key), parameter :: dtype_group_real    = 'real'
  character(clen_key), parameter :: dtype_group_invalid = 'invalid'

  integer, parameter :: rec_undef = -9999

  character(clen_key), parameter :: status_old     = 'old'
  character(clen_key), parameter :: status_new     = 'new'
  character(clen_key), parameter :: status_replace = 'replace'
  character(clen_key), parameter :: status_unknown = 'unknown'
  character(clen_key), parameter :: status_undef   = 'undef'

  character(clen_key), parameter :: action_read      = 'read'
  character(clen_key), parameter :: action_write     = 'write'
  character(clen_key), parameter :: action_readwrite = 'readwrite'
  character(clen_key), parameter :: action_undef     = 'undef'

  character(clen_key), parameter :: position_rewind = 'rewind'
  character(clen_key), parameter :: position_append = 'append'

  integer, parameter :: permission_r     = 1
  integer, parameter :: permission_w     = 2
  integer, parameter :: permission_x     = 4
  integer, parameter :: permission_rw    = 3
  integer, parameter :: permission_rx    = 5
  integer, parameter :: permission_wx    = 6
  integer, parameter :: permission_rwx   = 7
  integer, parameter :: permission_undef = -1
  !=============================================================
  ! Special numbers
  !=============================================================
  integer(1), parameter :: int1_ulim =  huge(0_1)
  integer(1), parameter :: int1_llim = -huge(0_1) - 1_1
  integer(2), parameter :: int2_ulim =  huge(0_2)
  integer(2), parameter :: int2_llim = -huge(0_2) - 1_2
  integer(4), parameter :: int4_ulim =  huge(0_4)
  integer(4), parameter :: int4_llim = -huge(0_4) - 1_4
  integer(8), parameter :: int8_ulim =  huge(0_8)
  integer(8), parameter :: int8_llim = -huge(0_8) - 1_8

!  real(4), parameter :: nan4 = transfer(Z'7FC00000', 0.e0)
!  real(8), parameter :: nan8 = transfer(Z'7FF8000000000000', 0.d0)

!  real(4), parameter :: pinf4 = transfer(Z'7F800000', 0.e0)
!  real(4), parameter :: ninf4 = transfer(Z'FF800000', 0.e0)
!  real(8), parameter :: pinf8 = transfer(Z'7FF0000000000000', 0.d0)
!  real(8), parameter :: ninf8 = transfer(Z'FFF0000000000000', 0.d0)
  !=============================================================
  ! Physical constants
  !=============================================================
  complex(8), parameter :: ei = (0.d0, 1.d0)

  real(8), parameter :: pi = acos(-1.d0)
  real(8), parameter :: d2r = pi / 1.8d2
  real(8), parameter :: r2d = 1.8d2 / pi
  real(8), parameter :: rad_0deg   = 0.d0
  real(8), parameter :: rad_30deg  = pi/6.d0
  real(8), parameter :: rad_90deg  = pi/2.d0
  real(8), parameter :: rad_180deg = pi
  real(8), parameter :: rad_270deg = pi/2.d0*3
  real(8), parameter :: rad_360deg = pi*2.d0

  integer, parameter :: sec_hour = 60 * 60
  integer, parameter :: sec_day  = sec_hour * 24

  real(8), parameter :: dc_0k = -273.15d0  ! Zero Kelvin in degree C
  !=============================================================
  ! Earth constans
  !=============================================================
  real(8), parameter :: earth_WGS84Ellips_r_semimajor = 6378137.d0
  real(8), parameter :: earth_WGS84Ellips_r_semiminor = 6356752.3142d0
  real(8), parameter :: earth_WGS84Ellips_r_mean      = 6371008.7714d0
  real(8), parameter :: earth_WGS84Ellips_r_authalic  = 6371007.1809d0
  real(8), parameter :: earth_WGS84Ellips_r_volmetric = 6371000.7900d0

  real(8), parameter :: earth_WGS84Ellips_e2 = 0.00669437999014d0
  !=============================================================
  ! PNG
  !=============================================================
  integer, parameter :: n_255 = 255
  integer, parameter :: n_rgba = 4

  integer, parameter :: png_size_FileSignature = 8
  integer, parameter :: png_size_IHDR = 25
  integer, parameter :: png_size_IDAT_NoData = 12
  integer, parameter :: png_size_IEND = 12

!  integer(1), parameter :: png_FileSignature(8) = int((/-119, 80, 78, 71, 13, 10, 26, 10/),1)
!  integer(1), parameter :: png_ChunkType_IHDR(4) = int((/16*4+9, 16*4+8, 16*4+4, 16*5+2/),1)
!  integer(1), parameter :: png_ChunkType_IDAT(4) = int((/16*4+9, 16*4+4, 16*4+1, 16*5+4/),1)
!  integer(1), parameter :: png_ChunkType_IEND(4) = int((/16*4+9, 16*4+5, 16*4+14, 16*4+4/),1)

!  integer(1), parameter :: png_ChunkType_IEND(4) = int((/16*4+9, 16*4+5, 16*4+14, 16*4+4/),1)
!  integer(1), parameter :: png_ChunkType_IEND(4) = (/16_1*4_1+9_1, 16_1*4_1+5_1, 16_1*4_1+14_1, 16_1*4_1+4_1/)
!  integer(1), parameter :: png_ChunkType_IEND(4) = (/16*4+9, 16*4+5, 16*4+14, 16*4+4/)
  !=============================================================
  ! Keywords or specific numbers
  !=============================================================
  character(clen_key), parameter :: quote_both   = 'both'
  character(clen_key), parameter :: quote_single = 'single'
  character(clen_key), parameter :: quote_double = 'double'
  character(clen_key), parameter :: quote_none   = 'none'

  character(clen_key), parameter :: dgt_opt_sum = 'sum'
  character(clen_key), parameter :: dgt_opt_max = 'max'

  character(clen_key), parameter :: file_format_plainbinary = 'plain_binary'
  character(clen_key), parameter :: file_format_shapefile   = 'shapefile'

  character(clen_key), parameter :: interp_method_nearest  = 'nearest'
  character(clen_key), parameter :: interp_method_linear   = 'linear'
  character(clen_key), parameter :: interp_method_bilinear = 'bilinear'
  character(clen_key), parameter :: interp_method_cubic    = 'cubic'

  character(clen_key), parameter :: earth_shape_sphere = 'sphere'
  character(clen_key), parameter :: earth_shape_ellips = 'ellips'

  character(clen_key), parameter :: unit_degree    = 'degree'
  character(clen_key), parameter :: unit_radian    = 'radian'
  character(clen_key), parameter :: unit_meter     = 'm'
  character(clen_key), parameter :: unit_kilometer = 'km'
  character(clen_key), parameter :: unit_square_meter     = 'm2'
  character(clen_key), parameter :: unit_square_kilometer = 'km2'

  character(clen_key), parameter :: coord_sys_spherical = 'spherical'
  character(clen_key), parameter :: coord_sys_cartesian = 'cartesian'

  character(clen_key), parameter :: inequality_none = 'none'
  character(clen_key), parameter :: inequality_lt = 'lt'
  character(clen_key), parameter :: inequality_le = 'le'
  character(clen_key), parameter :: inequality_gt = 'gt'
  character(clen_key), parameter :: inequality_ge = 'ge'

  integer, parameter :: sign_counterclockwise =  1
  integer, parameter :: sign_clockwise        = -1

  integer(1), parameter :: zone_type_undef    = 0
  integer(1), parameter :: zone_type_global   = 1
  integer(1), parameter :: zone_type_cyclic   = 2
  integer(1), parameter :: zone_type_regional = 3

  integer(1), parameter :: arc_type_undef    = 0_1
  integer(1), parameter :: arc_type_normal   = 1_1
  integer(1), parameter :: arc_type_parallel = 2_1
  integer(1), parameter :: arc_type_meridian = 3_1

  integer(1), parameter :: arc_position_undef  = 0_1
  integer(1), parameter :: arc_position_normal = 1_1
  integer(1), parameter :: arc_position_lon0   = 2_1
  integer(1), parameter :: arc_position_polar  = 3_1

  integer(1), parameter :: stat_lon_between_undef   = 0_1
  integer(1), parameter :: stat_lon_between_inside  = 1_1
  integer(1), parameter :: stat_lon_between_outside = 2_1
  integer(1), parameter :: stat_lon_between_west    = 3_1
  integer(1), parameter :: stat_lon_between_east    = 4_1

  integer(1), parameter :: arc_rel_lat_para_para_undef = 10
  integer(1), parameter :: arc_rel_lat_para_para_below = 11
  integer(1), parameter :: arc_rel_lat_para_para_above = 12
  integer(1), parameter :: arc_rel_lat_para_norm_undef = 20
  integer(1), parameter :: arc_rel_lat_para_norm_below = 21
  integer(1), parameter :: arc_rel_lat_para_norm_above = 22
  integer(1), parameter :: arc_rel_lat_para_norm_one_intersection_upward   = 23
  integer(1), parameter :: arc_rel_lat_para_norm_one_intersection_downward = 24
  integer(1), parameter :: arc_rel_lat_para_norm_two_intersections_convex_upward = 25
  integer(1), parameter :: arc_rel_lat_para_norm_two_intersections_convex_downward = 26
  integer(1), parameter :: arc_rel_lat_norm_para_undef = 30
  integer(1), parameter :: arc_rel_lat_norm_para_below = 31
  integer(1), parameter :: arc_rel_lat_norm_para_above = 32
  integer(1), parameter :: arc_rel_lat_norm_para_one_intersection_upward   = 33
  integer(1), parameter :: arc_rel_lat_norm_para_one_intersection_downward = 34
  integer(1), parameter :: arc_rel_lat_norm_para_two_intersections_convex_upward = 35
  integer(1), parameter :: arc_rel_lat_norm_para_two_intersections_convex_downward = 36
  integer(1), parameter :: arc_rel_lat_norm_norm_undef = 40
  integer(1), parameter :: arc_rel_lat_norm_norm_below = 41
  integer(1), parameter :: arc_rel_lat_norm_norm_above = 42
  integer(1), parameter :: arc_rel_lat_norm_norm_intersection_upward   = 43
  integer(1), parameter :: arc_rel_lat_norm_norm_intersection_downward = 44

  integer(1), parameter :: arc_convex_undef    = 9
  integer(1), parameter :: arc_convex_monotone = 0
  integer(1), parameter :: arc_convex_upward   = 1
  integer(1), parameter :: arc_convex_downward = 2

  integer(1), parameter :: polygon_position_undef  = 9_1
  integer(1), parameter :: polygon_position_normal = 0_1
  integer(1), parameter :: polygon_position_lon0   = 1_1
  integer(1), parameter :: polygon_position_polar  = 2_1

  integer, parameter :: stat_inclusion_undef   = 0
  integer, parameter :: stat_inclusion_inside  = 1
  integer, parameter :: stat_inclusion_outside = 2
  integer, parameter :: stat_inclusion_on_side = 3
  integer, parameter :: stat_inclusion_invalid = 9

  integer, parameter :: stat_intersection_no  = 1
  integer, parameter :: stat_intersection_yes = 0
end module lib_const
