module mod_sphere
  use lib_const
  use lib_log
  use lib_math
  use lib_array
  implicit none
  private
  !-------------------------------------------------------------
  ! Public Procedures
  !-------------------------------------------------------------
  public :: rho_to_thresh
  public :: calc_dist_h
  public :: calc_dist_v
  public :: calc_wgt_dist

  public :: find_grids_around
  public :: get_ngrid_around
  public :: get_indices_around
  public :: free_grid

  public :: find_ilon
  public :: find_ilat
  !-------------------------------------------------------------
  ! Private Module Variables
  !-------------------------------------------------------------
  type grid_
    integer :: idx
    integer :: ngrid_around
    integer, pointer :: ilon_around(:), ilat_around(:)
    integer, pointer :: idx_around(:)
  end type

  type(grid_), pointer :: grid(:,:)
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
real(8) function rho_to_thresh(rho) result(thresh)
  implicit none
  real(8), intent(in) :: rho

  thresh = rho * (2.d0*sqrt(10.d0/3))
end function rho_to_thresh
!===============================================================
!
!===============================================================
real(8) function calc_dist_h(lon1, lat1, lon2, lat2) result(d)
  implicit none
  real(8), intent(in) :: lon1, lat1, lon2, lat2

  d = dist_sphere(lon1*d2r, lat1*d2r, lon2*d2r, lat2*d2r)
end function calc_dist_h
!===============================================================
!
!===============================================================
real(8) function calc_dist_v(logp1, logp2) result(d)
  implicit none
  real(8), intent(in) :: logp1, logp2

  d = abs(logp1 - logp2)
end function calc_dist_v
!===============================================================
!
!===============================================================
real(8) function calc_wgt_dist(dh, dv, rho_h, rho_v) result(w)
  implicit none
  real(8), intent(in) :: dh, dv
  real(8), intent(in) :: rho_h, rho_v

  w = exp(-0.5d0*((dh/rho_h)**2 + (dv/rho_v)**2))
end function calc_wgt_dist
!===============================================================
!
!===============================================================
integer function adjust_ilon(ilon_, nlon) result(ilon)
  implicit none
  integer, intent(in) :: ilon_
  integer, intent(in) :: nlon

  if( ilon_ < 1 )then
    ilon = ilon_ + nlon
  elseif( ilon_ > nlon )then
    ilon = ilon_ - nlon
  else
    ilon = ilon_
  endif
end function adjust_ilon
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
subroutine find_grids_around(&
    nlon, nlat, lonb, latb, dist_thresh)
  implicit none
  integer, intent(in) :: nlon, nlat
  real(8), intent(in) :: lonb(0:), latb(0:)
  real(8), intent(in) :: dist_thresh  ! Length on the unit sphere

  integer :: ilon, ilat
  integer :: ilon2, ilat2
  integer :: iilon
  integer :: idx
  integer :: igrid, igrid2
  real(8) :: dist
  integer :: loc
  type(grid_), pointer :: g, g2
  integer :: ilat_s, ilat_n
  integer :: ilat_latb_dist
  integer :: ilat_s_lonall, ilat_n_lonall
  integer :: iilon_w, iilon_e
  integer, allocatable :: lst_iilon_w(:), lst_iilon_e(:)
  integer, allocatable :: arg(:)

  integer :: dgt_xy
  !logical :: dbg

  call echo(code%bgn, 'find_grids_around')
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  allocate(grid(nlon,nlat))

  idx = 0
  do ilat = 1, nlat
  do ilon = 1, nlon
    idx = idx + 1
    g => grid(ilon,ilat)
    g%idx = idx
  enddo
  enddo
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Finding grids around', '-p -x2')

  allocate(lst_iilon_w(nlat))
  allocate(lst_iilon_e(nlat))
  allocate(arg(nlon*nlat))

  do ilat = 1, nlat
  do ilon = 1, nlon
    g => grid(ilon, ilat)

    lst_iilon_w(:) = 0
    lst_iilon_e(:) = 0

    !if( dbg )then
    !  print*, 'grid', ilon, ilat
    !endif
    !-----------------------------------------------------------
    ! Calc. range in latit.
    !-----------------------------------------------------------
    ilat_s = ilat - 1
    do while( ilat_s >= 1 )
      dist = calc_dist_h(lonb(ilon), latb(ilat-1), lonb(ilon), latb(ilat_s-1))
      if( dist > dist_thresh ) exit
      ilat_s = ilat_s - 1
    enddo
    !if( dbg )then
    !  print*, 'ilat_s', ilat_s
    !endif
    if( ilat_s == 0 )then
      ilat_s_lonall = 1
      do while( ilat_s_lonall <= nlat )
        dist = calc_dist_h(lonb(ilon), latb(ilat-1), lonb(ilon)+1.8d2, latb(ilat_s_lonall-1))
        if( dist > dist_thresh ) exit
        ilat_s_lonall = ilat_s_lonall + 1
      enddo
      ilat_s_lonall = ilat_s_lonall - 1
    else
      ilat_s_lonall = 0
    endif

    ilat_n = ilat + 1
    do while( ilat_n <= nlat )
      dist = calc_dist_h(lonb(ilon), latb(ilat), lonb(ilon), latb(ilat_n))
      if( dist > dist_thresh ) exit
      ilat_n = ilat_n + 1
    enddo
    !if( dbg )then
    !  print*, 'ilat_n', ilat_n
    !endif
    if( ilat_n == nlat+1 )then
      ilat_n_lonall = nlat
      do while( ilat_n_lonall >= 1 )
        dist = calc_dist_h(lonb(ilon), latb(ilat), lonb(ilon)+1.8d2, latb(ilat_n_lonall))
        if( dist > dist_thresh ) exit
        ilat_n_lonall = ilat_n_lonall - 1
      enddo
      ilat_n_lonall = ilat_n_lonall + 1
      !if( dbg )then
      !  print*, 'ilat_n_lonall', ilat_n_lonall
      !endif
    else
      ilat_n_lonall = nlat+1
    endif
    !-----------------------------------------------------------
    ! Southern grids
    !-----------------------------------------------------------
    !if( dbg ) print*, 'Southern grids'
    if( ilat_s == 0 )then
      iilon_w = ilon - 1
      iilon_e = nlon - ilon

      lst_iilon_w(1:ilat-1) = iilon_w
      lst_iilon_e(1:ilat-1) = iilon_e
    else
      do ilat2 = ilat_s, ilat-1
        if( ilat2 <= ilat_s_lonall .or. ilat2 >= ilat_n_lonall ) cycle

        iilon_w = 2
        do while( iilon_w < nlon/2 )
          ilon2 = adjust_ilon(ilon-iilon_w, nlon)

          ! Smallest distance between grid (ilon,ilat) and (ilon2,ilat2):
          ! lower left corner of the former and upper right corner of the latter
          dist = calc_dist_h(&
                   lonb(ilon-1), latb(ilat-1), lonb(ilon2), latb(ilat2))
          !if( dbg )then
          !  print*, ilon2, ilat2, dist, dist <= dist_h_thresn
          !endif
          if( dist > dist_thresh ) exit
          iilon_w = iilon_w + 1
        enddo
        iilon_w = iilon_w - 1

        iilon_e = 2
        do while( iilon_e < nlon/2 )
          ilon2 = adjust_ilon(ilon+iilon_e, nlon)

          ! Smallest distance between grid (ilon,ilat) and (ilon2,ilat2):
          ! lower right corner of the former and upper left corner of the latter
          dist = calc_dist_h(&
                   lonb(ilon), latb(ilat-1), lonb(ilon2-1), latb(ilat2))
          !if( dbg )then
          !  print*, ilon2, ilat2, dist, dist <= dist_thresh
          !endif
          if( dist > dist_thresh ) exit
          iilon_e = iilon_e + 1
        enddo
        iilon_e = iilon_e - 1

        lst_iilon_w(ilat2) = iilon_w
        lst_iilon_e(ilat2) = iilon_e
      enddo  ! ilat2/
    endif

    if( ilat_s_lonall /= 0 )then
      lst_iilon_w(1:ilat_s_lonall) = ilon - 1
      lst_iilon_e(1:ilat_s_lonall) = nlon - ilon
    endif
    !-----------------------------------------------------------
    ! Grids in the same latitude
    !-----------------------------------------------------------
    !if( dbg ) print*, 'Grids in the same latitude'
    if( ilat > ilat_s_lonall .and. ilat < ilat_n_lonall )then
      ilat2 = ilat
      ! Southern hemisphere
      if( ilat <= nlat/2 )then
        ilat_latb_dist = ilat - 1
      ! Northern hemisphere
      else
        ilat_latb_dist = ilat
      endif

      if( ilat_s < 1 .or. ilat_n > nlat )then
        iilon_w = ilon - 1
        iilon_e = nlon - ilon
      else
        iilon_w = 2
        do while( iilon_w < nlon/2 )
          ilon2 = adjust_ilon(ilon-iilon_w, nlon)

          ! Smallest distance between grid (ilon,ilat) and (ilon2,ilat2):
          ! left side of the former and right side of the latter
          dist = calc_dist_h(&
                   !lonb(ilon-1), lat(ilat), lonb(ilon2), lat(ilat2))
                   lonb(ilon-1), latb(ilat_latb_dist), &
                   lonb(ilon2) , latb(ilat_latb_dist))
          !if( dbg )then
          !  print*, ilon2, ilat2, dist, dist <= dist_thresh
          !endif
          if( dist > dist_thresh ) exit
          iilon_w = iilon_w + 1
        enddo
        iilon_w = iilon_w - 1

        iilon_e = 2
        do while( iilon_e < nlon/2 )
          ilon2 = adjust_ilon(ilon+iilon_e, nlon)

          ! Smallest distance between grid (ilon,ilat) and (ilon2,ilat2):
          ! right side of the former and left side of the latter
          dist = calc_dist_h(&
                   !lonb(ilon), lat(ilat), lonb(ilon2-1), lat(ilat2))
                   lonb(ilon)   , latb(ilat_latb_dist), &
                   lonb(ilon2-1), latb(ilat_latb_dist))
          !if( dbg )then
          !  print*, ilon2, ilat2, dist, dist <= dist_thresh
          !endif
          if( dist > dist_thresh ) exit
          iilon_e = iilon_e + 1
        enddo
        iilon_e = iilon_e - 1
      endif

      lst_iilon_w(ilat2) = iilon_w
      lst_iilon_e(ilat2) = iilon_e
    endif
    !-----------------------------------------------------------
    ! Northern grids
    !-----------------------------------------------------------
    !if( dbg ) print*, 'Northern grids'
    if( ilat_n > nlat )then
      do ilat2 = ilat+1, nlat
        iilon_w = ilon - 1
        iilon_e = nlon - ilon

        lst_iilon_w(ilat2) = iilon_w
        lst_iilon_e(ilat2) = iilon_e
      enddo
    else
      do ilat2 = ilat+1, ilat_n
        if( ilat2 <= ilat_s_lonall .or. ilat2 >= ilat_n_lonall ) cycle

        iilon_w = 2
        do while( iilon_w < nlon/2 )
          ilon2 = adjust_ilon(ilon-iilon_w, nlon)

          ! Smallest diatance between grid (ilon,ilat) and (ilon2,ilat2):
          ! upper left corner of the formar and lower right corner of the latter
          dist = calc_dist_h(&
                   lonb(ilon-1), latb(ilat), lonb(ilon2), latb(ilat2-1))
          !if( dbg )then
          !  print*, ilon2, ilat2, dist, dist <= dist_thresh
          !endif
          if( dist > dist_thresh ) exit
          iilon_w = iilon_w + 1
        enddo
        iilon_w = iilon_w - 1

        iilon_e = 2
        do while( iilon_e < nlon/2 )
          ilon2 = adjust_ilon(ilon+iilon_e, nlon)

          ! Smallest distance between grid (ilon,ilat) and (ilon2,ilat2):
          ! upper right corner of the former and lower left corner of the latter
          dist = calc_dist_h(&
                   lonb(ilon), latb(ilat), lonb(ilon2-1), latb(ilat2-1))
          !if( dbg )then
          !  print*, ilon2, ilat2, dist, dist <= dist_thresh
          !endif
          if( dist > dist_thresh ) exit
          iilon_e = iilon_e + 1
        enddo
        iilon_e = iilon_e - 1

        lst_iilon_w(ilat2) = iilon_w
        lst_iilon_e(ilat2) = iilon_e
      enddo  ! ilat2/
    endif

    if( ilat_n_lonall /= nlat+1 )then
      lst_iilon_w(ilat_n_lonall:nlat) = ilon - 1
      lst_iilon_e(ilat_n_lonall:nlat) = nlon - ilon
    endif
    !-------------------------------------------------------------
    ! Copy info.
    !-------------------------------------------------------------
    g%ngrid_around = 0
    do ilat2 = max(ilat_s,1), min(ilat_n,nlat)
      do iilon = -lst_iilon_w(ilat2), lst_iilon_e(ilat2)
        g%ngrid_around = g%ngrid_around + 1
      enddo
    enddo

    allocate(g%ilon_around(g%ngrid_around))
    allocate(g%ilat_around(g%ngrid_around))
    allocate(g%idx_around(g%ngrid_around))

    g%ngrid_around = 0
    do ilat2 = max(ilat_s,1), min(ilat_n,nlat)
      do iilon = -lst_iilon_w(ilat2), lst_iilon_e(ilat2)
        ilon2 = adjust_ilon(ilon+iilon, nlon)
        g%ngrid_around = g%ngrid_around + 1
        g%ilon_around(g%ngrid_around) = ilon2
        g%ilat_around(g%ngrid_around) = ilat2
        g%idx_around(g%ngrid_around) = grid(ilon2,ilat2)%idx
      enddo
    enddo

    call argsort(g%idx_around, arg(:g%ngrid_around))
    call sort(g%ilon_around, arg(:g%ngrid_around))
    call sort(g%ilat_around, arg(:g%ngrid_around))
    call sort(g%idx_around, arg(:g%ngrid_around))
  enddo  ! ilon = 1, nlon/
  enddo  ! ilat = 1, nlat/

  deallocate(lst_iilon_w)
  deallocate(lst_iilon_e)
  deallocate(arg)

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  call echo(code%ent, 'Checking consistency', '-p -x2')

  dgt_xy = max(dgt(nlon),dgt(nlat))

  do ilat = 1, nlat
  do ilon = 1, nlon
    g => grid(ilon,ilat)
    do igrid = 1, g%ngrid_around
      g2 => grid(g%ilon_around(igrid), g%ilat_around(igrid))
      call search(g%idx, g2%idx_around, loc)
      if( loc == 0 )then
        call eerr('grid('//str(ilon)//','//str(ilat)//')'//&
                  ' grid_around '//str(igrid)//&
                '\n  -> g2 = grid('//str((/g%ilon_around(igrid),&
                  g%ilat_around(igrid)/),dgt_xy,',')//')', &
                  '-q -b')
        call eerr('grids around g2:', '-q -p -b')
        do igrid2 = 1, g2%ngrid_around
          call eerr('  ('//str((/g2%ilon_around(igrid2),&
                    g2%ilat_around(igrid2)/),dgt_xy,',')//')', &
                    '-q -p -b')
        enddo
        call eerr('', '-p')
      endif
    enddo  ! igrid/
  enddo  ! ilon/
  enddo  ! ilat/

  call echo(code%ext)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  do ilat = 1, nlat
  do ilon = 1, nlon
    deallocate(grid(ilon,ilat)%idx_around)
  enddo
  enddo
  !-------------------------------------------------------------
  call echo(code%ret)
end subroutine find_grids_around
!===============================================================
!
!===============================================================
subroutine get_ngrid_around(ilon, ilat, ngrid_around)
  implicit none
  integer, intent(in) :: ilon, ilat
  integer, intent(out) :: ngrid_around

  ngrid_around = grid(ilon,ilat)%ngrid_around
end subroutine get_ngrid_around
!===============================================================
!
!===============================================================
subroutine get_indices_around(ilon, ilat, ilon_around, ilat_around)
  implicit none
  integer, intent(in) :: ilon, ilat
  integer, intent(out) :: ilon_around(:), ilat_around(:)

  type(grid_), pointer :: g

  g => grid(ilon,ilat)
  if( size(ilon_around) /= g%ngrid_around )then
    call eerr('size(ilon_around) /= g%ngrid_around')
  endif
  ilon_around(:) = g%ilon_around(:)
  ilat_around(:) = g%ilat_around(:)
end subroutine get_indices_around
!===============================================================
!
!===============================================================
subroutine free_grid
  implicit none

  deallocate(grid)
end subroutine free_grid
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
integer function find_ilon(lon, nlon, lonb, ilon) result(info)
  implicit none
  real(8), intent(in)  :: lon
  integer, intent(in)  :: nlon
  real(8), intent(in)  :: lonb(0:)
  integer, intent(out) :: ilon

  info = 1
  do ilon = 1, nlon
    if( abs(lonb(ilon)-lonb(ilon-1)) > 1.8d2 )then
      if( lonb(ilon-1) <= lon .or. lon < lonb(ilon) )then
        info = 0
        exit
      endif
    else
      if( lonb(ilon-1) <= lon .and. lon < lonb(ilon) )then
        info = 0
        exit
      endif
    endif
  enddo
end function find_ilon
!===============================================================
!
!===============================================================
integer function find_ilat(lat, nlat, latb, ilat) result(info)
  implicit none
  real(8), intent(in)  :: lat
  integer, intent(in)  :: nlat
  real(8), intent(in)  :: latb(0:)
  integer, intent(out) :: ilat

  if( lat == 9.d1 )then
    info = 0
    ilat = nlat
  else
    info = 1
    do ilat = 1, nlat
      if( latb(ilat-1) <= lat .and. lat < latb(ilat) )then
        info = 0
        exit
      endif
    enddo
  endif
end function find_ilat
!===============================================================
!
!===============================================================
end module mod_sphere
