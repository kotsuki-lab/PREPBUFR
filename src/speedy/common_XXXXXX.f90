!MODULE common_climax
MODULE common_XXXXXX
!=======================================================================
!
! [PURPOSE:] Common Information for CLIMAX
!
! [HISTORY:]
!   10/15/2004 Takemasa Miyoshi  created
!   01/23/2009 Takemasa Miyoshi  modified
!   06/29/2024 Shunji   Kotsuki  updated for climax
!
!=======================================================================
!$USE OMP_LIB
  USE common
  IMPLICIT NONE
  PUBLIC
!-----------------------------------------------------------------------
! General parameters
!-----------------------------------------------------------------------
  INTEGER,PARAMETER :: nlon=64
  INTEGER,PARAMETER :: nlat=nlon/2
  INTEGER,PARAMETER :: nlev=7
  INTEGER,PARAMETER :: nv3d=5 ! u,v,t,q,geo
  INTEGER,PARAMETER :: nv2d=4 ! t2m,u10m,v10m,ps
  INTEGER,PARAMETER :: iv3d_u=1
  INTEGER,PARAMETER :: iv3d_v=2
  INTEGER,PARAMETER :: iv3d_t=3
  INTEGER,PARAMETER :: iv3d_q=4
  INTEGER,PARAMETER :: iv3d_geo=5
  INTEGER,PARAMETER :: iv2d_t2m=1
  INTEGER,PARAMETER :: iv2d_u10m=2
  INTEGER,PARAMETER :: iv2d_v10m=3
  INTEGER,PARAMETER :: iv2d_ps=4
  INTEGER,PARAMETER :: iv2d_rain=5    ! not assined for nv2d
  INTEGER,PARAMETER :: nij0=nlon*nlat
  INTEGER,PARAMETER :: nlevall=nlev*nv3d+nv2d
  INTEGER,PARAMETER :: ngpv=nij0*nlevall
  REAL(r_size),SAVE :: lon(nlon)
  REAL(r_size),SAVE :: lat(nlat)
  REAL(r_size),SAVE :: sig(nlev)
  REAL(r_size),SAVE :: plv(nlev) ! for climax
  REAL(r_size),SAVE :: dx(nlat)
  REAL(r_size),SAVE :: dy(nlat)
  REAL(r_size),SAVE :: dy2(nlat)
  REAL(r_size),SAVE :: fcori(nlat)
  REAL(r_size),SAVE :: phi0(nlon,nlat)
  REAL(4),     SAVE :: phir(nlon,nlat)
  CHARACTER(4),SAVE :: element(nv3d+nv2d)

  !===> letkf.cnf
  REAL(r_size),SAVE :: sigma_obs  = 9999.0d3
  INTEGER,     SAVE :: pymdh      = 0                             ! KK
  INTEGER,     SAVE :: ymdh       = 0                             ! KK  
  LOGICAL,     SAVE :: logic_wout = .false.
  LOGICAL,     SAVE :: logic_wsth = .false.
  LOGICAL,     SAVE :: logic_wint = .false.
  INTEGER,     SAVE :: dastype    = 0
  INTEGER,     SAVE :: resample_m = 0
  INTEGER,     SAVE :: type_wnorm = 0
  INTEGER,     SAVE :: type_pfmtx = 0
  INTEGER,     SAVE :: type_relax = 0
  REAL(r_size),SAVE :: alph_relax = 0.0d0
  REAL(r_size),SAVE :: fgt_factor = 1.0d0
  REAL(r_size),SAVE :: gamma_gmpf = 1.0d0
CONTAINS
!-----------------------------------------------------------------------
! Set the parameters
!-----------------------------------------------------------------------
!SUBROUTINE set_common_climax
SUBROUTINE set_common_XXXXXX
  IMPLICIT NONE
  INTEGER :: i,j

  WRITE(6,'(A)') 'Hello from set_common_XXXXXX(climax)'
  !
  ! Elements
  !
  element(iv3d_u)         = 'U   '
  element(iv3d_v)         = 'V   '
  element(iv3d_t)         = 'T   '
  element(iv3d_q)         = 'Q   '
  element(iv3d_geo)       = 'Geo '
  element(nv3d+iv2d_t2m)  = 'T2m '
  element(nv3d+iv2d_u10m) = 'U10m'
  element(nv3d+iv2d_v10m) = 'V10m'
  element(nv3d+iv2d_ps)   = 'PS  '
  !
  ! Lon, Lat, Sigma
  !
!$OMP PARALLEL DO PRIVATE(i)
  DO i=1,nlon
    lon(i) = 360.d0/nlon*(i-1)
  END DO
!$OMP END PARALLEL DO

!$OMP PARALLEL DO PRIVATE(i)
  DO i=1,nlat
    lat(i) = -87.1875 + 5.625*real(i-1)
  END DO
!$OMP END PARALLEL DO

  sig(1) = undef !TMP! .95d0
  sig(2) = undef !TMP! .835d0
  sig(3) = undef !TMP! .685d0
  sig(4) = undef !TMP! .51d0
  sig(5) = undef !TMP! .34d0
  sig(6) = undef !TMP! .2d0
  sig(7) = undef !TMP! .08d0
  plv(1) = 92500.0d0 ! specified for climax      
  plv(2) = 85000.0d0 ! specified for climax
  plv(3) = 70000.0d0 ! specified for climax
  plv(4) = 60000.0d0 ! specified for climax
  plv(5) = 50000.0d0 ! specified for climax
  plv(6) = 25000.0d0 ! specified for climax
  plv(7) =  5000.0d0 ! specified for climax
  !
  ! dx and dy
  !
!$OMP PARALLEL
!$OMP WORKSHARE
  dx(:) = 2.0d0 * pi * re * cos(lat(:) * pi / 180.0d0) / REAL(nlon,r_size)
!$OMP END WORKSHARE

!$OMP DO
  DO i=1,nlat-1
    dy(i) = 2.0d0 * pi * re * (lat(i+1) - lat(i)) / 360.0d0
  END DO
!$OMP END DO
!$OMP END PARALLEL
  dy(nlat) = 2.0d0 * pi * re * (90.0d0 - lat(nlat)) / 180.0d0

!$OMP PARALLEL DO
  DO i=2,nlat
    dy2(i) = (dy(i-1) + dy(i)) * 0.5d0
  END DO
!$OMP END PARALLEL DO
  dy2(1) = (dy(nlat) + dy(1)) * 0.5d0
  !
  ! Corioris parameter
  !
!$OMP PARALLEL WORKSHARE
  fcori(:) = 2.0d0 * r_omega * sin(lat(:)*pi/180.0d0)
!$OMP END PARALLEL WORKSHARE
  !
  ! Surface geoptential (Read Orography file)
  !
  !JSS2,BUG???,20190417SK!READ(21) phi0
  !!open(31,file="./orography_t30.dat",form="unformatted",access="sequential",status="old")
  !!  read(31) phi0(:,:)
  !!close(31)
  !!open(31, file="./orography_t30.dat", form="unformatted")  ! KK
  !!  READ(31) phi0
  !!close(31)
  open(31,file="./orography_x64y32.grd",form="unformatted",access="direct",status="old",recl=nlon*nlat*4)
    read(31,rec=1) phir(:,:) ; phi0(:,:)=dble(phir(:,:))
  close(31)

  RETURN
!END SUBROUTINE set_common_climax
END SUBROUTINE set_common_XXXXXX
!-----------------------------------------------------------------------
! p_full
!-----------------------------------------------------------------------
SUBROUTINE calc_pfull(ix,jy,ps,p_full)
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: ix,jy
  REAL(r_size),INTENT(IN) :: ps(ix,jy)
  REAL(r_size),INTENT(OUT) :: p_full(ix,jy,nlev)
  INTEGER :: i,j,k

!$OMP PARALLEL DO PRIVATE(i,j,k)
  DO k=1,nlev
    DO j=1,jy
      DO i=1,ix
!        p_full(i,j,k) = ps(i,j) * sig(k)
        p_full(i,j,k) = plv(k)
      END DO
    END DO
  END DO
!$OMP END PARALLEL DO

  RETURN
END SUBROUTINE calc_pfull
!END MODULE common_climax
END MODULE common_XXXXXX