!MODULE common_obs_speedy
MODULE common_obs_XXXXXX
!=======================================================================
!
! [PURPOSE:] Observational procedures
!
! [HISTORY:]
!   01/23/2009 Takemasa MIYOSHI  created
!   07/25/2024 Shunji   Kotsuki  updated for speedy/climax letkf
!
!=======================================================================
!$USE OMP_LIB
  USE common
  !USE common_speedy
  USE common_XXXXXX

  IMPLICIT NONE
  PUBLIC

  ! element of obs
  INTEGER,PARAMETER :: nid_obs=7
  INTEGER,PARAMETER :: id_u_obs=2819
  INTEGER,PARAMETER :: id_v_obs=2820
  INTEGER,PARAMETER :: id_t_obs=3073
  INTEGER,PARAMETER :: id_q_obs=3330
  INTEGER,PARAMETER :: id_rh_obs=3331
  INTEGER,PARAMETER :: id_ps_obs=14593
  INTEGER,PARAMETER :: id_rain_obs=9999

  ! Adapted from SO's code :: 20181219
  integer, parameter :: obs_ids(nid_obs) = (/                                       & !SO
       & id_u_obs, id_v_obs, id_t_obs, id_q_obs, id_rh_obs, id_ps_obs, id_rain_obs /) ! 1-7
  ![2024/09/11 Takeshima]
  !character(15), parameter :: obs_names(nid_obs) = (/   & !SO
  !     & "U", "V", "T", "Q", "RH", "PS", "Rain"        /) ! 1-7
  character(15), parameter :: obs_names(nid_obs) = (/   & !SO
       & "U   ", "V   ", "T   ", "Q   ", &
       & "RH  ", "PS  ", "Rain"        /) ! 1-7

  ! type of obs
  INTEGER,PARAMETER  :: ntyp_obs        = 20
  INTEGER,PARAMETER  :: ityp_ADPUPA_obs =  1  ! PREPBUFR ADPUPA
  INTEGER,PARAMETER  :: ityp_AIRCAR_obs =  2  ! PREPBUFR AIRCAR
  INTEGER,PARAMETER  :: ityp_AIRCFT_obs =  3  ! PREPBUFR AIRCFT
  INTEGER,PARAMETER  :: ityp_SATWND_obs =  4  ! PREPBUFR SATWND
  INTEGER,PARAMETER  :: ityp_PROFLR_obs =  5  ! PREPBUFR PROFLR
  INTEGER,PARAMETER  :: ityp_VADWND_obs =  6  ! PREPBUFR VADWND
  INTEGER,PARAMETER  :: ityp_SATEMP_obs =  7  ! PREPBUFR SATEMP
  INTEGER,PARAMETER  :: ityp_ADPSFC_obs =  8  ! PREPBUFR ADPSFC
  INTEGER,PARAMETER  :: ityp_SFCSHP_obs =  9  ! PREPBUFR SFCSHP
  INTEGER,PARAMETER  :: ityp_SFCBOG_obs = 10  ! PREPBUFR SFCBOG
  INTEGER,PARAMETER  :: ityp_SPSSMI_obs = 11  ! PREPBUFR SPSSMI
  INTEGER,PARAMETER  :: ityp_SYNDAT_obs = 12  ! PREPBUFR SYNDAT
  INTEGER,PARAMETER  :: ityp_ERS1DA_obs = 13  ! PREPBUFR ERS1DA
  INTEGER,PARAMETER  :: ityp_GOESND_obs = 14  ! PREPBUFR GOESND
  INTEGER,PARAMETER  :: ityp_QKSWND_obs = 15  ! PREPBUFR QKSWND
  INTEGER,PARAMETER  :: ityp_MSONET_obs = 16  ! PREPBUFR MSONET
  INTEGER,PARAMETER  :: ityp_GPSIPW_obs = 17  ! PREPBUFR GPSIPW 
  INTEGER,PARAMETER  :: ityp_RASSDA_obs = 18  ! PREPBUFR RASSDA
  INTEGER,PARAMETER  :: ityp_WDSATR_obs = 19  ! PREPBUFR WDSATR
  INTEGER,PARAMETER  :: ityp_ASCATW_obs = 20  ! PREPBUFR ASCATW

CONTAINS
!-----------------------------------------------------------------------
! Transformation from model variables to an observation
!-----------------------------------------------------------------------
SUBROUTINE Trans_XtoY(elm,ri,rj,rk,v3d,v2d,p_full,yobs)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: elm
  REAL(r_size),INTENT(IN) :: ri,rj,rk
  REAL(r_size),INTENT(IN) :: v3d(nlon,nlat,nlev,nv3d)
  REAL(r_size),INTENT(IN) :: v2d(nlon,nlat,nv2d)
  REAL(r_size),INTENT(IN) :: p_full(nlon,nlat,nlev)
  REAL(r_size),INTENT(OUT) :: yobs
  REAL(r_size) :: rh(nlon,nlat,nlev)
  INTEGER :: i,j,k
  INTEGER :: is,ie,js,je,ks,ke
  ie = CEILING( ri )
  is = ie-1
  je = CEILING( rj )
  js = je-1
  ke = CEILING( rk )
  ks = ke-1

  SELECT CASE (NINT(elm))
  CASE(id_u_obs)  ! U
    CALL itpl_3d(v3d(:,:,:,iv3d_u),ri,rj,rk,yobs)
  CASE(id_v_obs)  ! V
    CALL itpl_3d(v3d(:,:,:,iv3d_v),ri,rj,rk,yobs)
  CASE(id_t_obs)  ! T
    CALL itpl_3d(v3d(:,:,:,iv3d_t),ri,rj,rk,yobs)
  CASE(id_q_obs)  ! Q
    CALL itpl_3d(v3d(:,:,:,iv3d_q),ri,rj,rk,yobs)
  CASE(id_ps_obs) ! PS
    CALL itpl_2d(v2d(:,:,iv2d_ps),ri,rj,yobs)
  CASE(id_rain_obs) ! RAIN
    CALL itpl_2d(v2d(:,:,iv2d_rain),ri,rj,yobs)
  CASE(id_rh_obs) ! RH
    DO k=ks,ke
      DO j=js,je
        IF(ie <= nlon ) THEN
          CALL calc_rh(v3d(is,j,k,iv3d_t),v3d(is,j,k,iv3d_q),&
            & p_full(is,j,k),rh(is,j,k))
          CALL calc_rh(v3d(ie,j,k,iv3d_t),v3d(ie,j,k,iv3d_q),&
            & p_full(ie,j,k),rh(ie,j,k))
        ELSE
          CALL calc_rh(v3d(is,j,k,iv3d_t),v3d(is,j,k,iv3d_q),&
            & p_full(is,j,k),rh(is,j,k))
          CALL calc_rh(v3d( 1,j,k,iv3d_t),v3d( 1,j,k,iv3d_q),&
            & p_full( 1,j,k),rh( 1,j,k))
        END IF
      END DO
    END DO
    CALL itpl_3d(rh,ri,rj,rk,yobs)
  END SELECT

  RETURN
END SUBROUTINE Trans_XtoY
!-----------------------------------------------------------------------
! Compute relative humidity (RH)
!-----------------------------------------------------------------------
SUBROUTINE calc_rh(t,q,p,rh)
  IMPLICIT NONE
  REAL(r_size),PARAMETER :: t0=273.15d0
  REAL(r_size),PARAMETER :: e0c=6.11d0
  REAL(r_size),PARAMETER :: al=17.3d0
  REAL(r_size),PARAMETER :: bl=237.3d0
  REAL(r_size),PARAMETER :: e0i=6.1121d0
  REAL(r_size),PARAMETER :: ai=22.587d0
  REAL(r_size),PARAMETER :: bi=273.86d0
  REAL(r_size),INTENT(IN) :: t,q,p
  REAL(r_size),INTENT(OUT) :: rh
  REAL(r_size) :: e,es,tc

  e = q * p * 0.01d0 / (0.378d0 * q + 0.622d0)

  tc = t-t0
  IF(tc >= 0.0d0) THEN
    es = e0c * exp(al*tc/(bl+tc))
  ELSE IF(tc <= -15.d0) THEN
    es = e0i * exp(ai*tc/(bi+tc))
  ELSE
    es = e0c * exp(al*tc/(bl+tc)) * (15.0d0+tc)/15.0d0 &
       + e0i * exp(ai*tc/(bi+tc)) * (-tc) / 15.0d0
  END IF

  rh = e/es

  RETURN
END SUBROUTINE calc_rh
!-----------------------------------------------------------------------
! Pressure adjustment for a different height level
!-----------------------------------------------------------------------
SUBROUTINE prsadj(p,dz,t,q)
  IMPLICIT NONE
  REAL(r_size),INTENT(INOUT) :: p
  REAL(r_size),INTENT(IN) :: dz ! height difference (target - original) [m]
  REAL(r_size),INTENT(IN) :: t  ! temperature [K] at target level
  REAL(r_size),INTENT(IN) :: q  ! humidity [kg/kg] at target level
  REAL(r_size),PARAMETER :: gamma=5.0d-3 ! lapse rate [K/m]
  REAL(r_size) :: tv

  tv = t * (1.0d0 + 0.608d0 * q)
  IF(dz /= 0) THEN
!    p = p * ((-gamma*dz+tv)/tv)**(gg/(gamma*rd)) !tv is at original level
    p = p * (tv/(tv+gamma*dz))**(gg/(gamma*rd)) !tv is at target level
  END IF

  RETURN
END SUBROUTINE prsadj
!-----------------------------------------------------------------------
! Coordinate conversion
!-----------------------------------------------------------------------
SUBROUTINE phys2ijk(p_full,elem,rlon,rlat,rlev,ri,rj,rk)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: p_full(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: elem
  REAL(r_size),INTENT(IN) :: rlon
  REAL(r_size),INTENT(IN) :: rlat
  REAL(r_size),INTENT(IN) :: rlev ! pressure levels
  REAL(r_size),INTENT(OUT) :: ri
  REAL(r_size),INTENT(OUT) :: rj
  REAL(r_size),INTENT(OUT) :: rk
  REAL(r_size) :: aj,ak
  REAL(r_size) :: lnps(nlon,nlat)
  REAL(r_size) :: plev(nlev)
  INTEGER :: i,j,k
!
! rlon -> ri
!
  IF(rlon == 0.0 .OR. rlon == 360.0) THEN
    ri = REAL(nlon+1,r_size)
  ELSE
    ri = rlon / 360.0d0 * REAL(nlon,r_size) + 1.0d0
  END IF
  IF(CEILING(ri) < 2 .OR. nlon+1 < CEILING(ri)) RETURN
!
! rlat -> rj
!
  DO j=1,nlat
    IF(rlat < lat(j)) EXIT
  END DO
  IF(j == 1) THEN
    rj = (rlat + 90.0d0) / (lat(1) + 90.0d0)
  ELSE IF(j == nlat+1) THEN
    aj = (rlat - lat(nlat)) / (90.0d0 - lat(nlat))
    rj = REAL(nlat,r_size) + aj
  ELSE
    aj = (rlat - lat(j-1)) / (lat(j) - lat(j-1))
    rj = REAL(j-1,r_size) + aj
  END IF
  IF(CEILING(rj) < 2 .OR. nlat < CEILING(rj)) RETURN
!
! rlev -> rk
!
  IF(NINT(elem) == id_ps_obs) THEN ! surface pressure observation
    rk = 0.0d0
  ELSE
    !
    ! horizontal interpolation
    !
    i = CEILING(ri)
    j = CEILING(rj)
    DO k=1,nlev
      IF(i <= nlon) THEN
        lnps(i-1:i,j-1:j) = LOG(p_full(i-1:i,j-1:j,k))
      ELSE
        lnps(i-1,j-1:j) = LOG(p_full(i-1,j-1:j,k))
        lnps(1,j-1:j) = LOG(p_full(1,j-1:j,k))
      END IF
      CALL itpl_2d(lnps,ri,rj,plev(k))
    END DO
    !
    ! Log pressure
    !
    rk = LOG(rlev)
    !
    ! find rk
    !
    DO k=2,nlev-1
      IF(plev(k) < rk) EXIT ! assuming descending order of plev
    END DO
    ak = (rk - plev(k-1)) / (plev(k) - plev(k-1))
    rk = REAL(k-1,r_size) + ak
  END IF

  RETURN
END SUBROUTINE phys2ijk
!-----------------------------------------------------------------------
! Interpolation
!-----------------------------------------------------------------------
SUBROUTINE itpl_2d(var,ri,rj,var5)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: var(nlon,nlat)
  REAL(r_size),INTENT(IN) :: ri
  REAL(r_size),INTENT(IN) :: rj
  REAL(r_size),INTENT(OUT) :: var5
  REAL(r_size) :: ai,aj
  INTEGER :: i,j

  i = CEILING(ri)
  ai = ri - REAL(i-1,r_size)
  j = CEILING(rj)
  aj = rj - REAL(j-1,r_size)

  IF(i <= nlon) THEN
    var5 = var(i-1,j-1) * (1-ai) * (1-aj) &
       & + var(i  ,j-1) *    ai  * (1-aj) &
       & + var(i-1,j  ) * (1-ai) *    aj  &
       & + var(i  ,j  ) *    ai  *    aj
  ELSE
    var5 = var(i-1,j-1) * (1-ai) * (1-aj) &
       & + var(1  ,j-1) *    ai  * (1-aj) &
       & + var(i-1,j  ) * (1-ai) *    aj  &
       & + var(1  ,j  ) *    ai  *    aj
  END IF

  RETURN
END SUBROUTINE itpl_2d

SUBROUTINE itpl_3d(var,ri,rj,rk,var5)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: var(nlon,nlat,nlev)
  REAL(r_size),INTENT(IN) :: ri
  REAL(r_size),INTENT(IN) :: rj
  REAL(r_size),INTENT(IN) :: rk
  REAL(r_size),INTENT(OUT) :: var5
  REAL(r_size) :: ai,aj,ak
  INTEGER :: i,j,k

  i = CEILING(ri)
  ai = ri - REAL(i-1,r_size)
  j = CEILING(rj)
  aj = rj - REAL(j-1,r_size)
  k = CEILING(rk)
  ak = rk - REAL(k-1,r_size)

  IF(i <= nlon) THEN
    var5 = var(i-1,j-1,k-1) * (1-ai) * (1-aj) * (1-ak) &
       & + var(i  ,j-1,k-1) *    ai  * (1-aj) * (1-ak) &
       & + var(i-1,j  ,k-1) * (1-ai) *    aj  * (1-ak) &
       & + var(i  ,j  ,k-1) *    ai  *    aj  * (1-ak) &
       & + var(i-1,j-1,k  ) * (1-ai) * (1-aj) *    ak  &
       & + var(i  ,j-1,k  ) *    ai  * (1-aj) *    ak  &
       & + var(i-1,j  ,k  ) * (1-ai) *    aj  *    ak  &
       & + var(i  ,j  ,k  ) *    ai  *    aj  *    ak
  ELSE
    var5 = var(i-1,j-1,k-1) * (1-ai) * (1-aj) * (1-ak) &
       & + var(1  ,j-1,k-1) *    ai  * (1-aj) * (1-ak) &
       & + var(i-1,j  ,k-1) * (1-ai) *    aj  * (1-ak) &
       & + var(1  ,j  ,k-1) *    ai  *    aj  * (1-ak) &
       & + var(i-1,j-1,k  ) * (1-ai) * (1-aj) *    ak  &
       & + var(1  ,j-1,k  ) *    ai  * (1-aj) *    ak  &
       & + var(i-1,j  ,k  ) * (1-ai) *    aj  *    ak  &
       & + var(1  ,j  ,k  ) *    ai  *    aj  *    ak
  END IF

  RETURN
END SUBROUTINE itpl_3d
!-----------------------------------------------------------------------
! Monitor departure
!-----------------------------------------------------------------------
SUBROUTINE monit_dep(nn,elm,dep,qc)
  IMPLICIT NONE
  INTEGER,     INTENT(IN) :: nn
  REAL(r_size),INTENT(IN) :: elm(nn)
  REAL(r_size),INTENT(IN) :: dep(nn)
  INTEGER,     INTENT(IN) :: qc(nn)
  REAL(r_size) :: rmse_u,rmse_v,rmse_t,rmse_q,rmse_ps,rmse_rh
  REAL(r_size) :: bias_u,bias_v,bias_t,bias_q,bias_ps,bias_rh
  INTEGER      :: n,iu,iv,it,iq,ips,irh

  rmse_u  = 0.0d0  ;  bias_u  = 0.0d0  ;  iu  = 0
  rmse_v  = 0.0d0  ;  bias_v  = 0.0d0  ;  iv  = 0
  rmse_t  = 0.0d0  ;  bias_t  = 0.0d0  ;  it  = 0
  rmse_q  = 0.0d0  ;  bias_q  = 0.0d0  ;  iq  = 0
  rmse_ps = 0.0d0  ;  bias_ps = 0.0d0  ;  ips = 0
  rmse_rh = 0.0d0  ;  bias_rh = 0.0d0  ;  irh = 0
  
  DO n=1,nn
    IF(qc(n) /= 1) CYCLE
    SELECT CASE(NINT(elm(n)))
    CASE(id_u_obs)
      rmse_u = rmse_u + dep(n)**2
      bias_u = bias_u + dep(n)
      iu = iu + 1
    CASE(id_v_obs)
      rmse_v = rmse_v + dep(n)**2
      bias_v = bias_v + dep(n)
      iv = iv + 1
    CASE(id_t_obs)
      rmse_t = rmse_t + dep(n)**2
      bias_t = bias_t + dep(n)
      it = it + 1
    CASE(id_q_obs)
      rmse_q = rmse_q + dep(n)**2
      bias_q = bias_q + dep(n)
      iq = iq + 1
    CASE(id_ps_obs)
      rmse_ps = rmse_ps + dep(n)**2
      bias_ps = bias_ps + dep(n)
      ips = ips + 1
    CASE(id_rh_obs)
      rmse_rh = rmse_rh + dep(n)**2
      bias_rh = bias_rh + dep(n)
      irh = irh + 1
    END SELECT
  END DO
  IF(iu == 0) THEN
    rmse_u = undef
    bias_u = undef
  ELSE
    rmse_u = SQRT(rmse_u / REAL(iu,r_size))
    bias_u = bias_u / REAL(iu,r_size)
  END IF
  IF(iv == 0) THEN
    rmse_v = undef
    bias_v = undef
  ELSE
    rmse_v = SQRT(rmse_v / REAL(iv,r_size))
    bias_v = bias_v / REAL(iv,r_size)
  END IF
  IF(it == 0) THEN
    rmse_t = undef
    bias_t = undef
  ELSE
    rmse_t = SQRT(rmse_t / REAL(it,r_size))
    bias_t = bias_t / REAL(it,r_size)
  END IF
  IF(iq == 0) THEN
    rmse_q = undef
    bias_q = undef
  ELSE
    rmse_q = SQRT(rmse_q / REAL(iq,r_size))
    bias_q = bias_q / REAL(iq,r_size)
  END IF
  IF(ips == 0) THEN
    rmse_ps = undef
    bias_ps = undef
  ELSE
    rmse_ps = SQRT(rmse_ps / REAL(ips,r_size))
    bias_ps = bias_ps / REAL(ips,r_size)
  END IF
  IF(irh == 0) THEN
    rmse_rh = undef
    bias_rh = undef
  ELSE
    rmse_rh = SQRT(rmse_rh / REAL(irh,r_size))
    bias_rh = bias_rh / REAL(irh,r_size)
  END IF

  WRITE(6,'(A)') '== OBSERVATIONAL DEPARTURE ====================================================='
  WRITE(6,'(6A12)') 'U','V','T','Q','PS','RH'
  WRITE(6,'(6ES12.3)') bias_u,bias_v,bias_t,bias_q,bias_ps,bias_rh
  WRITE(6,'(6ES12.3)') rmse_u,rmse_v,rmse_t,rmse_q,rmse_ps,rmse_rh
  WRITE(6,'(A)') '== NUMBER OF OBSERVATIONS TO BE ASSIMILATED (vars) ============================='
  WRITE(6,'(6A12)') 'U','V','T','Q','PS','RH'
  WRITE(6,'(6I12)') iu,iv,it,iq,ips,irh

  RETURN
END SUBROUTINE monit_dep
!-----------------------------------------------------------------------
! Monitor Observation Type
!-----------------------------------------------------------------------
SUBROUTINE monit_typ(nn,typ,qc)
  IMPLICIT NONE
  INTEGER,     INTENT(IN) :: nn
  REAL(r_size),INTENT(IN) :: typ(nn)
  INTEGER,     INTENT(IN) :: qc(nn)
  INTEGER :: n
  INTEGER :: iADPUPA,iAIRCAR,iAIRCFT,iSATWND,iPROFLR
  INTEGER :: iVADWND,iSATEMP,iADPSFC,iSFCSHP,iSFCBOG
  INTEGER :: iSPSSMI,iSYNDAT,iERS1DA,iGOESND,iQKSWND
  INTEGER :: iMSONET,iGPSIPW,iRASSDA,iWDSATR,iASCATW

  iADPUPA=0 ; iAIRCAR=0 ; iAIRCFT=0 ; iSATWND=0 ; iPROFLR=0
  iVADWND=0 ; iSATEMP=0 ; iADPSFC=0 ; iSFCSHP=0 ; iSFCBOG=0
  iSPSSMI=0 ; iSYNDAT=0 ; iERS1DA=0 ; iGOESND=0 ; iQKSWND=0
  iMSONET=0 ; iGPSIPW=0 ; iRASSDA=0 ; iWDSATR=0 ; iASCATW=0

  DO n=1,nn
    IF(qc(n) /= 1) CYCLE
    SELECT CASE(NINT(typ(n)))
      CASE(ityp_ADPUPA_obs)  ;  iADPUPA = iADPUPA + 1
      CASE(ityp_AIRCAR_obs)  ;  iAIRCAR = iAIRCAR + 1
      CASE(ityp_AIRCFT_obs)  ;  iAIRCFT = iAIRCFT + 1
      CASE(ityp_SATWND_obs)  ;  iSATWND = iSATWND + 1
      CASE(ityp_PROFLR_obs)  ;  iPROFLR = iPROFLR + 1
      CASE(ityp_VADWND_obs)  ;  iVADWND = iVADWND + 1
      CASE(ityp_SATEMP_obs)  ;  iSATEMP = iSATEMP + 1
      CASE(ityp_ADPSFC_obs)  ;  iADPSFC = iADPSFC + 1
      CASE(ityp_SFCSHP_obs)  ;  iSFCSHP = iSFCSHP + 1
      CASE(ityp_SFCBOG_obs)  ;  iSFCBOG = iSFCBOG + 1
      CASE(ityp_SPSSMI_obs)  ;  iSPSSMI = iSPSSMI + 1
      CASE(ityp_SYNDAT_obs)  ;  iSYNDAT = iSYNDAT + 1
      CASE(ityp_ERS1DA_obs)  ;  iERS1DA = iERS1DA + 1
      CASE(ityp_GOESND_obs)  ;  iGOESND = iGOESND + 1
      CASE(ityp_QKSWND_obs)  ;  iQKSWND = iQKSWND + 1
      CASE(ityp_MSONET_obs)  ;  iMSONET = iMSONET + 1
      CASE(ityp_GPSIPW_obs)  ;  iGPSIPW = iGPSIPW + 1 
      CASE(ityp_RASSDA_obs)  ;  iRASSDA = iRASSDA + 1
      CASE(ityp_WDSATR_obs)  ;  iWDSATR = iWDSATR + 1
      CASE(ityp_ASCATW_obs)  ;  iASCATW = iASCATW + 1
    END SELECT
  END DO

  WRITE(6,'(A)') '== NUMBER OF OBSERVATIONS TO BE ASSIMILATED (type) ============================='
  WRITE(6,'(10A8)') 'ADPUPA','AIRCAR','AIRCFT','SATWND','PROFLR','VADWND','SATEMP','ADPSFC','SFCSHP','SFCBOG'
  WRITE(6,'(10I8)') iADPUPA ,iAIRCAR ,iAIRCFT ,iSATWND ,iPROFLR ,iVADWND ,iSATEMP ,iADPSFC ,iSFCSHP ,iSFCBOG
  WRITE(6,'(A)') '- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  WRITE(6,'(10A8)') 'SPSSMI','SYNDAT','ERS1DA','GOESND','QKSWND','MSONET','GPSIPW','RASSDA','WDSATR','ASCATW'
  WRITE(6,'(10I8)') iSPSSMI ,iSYNDAT ,iERS1DA ,iGOESND ,iQKSWND ,iMSONET ,iGPSIPW ,iRASSDA ,iWDSATR ,iASCATW
  WRITE(6,'(A)') '================================================================================'

  RETURN
END SUBROUTINE monit_typ
!-----------------------------------------------------------------------
! Basic modules for observation input
!-----------------------------------------------------------------------
SUBROUTINE get_nobs(cfile,nrec,nn)
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: cfile
  INTEGER,INTENT(IN) :: nrec
  INTEGER,INTENT(OUT) :: nn
  REAL(r_sngl),ALLOCATABLE :: wk(:)
  INTEGER :: ios
  INTEGER :: iu,iv,it,iq,irh,ips
  INTEGER :: iunit
  LOGICAL :: ex
  integer :: num_obs_each_id(nid_obs) !SO
  integer :: iobs                     !SO

  ALLOCATE(wk(nrec))
  nn = 0
  num_obs_each_id(:) = 0 !SO

  iunit=91
  INQUIRE(FILE=cfile,EXIST=ex)
  IF(ex) THEN
    OPEN(iunit,FILE=cfile,FORM='unformatted',ACCESS='sequential')
    DO
      READ(iunit,IOSTAT=ios) wk
      IF(ios /= 0) EXIT
      do iobs = 1, nid_obs !SO
         if(obs_ids(iobs) == wk(1)) exit !SO
      end do !SO
      if(iobs <= nid_obs) then !SO
         num_obs_each_id(iobs) = num_obs_each_id(iobs) + 1 !SO
      else !SO
         write(6, *) "unknown observation ID: ", wk(1) !SO
      end if !SO
      nn = nn + 1
    END DO
    WRITE(6,'(I10,A)') nn,' OBSERVATIONS INPUT'
    do iobs = 1, nid_obs !SO
       WRITE(6,'(A15, ": ", I10)') obs_names(iobs), num_obs_each_id(iobs) !SO
    end do !SO
    CLOSE(iunit)
  ELSE
    WRITE(6,'(2A)') cfile,' does not exist -- skipped'
  END IF
  DEALLOCATE(wk)

!!  ALLOCATE(wk(nrec))
!!  nn = 0
!!  iu = 0
!!  iv = 0
!!  it = 0
!!  iq = 0
!!  irh = 0
!!  ips = 0
!!  iunit=91
!!  INQUIRE(FILE=cfile,EXIST=ex)
!!  IF(ex) THEN
!!    OPEN(iunit,FILE=cfile,FORM='unformatted',ACCESS='sequential')
!!    DO
!!      READ(iunit,IOSTAT=ios) wk
!!      IF(ios /= 0) EXIT
!!      SELECT CASE(NINT(wk(1)))
!!      CASE(id_u_obs)
!!        iu = iu + 1
!!      CASE(id_v_obs)
!!        iv = iv + 1
!!      CASE(id_t_obs)
!!        it = it + 1
!!      CASE(id_q_obs)
!!        iq = iq + 1
!!      CASE(id_rh_obs)
!!        irh = irh + 1
!!      CASE(id_ps_obs)
!!        ips = ips + 1
!!      END SELECT
!!      nn = nn + 1
!!    END DO
!!    WRITE(6,'(I10,A)') nn,' OBSERVATIONS INPUT'
!!    WRITE(6,'(A12,I10)') '          U:',iu
!!    WRITE(6,'(A12,I10)') '          V:',iv
!!    WRITE(6,'(A12,I10)') '          T:',it
!!    WRITE(6,'(A12,I10)') '          Q:',iq
!!    WRITE(6,'(A12,I10)') '         RH:',irh
!!    WRITE(6,'(A12,I10)') '         Ps:',ips
!!    CLOSE(iunit)
!!  ELSE
!!    WRITE(6,'(2A)') cfile,' does not exist -- skipped'
!!  END IF
!!  DEALLOCATE(wk)

  RETURN
END SUBROUTINE get_nobs

SUBROUTINE read_obs(cfile,nn,elem,rlon,rlat,rlev,odat,oerr,otyp)
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: cfile
  INTEGER,INTENT(IN) :: nn
  REAL(r_size),INTENT(OUT) :: elem(nn) ! (1) element number
  REAL(r_size),INTENT(OUT) :: rlon(nn) ! (2) longitude
  REAL(r_size),INTENT(OUT) :: rlat(nn) ! (3) latitude
  REAL(r_size),INTENT(OUT) :: rlev(nn) ! (4) pressure 
  REAL(r_size),INTENT(OUT) :: odat(nn) ! (5) obs data
  REAL(r_size),INTENT(OUT) :: oerr(nn) ! (6) obs error
  REAL(r_size),INTENT(OUT) :: otyp(nn) ! (7) obs type
  REAL(r_sngl) :: wk(7)
  INTEGER :: n,iunit

  iunit=91
  OPEN(iunit,FILE=cfile,FORM='unformatted',ACCESS='sequential')
  DO n=1,nn
    READ(iunit) wk
    SELECT CASE(NINT(wk(1)))
    CASE(id_u_obs)
      wk(4) = wk(4) * 100.0 ! hPa -> Pa
    CASE(id_v_obs)
      wk(4) = wk(4) * 100.0 ! hPa -> Pa
    CASE(id_t_obs)
      wk(4) = wk(4) * 100.0 ! hPa -> Pa
    CASE(id_q_obs)
      wk(4) = wk(4) * 100.0 ! hPa -> Pa
    CASE(id_ps_obs)
      wk(5) = wk(5) * 100.0 ! hPa -> Pa
      wk(6) = wk(6) * 100.0 ! hPa -> Pa
    CASE(id_rh_obs)
      wk(4) = wk(4) * 100.0 ! hPa -> Pa
      wk(5) = wk(5) * 0.01  ! percent input
      wk(6) = wk(6) * 0.01  ! percent input
    END SELECT
    elem(n) = REAL(wk(1),r_size)
    rlon(n) = REAL(wk(2),r_size)
    rlat(n) = REAL(wk(3),r_size)
    rlev(n) = REAL(wk(4),r_size)
    odat(n) = REAL(wk(5),r_size)
    oerr(n) = REAL(wk(6),r_size)
    otyp(n) = REAL(wk(7),r_size)
  END DO
  CLOSE(iunit)

  RETURN
END SUBROUTINE read_obs

SUBROUTINE read_obs2(cfile, nn, elem, rlon, rlat, rlev, odat, oerr, ohx, oqc, otyp)
  IMPLICIT NONE
  CHARACTER(*), INTENT(IN) :: cfile
  INTEGER, INTENT(IN) :: nn
  REAL(r_size), INTENT(OUT) :: elem(nn) ! element number
  REAL(r_size), INTENT(OUT) :: rlon(nn)
  REAL(r_size), INTENT(OUT) :: rlat(nn)
  REAL(r_size), INTENT(OUT) :: rlev(nn)
  REAL(r_size), INTENT(OUT) :: odat(nn)
  REAL(r_size), INTENT(OUT) :: oerr(nn)
  REAL(r_size), INTENT(OUT) :: ohx(nn)
  REAL(r_size), INTENT(OUT) :: otyp(nn)
  INTEGER, INTENT(OUT) :: oqc(nn)
  integer, parameter :: nread = 10000
  REAL(r_sngl) :: wk(9, 0:nread)
  INTEGER :: n,iunit
  integer m, mmax, n0, n1

  mmax = nn / nread
  if(mod(nn, nread) > 0) mmax = mmax + 1

  iunit=91
  OPEN(iunit,FILE=cfile,FORM='unformatted',ACCESS='sequential')
  do m = 1, mmax
     n0 = (m - 1) * nread + 1
     n1 = m * nread
     if(n1 > nn) n1 = nn
     do n = n0, n1
        read(iunit) wk(:, n - n0)
     end do
     do n = n0, n1
        SELECT CASE(NINT(wk(1, n - n0)))
        CASE(id_u_obs)
           wk(4, n - n0) = wk(4, n - n0) * 100.0d0 ! hPa -> Pa
        CASE(id_v_obs)
           wk(4, n - n0) = wk(4, n - n0) * 100.0d0 ! hPa -> Pa
        CASE(id_t_obs)
           wk(4, n - n0) = wk(4, n - n0) * 100.0d0 ! hPa -> Pa
        CASE(id_q_obs)
           wk(4, n - n0) = wk(4, n - n0) * 100.0d0 ! hPa -> Pa
        CASE(id_ps_obs)
           wk(5, n - n0) = wk(5, n - n0) * 100.0d0 ! hPa -> Pa
           wk(6, n - n0) = wk(6, n - n0) * 100.0d0 ! hPa -> Pa
        CASE(id_rh_obs)
           wk(4, n - n0) = wk(4, n - n0) * 100.0d0 ! hPa -> Pa
           wk(5, n - n0) = wk(5, n - n0) * 0.01d0  ! percent input
           wk(6, n - n0) = wk(6, n - n0) * 0.01d0  ! percent input
        CASE(  id_rain_obs ) !!! taeka to here
           wk(4, n - n0) = wk(4, n - n0) * 100.0d0
        END SELECT

        elem(n) = REAL(wk(1, n - n0), r_size)
        rlon(n) = REAL(wk(2, n - n0), r_size)
        rlat(n) = REAL(wk(3, n - n0), r_size)
        rlev(n) = REAL(wk(4, n - n0), r_size)
        odat(n) = REAL(wk(5, n - n0), r_size)
        oerr(n) = REAL(wk(6, n - n0), r_size)
        ohx(n)  = REAL(wk(7, n - n0), r_size)
        oqc(n)  = NINT(wk(8, n - n0))
        otyp(n) = REAL(wk(9, n - n0), r_size)
     end do
  END DO
  CLOSE(iunit)

  RETURN
END SUBROUTINE read_obs2

SUBROUTINE write_obs2(cfile,nn,elem,rlon,rlat,rlev,odat,oerr,ohx,oqc,otyp)
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: cfile
  INTEGER,     INTENT(IN) :: nn
  REAL(r_size),INTENT(IN) :: elem(nn) ! element number
  REAL(r_size),INTENT(IN) :: rlon(nn)
  REAL(r_size),INTENT(IN) :: rlat(nn)
  REAL(r_size),INTENT(IN) :: rlev(nn)
  REAL(r_size),INTENT(IN) :: odat(nn)
  REAL(r_size),INTENT(IN) :: oerr(nn)
  REAL(r_size),INTENT(IN) :: ohx(nn)
  INTEGER,     INTENT(IN) :: oqc(nn)
  REAL(r_size),INTENT(IN) :: otyp(nn) 
  REAL(r_sngl) :: wk(9)
  INTEGER :: n,iunit

  iunit=92
  OPEN(iunit,FILE=cfile,FORM='unformatted',ACCESS='sequential')
  DO n=1,nn
    wk(1) = REAL(elem(n),r_sngl)
    wk(2) = REAL(rlon(n),r_sngl)
    wk(3) = REAL(rlat(n),r_sngl)
    wk(4) = REAL(rlev(n),r_sngl)
    wk(5) = REAL(odat(n),r_sngl)
    wk(6) = REAL(oerr(n),r_sngl)
    wk(7) = REAL(ohx(n), r_sngl)
    wk(8) = REAL(oqc(n), r_sngl)
    wk(9) = REAL(otyp(n),r_sngl)
    SELECT CASE(NINT(wk(1)))
    CASE(id_u_obs)
      wk(4) = wk(4) * 0.01
    CASE(id_v_obs)
      wk(4) = wk(4) * 0.01
    CASE(id_t_obs)
      wk(4) = wk(4) * 0.01
    CASE(id_q_obs)
      wk(4) = wk(4) * 0.01
    CASE(id_ps_obs)
      wk(5) = wk(5) * 0.01
      wk(6) = wk(6) * 0.01
    CASE(id_rh_obs)
      wk(4) = wk(4) * 0.01
      wk(5) = wk(5) * 100.0
      wk(6) = wk(6) * 100.0
    END SELECT
    WRITE(iunit) wk
  END DO
  CLOSE(iunit)

  RETURN
END SUBROUTINE write_obs2

!END MODULE common_obs_speedy
END MODULE common_obs_XXXXXX
