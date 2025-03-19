module def_const_pb
  use lib_const
  implicit none
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  integer, parameter :: CLEN_MNNC = 8  ! length of mnemonic
  integer, parameter :: CLEN_SID = 8

  integer, parameter :: NMSGTYP = 20
  character(CLEN_MNNC), parameter :: MSGTYP_ADPUPA = 'ADPUPA'
  character(CLEN_MNNC), parameter :: MSGTYP_AIRCAR = 'AIRCAR'
  character(CLEN_MNNC), parameter :: MSGTYP_AIRCFT = 'AIRCFT'
  character(CLEN_MNNC), parameter :: MSGTYP_SATWND = 'SATWND'
  character(CLEN_MNNC), parameter :: MSGTYP_PROFLR = 'PROFLR'
  character(CLEN_MNNC), parameter :: MSGTYP_VADWND = 'VADWND'
  character(CLEN_MNNC), parameter :: MSGTYP_SATEMP = 'SATEMP'
  character(CLEN_MNNC), parameter :: MSGTYP_ADPSFC = 'ADPSFC'
  character(CLEN_MNNC), parameter :: MSGTYP_SFCSHP = 'SFCSHP'
  character(CLEN_MNNC), parameter :: MSGTYP_SFCBOG = 'SFCBOG'
  character(CLEN_MNNC), parameter :: MSGTYP_SPSSMI = 'SPSSMI'
  character(CLEN_MNNC), parameter :: MSGTYP_SYNDAT = 'SYNDAT'
  character(CLEN_MNNC), parameter :: MSGTYP_ERS1DA = 'ERS1DA'
  character(CLEN_MNNC), parameter :: MSGTYP_GOESND = 'GOESND'
  character(CLEN_MNNC), parameter :: MSGTYP_QKSWND = 'QKSWND'
  character(CLEN_MNNC), parameter :: MSGTYP_MSONET = 'MSONET'
  character(CLEN_MNNC), parameter :: MSGTYP_GPSIPW = 'GPSIPW'
  character(CLEN_MNNC), parameter :: MSGTYP_RASSDA = 'RASSDA'
  character(CLEN_MNNC), parameter :: MSGTYP_WDSATR = 'WDSATR'
  character(CLEN_MNNC), parameter :: MSGTYP_ASCATW = 'ASCATW'

  character(CLEN_MNNC), parameter :: VMNC_U_OBS    = 'UOB'
  character(CLEN_MNNC), parameter :: VMNC_V_OBS    = 'VOB'
  character(CLEN_MNNC), parameter :: VMNC_T_OBS    = 'TOB'
  character(CLEN_MNNC), parameter :: VMNC_Q_OBS    = 'QOB'
  character(CLEN_MNNC), parameter :: VMNC_Z_OBS    = 'ZOB'
  character(CLEN_MNNC), parameter :: VMNC_P_OBS    = 'POB'
  character(CLEN_MNNC), parameter :: VMNC_PS_OBS   = 'PRSS'
  character(CLEN_MNNC), parameter :: VMNC_PW_OBS   = 'PWO'
  character(CLEN_MNNC), parameter :: VMNC_RAIN_OBS = 'REQV'

  real(8), parameter :: PCD_TV = 8.d0  ! Program event code of virtual temperature

  real(8), parameter :: PREPBUFR_MISS = 1d11
  integer, parameter :: PREPBUFR_MISS_INT = -9999

  integer, parameter :: MXLV = 250

  character(CLEN_KEY), parameter :: MODE_NEW    = 'new'
  character(CLEN_KEY), parameter :: MODE_APPEND = 'append'

  ! Max length is specified in subroutine "string" in strings.F90 of NCEPLIBS
  character(80) :: hdstr = 'SID XOB YOB DHR TYP ELV SAID T29'
  character(80) :: obstr = 'POB QOB TOB ZOB UOB VOB PWO CAT'
  character(80) :: oestr = 'POE QOE TOE NUL WOE WOE PWE NUL'
  character(80) :: qmstr = 'PQM QQM TQM ZQM WQM WQM PWQ NUL'
  character(80) :: pcstr = 'PPC QPC TPC ZPC DFP DFP PWP NUL'
  character(80) :: rcstr = 'PRC QRC TRC ZRC DFR DFR PRW NUL'

  integer, parameter :: IHDR_DHR = 4
  integer, parameter :: IHDR_TYP = 5

  integer, parameter :: TYP_MASS_ALL(37) &
    = (/111, 112, 120, 122, 126, 130, 131, 132, 133, 134, 135, & 
        150, 151, 152, 153, 154, 156, 157, 158, 159, 164, 165, &
        170, 171, 174, 175, 180, 181, 182, 183, 187, 188, &
        191, 192, 193, 194, 195/)
  integer, parameter :: TYP_WIND_ALL(54) &
    = (/210, 220, 221, 222, 223, 224, 227, 228, 229, 230, &
        231, 232, 233, 234, 235, 240, 241, 242, 243, 244, &
        245, 246, 247, 248, 249, 250, 251, 252, 253, 254, &
        255, 256, 257, 258, 259, 260, &
        270, 271, 280, 281, 282, 283, 284, 285, 286, 287, &
        288, 289, 290, 291, 292, 293, 294, 295/)
  integer, parameter :: TYP_MASS_ADPSFC(5) = (/181,183,187,192,193/)
  integer, parameter :: TYP_WIND_ADPSFC(5) = (/281,284,287,292,293/)
  integer, parameter :: TYP_MASS_SFCSHP(4) = (/180,182,183,194/)
  integer, parameter :: TYP_WIND_SFCSHP(4) = (/280,282,284,294/)
  integer, parameter :: TYP_MASS_SFCBOG(1) = (/191/)
  integer, parameter :: TYP_WIND_SFCBOG(1) = (/-1/)
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: MARK_EOF = 'EOF'
  !-------------------------------------------------------------
end module def_const_pb
