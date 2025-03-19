module def_const
  use lib_const
  use def_const_pb
  implicit none
  public
  !-------------------------------------------------------------
  ! Names or numbers
  !-------------------------------------------------------------
  character(CLEN_MNNC), parameter :: VAR_U    = 'U'
  character(CLEN_MNNC), parameter :: VAR_V    = 'V'
  character(CLEN_MNNC), parameter :: VAR_T    = 'T'
  character(CLEN_MNNC), parameter :: VAR_Q    = 'Q'
  character(CLEN_MNNC), parameter :: VAR_P    = 'P'
  character(CLEN_MNNC), parameter :: VAR_Z    = 'Z'
  character(CLEN_MNNC), parameter :: VAR_PW   = 'PW'
  character(CLEN_MNNC), parameter :: VAR_PS   = 'Ps'
  character(CLEN_MNNC), parameter :: VAR_RAIN = 'Rain'
  character(CLEN_MNNC), parameter :: VAR_TV   = 'Tv'  ! ext.

  integer, parameter :: REPORT_TYPE_UNDEF = -9
  integer, parameter :: REPORT_TYPE_MASS  = 1
  integer, parameter :: REPORT_TYPE_WIND  = 2

  integer, parameter :: NOBSERR = 5
  integer, parameter :: VIDX_OBSERR_U  = 1
  integer, parameter :: VIDX_OBSERR_V  = 2
  integer, parameter :: VIDX_OBSERR_T  = 3
  integer, parameter :: VIDX_OBSERR_Q  = 4
  integer, parameter :: VIDX_OBSERR_PS = 5

  integer, parameter :: UB_WGT_QLT = 5

  integer, parameter :: IDTYPE_SID  = 1
  integer, parameter :: IDTYPE_SAID = 2

  integer, parameter :: CODE_SAME_ID__NONE        = 0
  integer, parameter :: CODE_SAME_ID__SID_REDUCE  = 1
  integer, parameter :: CODE_SAME_ID__SID_LIMIT   = 2
  integer, parameter :: CODE_SAME_ID__SAID_REDUCE = 3
  integer, parameter :: CODE_SAME_ID__SAID_LIMIT  = 4

  integer, parameter :: OPT_SFC__NOT_MODIFY = 1
  integer, parameter :: OPT_SFC__MODIFY     = 2

  integer, parameter :: OPT_TV__DO_NOT_USE    =  1
  integer, parameter :: OPT_TV__CONV_TO_T_Q0  = 10
  integer, parameter :: OPT_TV__CONV_TO_T_Q1  = 11
  integer, parameter :: OPT_TV__CONV_TO_T_Q2  = 12
  integer, parameter :: OPT_TV__CONV_TO_T_Q3  = 13
  integer, parameter :: OPT_TV__CONV_TO_T_Q4  = 14
  integer, parameter :: OPT_TV__USE_AS_TV     = 20

  integer, parameter :: OPT_QERR__DO_NOT_USE = 1
  integer, parameter :: OPT_QERR__CONV_RHERR = 2
  !-------------------------------------------------------------
  ! Keywords
  !-------------------------------------------------------------
  character(CLEN_VAR), parameter :: METHOD_ALL   = 'all'
  character(CLEN_VAR), parameter :: METHOD_WGT1  = 'wgt1'
  character(CLEN_VAR), parameter :: METHOD_DNST1 = 'dnst1'
  character(CLEN_VAR), parameter :: METHOD_DNST2 = 'dnst2'
  character(CLEN_VAR), parameter :: METHOD_NONE  = 'none'
  !-------------------------------------------------------------
  ! Thresholds
  !-------------------------------------------------------------
  real(8), parameter :: WGT_THRESH   = 1.d-2
  real(8), parameter :: CNTRB_THRESH = 1.d-2
  !-------------------------------------------------------------
end module def_const
