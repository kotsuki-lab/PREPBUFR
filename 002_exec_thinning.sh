#!/bin/sh
#=======================================================================
# environment::
#   singularity run --bind /data02,/data10 ../share/oneapi-hpckit_latest.sif
#     - bind directories (e.g., data02 data10) should be modified by users
#=======================================================================
#set -ex
. ./000_func_calendar.sh

RUNNAME="dnst1_sfc1_Tv12_n0"

if [ $# -eq 2 ]; then
  IY=`echo $(( 10#$1 ))`
  IM=`echo $(( 10#$2 ))`
  ID=1
  EY=$IY
  EM=$IM
  ED=`get_dymax $IY $IM`

  IY=`printf "%04d" $IY`
  IM=`printf "%02d" $IM`
  ID=`printf "%02d" $ID`
  IH=00
  EY=`printf "%04d" $EY`
  EM=`printf "%02d" $EM`
  ED=`printf "%02d" $ED`
  EH=18
elif [ $# -eq 1 ]; then
  # Initial date
  IY=`echo $1 | cut -c 1-4`
  IM=`echo $1 | cut -c 5-6`
  ID=`echo $1 | cut -c 7-8`
  IH=`echo $1 | cut -c 9-10`
  # Final date
  EY=$IY
  EM=$IM
  ED=$ID
  EH=$IH
else
  echo "Invalid number of argument"
  exit
fi

# Limitation of time range to output
TOUT_BHD=0
TOUT_AHD=0

# Thinning parameter
NMAX=0
RHO_H=500
RHO_V=0.1
DNST_THRESH="1.d0"
WGT_QLT="1.0, 0.8, 0.4, 0.1, 0.0"
WGT_SAME_SID="1.0"
NMAX_SAME_SID="0"
WGT_SAME_SAID="1.0"
NMAX_SAME_SAID="0"
OPT_SFC=1   # 1: not modify, 2: modify
OPT_TV=12   # 1: do not use, 10-14: conv to T, 20: use as Tv
OPT_QERR=2 # 1: do not use, 2: conv RHerr
OBSERR_SCALE="1.0"
F_OBSERR="obserr.tbl"
#---------------------------------------------------------------
#
#---------------------------------------------------------------
DMPDIR="out/dump"
RUNDIR="out/thinning/$RUNNAME"
OBSDIR=$RUNDIR/obs
CNFDIR=$RUNDIR/conf
LOGDIR=$RUNDIR/log
MNTDIR=$RUNDIR/monit

mkdir -p $RUNDIR
mkdir -p $OBSDIR
mkdir -p $CNFDIR
mkdir -p $LOGDIR
mkdir -p $MNTDIR

[ ! -f $RUNDIR/$0 ] && cp $0 $RUNDIR/.
#---------------------------------------------------------------
# Loop for time
#---------------------------------------------------------------
while test $IY$IM$ID$IH -le $EY$EM$ED$EH; do

  DATETIME=$IY$IM$ID$IH
  #echo $DATETIME

  F_OBS="$OBSDIR/$DATETIME.dat"

  F_CONF=$CNFDIR/$DATETIME
  F_LOG=$LOGDIR/$DATETIME

  mkdir -p $MNTDIR/$DATETIME
  #-------------------------------------------------------------
  # Make config. file
  #-------------------------------------------------------------
  cat << EOF > $F_CONF
&nml_time
  datetime = $DATETIME,
  tout_bhd = $TOUT_BHD, tout_ahd = $TOUT_AHD,
/

&nml_file
  f_data  = "$DMPDIR/$IY/$DATETIME/data",
  f_table = "$DMPDIR/$IY/$DATETIME/table",
  f_obs   = "$F_OBS",
  replace_old_dumped_file = .false.,
/

&nml_geo
!  nlon = 3600,
!  nlat = 1800,
!  west = -180, east=180, south=-90, north=90,
!  is_south_to_north = .false.,
!  f_elv = "topo/cmf_v400_glb06min_elevtn.bin",
/

&nml_common
  nmax = $NMAX, 
  rho_h = $RHO_H,
  rho_v = $RHO_V,
  dnst_thresh = $DNST_THRESH,
  wgt_qlt = $WGT_QLT,
  wgt_same_sid   = $WGT_SAME_SID, 
  nmax_same_sid  = $NMAX_SAME_SID, 
  wgt_same_said  = $WGT_SAME_SAID, 
  nmax_same_said = $NMAX_SAME_SAID, 
  opt_sfc  = $OPT_SFC,  ! 1: not modify, 2: modify
  opt_Tv   = $OPT_TV,  ! 1: do not use, 2: conv to T, 3: use as Tv
  opt_Qerr = $OPT_QERR,  ! 1: do not use, 2: conv RHerr
  obserr_scale = $OBSERR_SCALE,
  f_obserr = $F_OBSERR,
/

&nml_constant
/

 &nml_msg typ='ADPUPA', method='dnst1', vars='U V T Q   ',/
 &nml_msg typ='AIRCAR', method='none ', vars='U V T Q   ',/
 &nml_msg typ='AIRCFT', method='dnst1', vars='U V T Q   ',/
 &nml_msg typ='SATWND', method='dnst1', vars='U V       ',/
 &nml_msg typ='PROFLR', method='dnst1', vars='U V       ',/
 &nml_msg typ='VADWND', method='dnst1', vars='U V       ',/
 &nml_msg typ='SATEMP', method='dnst1', vars='    T Q   ',/
 &nml_msg typ='ADPSFC', method='dnst1', vars='        Ps',/
 &nml_msg typ='SFCSHP', method='dnst1', vars='U V T Q Ps',/
 &nml_msg typ='SFCBOG', method='none ', vars='          ',/
 &nml_msg typ='SPSSMI', method='none ', vars='U V       ',/
 &nml_msg typ='SYNDAT', method='none ', vars='          ',/
 &nml_msg typ='ERS1DA', method='none ', vars='U V       ',/
 &nml_msg typ='GOESND', method='none ', vars='U V       ',/
 &nml_msg typ='QKSWND', method='none ', vars='U V       ',/
 &nml_msg typ='MSONET', method='none ', vars='U V       ',/
 &nml_msg typ='GPSIPW', method='none ', vars='      Q   ',/
 &nml_msg typ='RASSDA', method='none ', vars='    T     ',/
 &nml_msg typ='WDSATR', method='none ', vars='U V       ',/
 &nml_msg typ='ASCATW', method='dnst1', vars='U V       ',/

&nml_monitor
  monit_proc        = .false.,
  monit_itr_sct_rec = .false.,
  monit_itr_add_rec = .false.,
  save_modif = .false.,
  save_monit = .false.,
  dir_monit = "$MNTDIR/$DATETIME",
/

EOF
  #-------------------------------------------------------------
  # Make obs.
  #-------------------------------------------------------------
  rm -f $F_LOG

  echo "Log file: $F_LOG"

  srun ./main make_obs $F_CONF >> $F_LOG

  for T in $(seq $TOUT_BHD -1); do
    F_OBS_THIS=$F_OBS.prev`printf "%02d" $((-T))`
    echo "F_OBS_THIS" $F_OBS_THIS
    if [ -f $F_OBS_THIS ]; then
      DATETIME_NEW=`date +%Y%m%d%H --date "-$T hour $IY$IM$ID $IH"`
      F_OBS_NEW=$OBSDIR/$DATETIME_NEW.dat
      #echo $DATETIME_NEW
      mv $F_OBS_THIS $F_OBS_NEW
      echo "  obs data made on :: " $F_OBS_NEW
    fi
  done

  F_OBS_THIS=$F_OBS.00
  echo "F_OBS_THIS" $F_OBS_THIS
  if [ -f $F_OBS_THIS ]; then
    F_OBS_NEW=$OBSDIR/$DATETIME.dat
    mv $F_OBS_THIS $F_OBS_NEW
    echo "  obs data made on :: " $F_OBS_NEW
  fi

  for T in $(seq 1 $TOUT_AHD); do
    F_OBS_THIS=$F_OBS.next`printf "%02d" $T`
    echo "F_OBS_THIS" $F_OBS_THIS
    if [ -f $F_OBS_THIS ]; then
      DATETIME_NEW=`date +%Y%m%d%H --date "$T hour $IY$IM$ID $IH"`
      F_OBS_NEW=$OBSDIR/$DATETIME_NEW.dat
      #echo $DATETIME_NEW
      mv $F_OBS_THIS $F_OBS_NEW
      echo "  obs data made on :: " $F_OBS_NEW
    fi
  done

  #echo "  obs data made on :: " $OBSDIR/$DATETIME.dat " for " $EXP " from " $F_PREPBUFR
  TYTMTDTH=`get_next_time_step $IY $IM $ID $IH`
  TY=`echo $TYTMTDTH | cut -c1-4`
  TM=`echo $TYTMTDTH | cut -c5-6`
  TD=`echo $TYTMTDTH | cut -c7-8`
  TH=`echo $TYTMTDTH | cut -c9-10`

  IY=$TY
  IM=$TM
  ID=$TD
  IH=$TH
done  # while test $IY$IM$ID$IH -le $EY$EM$ED$EH
#---------------------------------------------------------------
#
#---------------------------------------------------------------
echo "NORMAL END"
