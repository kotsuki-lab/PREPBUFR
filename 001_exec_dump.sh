#!/bin/sh
#=======================================================================
# environment::
#   singularity run --bind /data02,/data10 ../share/oneapi-hpckit_latest.sif
#     - bind directories (e.g., data02 data10) should be modified by users
#=======================================================================
#set -ex
#pwd
. ./000_func_calendar.sh

DIR_PREPBUFR="../../data_prepbufr/unzip"

if [ $# -eq 2 ]; then
  IY=`echo $(( 10#$1 ))`
  IM=`echo $(( 10#$2 ))`
  ID=0
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
#---------------------------------------------------------------
#
#---------------------------------------------------------------
DMPDIR="out/dump"

mkdir -p $DMPDIR

cp $0 prepbufr/.
#---------------------------------------------------------------
# Loop for time
#---------------------------------------------------------------
while test $IY$IM$ID$IH -le $EY$EM$ED$EH; do

  DATETIME=$IY$IM$ID$IH
  echo $DATETIME

  IS_OK=0
  if [ $IY$IM$ID -le 20080630 ]; then
    F_PREPBUFR="$DIR_PREPBUFR/$IY/$IY$IM$ID/prepbufr.gdas.$IY$IM$ID$IH.wo40"
    if [ ! -f $F_PREPBUFR ]; then
      echo "FILE NOT FOUND: ${F_PREPBUFR}"
      IS_OK=1
    fi
  else
    F_PREPBUFR="$DIR_PREPBUFR/$IY/$IY$IM$ID/prepbufr.gdas.$IY$IM$ID.t${IH}z.nr"
    if [ ! -f $F_PREPBUFR ]; then
      F_PREPBUFR2="$DIR_PREPBUFR/$IY/$IY$IM$ID/prepbufr.gdas.$IY$IM$ID$IH.nr"
      if [ ! -f $F_PREPBUFR2 ]; then
        echo "FILE NOT FOUND: ${F_PREPBUFR}"
        IS_OK=1
      fi
      F_PREPBUFR=$F_PREPBUFR2
    fi
  fi

  if [ $IS_OK -eq 0 ]; then
    F_CONF="$DMPDIR/$IY/$DATETIME/conf"
    F_LOG="$DMPDIR/$IY/$DATETIME/log"
    mkdir -p "$DMPDIR/$IY/$DATETIME"
    #-----------------------------------------------------------
    # Make config. file
    #-----------------------------------------------------------
    cat << EOF > $F_CONF
&nml_time
  datetime = $DATETIME,
/

&nml_file
  f_pb    = "$F_PREPBUFR",
  f_data  = "$DMPDIR/$IY/$DATETIME/data",
  f_table = "$DMPDIR/$IY/$DATETIME/table",
  f_obs   = "",
  replace_old_dumped_file = .true.,
/
EOF
    #-----------------------------------------------------------
    # Make obs.
    #-----------------------------------------------------------
    rm -f $F_LOG

    echo "Log file: $F_LOG"

    srun ./main dump $F_CONF >> $F_LOG
  fi

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
