#!/bin/bash

function get_dymax() {
  yy=$1
  mm=$2

  if [ $mm -eq 2 ];then
    dymax=28
    if [ $((yy % 4)) -eq 0 ]; then
      dymax=29
      if [ $((yy % 100)) -eq 0 ]; then
        dymax=28
        if [ $((yy % 400)) -eq 0 ]; then
          dymax=29
        fi
      fi
    fi
  elif [ $mm -eq 1 -o $mm -eq 3 -o $mm -eq 5 -o \
         $mm -eq 7 -o $mm -eq 8 -o $mm -eq 10 -o $mm -eq 12 ]; then
    dymax=31
  elif [ $mm -eq 4 -o $mm -eq 6 -o $mm -eq 9 -o $mm -eq 11 ]; then
    dymax=30
  else
    echo "Invalid value in \$mm: $mm"
    exit 1
  fi

  echo $dymax
}


function get_next_time_step() {
  yy=`echo $(( 10#$1 ))`
  mm=`echo $(( 10#$2 ))`
  dd=`echo $(( 10#$3 ))`
  hh=`echo $(( 10#$4 ))`

  dymax=`get_dymax $yy $mm`
  if [ $hh -eq 18 ]; then
    if [ $dd -eq $dymax ]; then
      if [ $mm -eq 12 ]; then
        yy=$((yy+1))
        mm=1
      else
        mm=$((mm+1))
      fi
      dd=1
    else
      dd=$((dd+1))
    fi
    hh=0
  else
    hh=$((hh+6))
  fi

  printf "%04d%02d%02d%02d\n" $yy $mm $dd $hh
}
