#!/bin/bash

## Change log
## - FI25 removed 20180425 (atarting from 2018_DJF)

#set -ex
archive='markku@hirlam.org:/home/hirlam/mastdata/ArchiveData'
mastkey=/home/fnm/.ssh/mastkey_markku
##archive='markku@hirlam.org:/home/hirlam/mastdata/'

MASTLIST="SODA CABA LIND"
##MASTLIST="SODA CABA LIND VALL"
for THISYEAR in 2022 2023
do 
for THISMONTH in DJF MAM JJA SON
do
#THISYEAR=2022
#THISMONTH='DJF'

case $THISMONTH in
    01|03|05|07|08|10|12) 
    SDATE=${THISYEAR}${THISMONTH}01
    EDATE=${THISYEAR}${THISMONTH}31
    keylist="${THISYEAR}${THISMONTH}??"
    ;;
    04|06|09|11)          
    SDATE=${THISYEAR}${THISMONTH}01
    EDATE=${THISYEAR}${THISMONTH}30
    keylist="${THISYEAR}${THISMONTH}??"
    ;;
    02)                   
    SDATE=${THISYEAR}${THISMONTH}01
    EDATE=${THISYEAR}${THISMONTH}28
    keylist="${THISYEAR}${THISMONTH}??"
    ;;
    DJF)
    SDATE=$[$THISYEAR-1]1201
    EDATE=${THISYEAR}0228
    keylist="$[$THISYEAR-1]12?? ${THISYEAR}0[1,2]??"
    ;;
    MAM)
    SDATE=${THISYEAR}0301
    EDATE=${THISYEAR}0531
    keylist="${THISYEAR}0[3,4,5]??"
    ;;
    JJA)
    SDATE=${THISYEAR}0601
    EDATE=${THISYEAR}0831
    keylist="${THISYEAR}0[6,7,8]??"
    ;;
    SON)
    SDATE=${THISYEAR}0901
    EDATE=${THISYEAR}1130
    keylist="${THISYEAR}09?? ${THISYEAR}1[0,1]??"
    ;;
esac 

echo $SDATE $EDATE


DBASE="/ec/res4/scratch/fnm/MastVerifData/${THISYEAR}/${THISMONTH}"

#DBASE="/lustre/tmp/mkangas"
#DBASE="/home/forteliu/hirlam/trunk/harmonie/util/monitor/mastdata"

export THISYEAR THISMONTH THISMAST EXP OBSPATH MODPATH SDATE EDATE

for THISMAST in $MASTLIST
do
   case $THISMAST in
      "SODA") EXP="EC01 FRAR MEP0";;
      "CABA") EXP="EC01 FRAR FARO MEP0";;
      "LIND") EXP="EC01 FRAR FARO";;
###      "VALL") EXP="EC01 FRAR FARO FI07 ";;
   esac
# Historical:
#   case $THISMAST in
#       "SODA") EXP="EC01 FRAR FI15 FI75 SP16 FIAR";;
#       "CABA") EXP="EC01 FRAR FI15 FI75 SP16 FRAL";;
#       "LIND") EXP="EC01 FRAR FI15 FI75 SP16 FRAL";;
#       "VALL") EXP="FRAR FI15 SP16 FRAL";;
#   esac
   OBSPATH=${DBASE}/obs/${THISMAST}/
   MODPATH="" 
   mkdir -p $OBSPATH
   for key in $keylist
  do
      scp -i ${mastkey} ${archive}/../Meas_${THISMAST}_Mast_${key}.txt $OBSPATH
      scp -i ${mastkey} ${archive}/../Meas_${THISMAST}_Flux_${key}.txt $OBSPATH
#      echo "No data are copied"1
   done
  #./DeleteEmpty.pl ${OBSPATH}/*
  ./check_file.bash ${OBSPATH}/ # remove bad files (hardo coded expected number of values!)
   
   for exp in $EXP
   do
      moddir=${DBASE}/fc/${THISMAST}/${exp}/
      MODPATH=${MODPATH}' '${DBASE}/fc/${THISMAST}/${exp}/
      mkdir -p $moddir
      for key in $keylist
      do
        scp  -i  ${mastkey} ${archive}/Fcst_${exp}_${THISMAST}_${key}_00.txt $moddir
#        echo "No forecasts are copied"
      done
   ./DeleteEmpty.pl ${moddir}/*
   done
   #./Run_verobs_all Env_exp_mastverif

done

done
done


   
#### Make sure all data is mde available first. Run_verobs_all Env_exp_Multimast

