#!/bin/bash
set -e

# Configurables
START_DATE=20060601
END_DATE=20060601

ROOT_DIR=$(pwd)
GRID_DIR=$ROOT_DIR/DATA/grid
WORK_DIR=$ROOT_DIR/WORK_ssh2
OBSIN_DIR=$ROOT_DIR/DATA/obs/ssh_adt/raw
OBSOUT_DIR=$ROOT_DIR/DATA/obs/ssh_adt/test


#------------------------------------------------------------
ulimit -s unlimited

# setup working directory
mkdir -p $WORK_DIR
cd $WORK_DIR
cp $ROOT_DIR/run/config.exp_default/da/obsprep.nml obsprep.nml
ln -sf $ROOT_DIR/build/obsprep_adt .
ln -sf $ROOT_DIR/build/obsprep_combine .
mkdir -p INPUT
cd INPUT
ln -sf $GRID_DIR/* .
mv hgrid.nc grid.nc
cd ..

# loop over all the dates of interest
cdate=$START_DATE
while [[ $cdate -le $END_DATE ]]
do
    echo Processing $cdate
    yr=${cdate:0:4}

    ## do the superobbing based on input files
    #f_all=''
    #for f in $OBSIN_DIR/$yr/$cdate/rads_adt_*.nc; do
    #    echo "ln -sf $f ."
    #    ln -sf $f .
    #    f2=`echo $f|rev|cut -d "/" -f 1 |rev`
    #    f_all="$f_all $f2"
    #done
    #echo "$f_all"
    #./obsprep_adt sob.${cdate}.nc $f_all


    ## combine all the satellite tracks into a single day
    #odir=$OBSOUT_DIR/$yr/$cdate
    #mkdir -p $odir
    # 
    #basedate=$(date "+%Y,%m,%d,0,0,0" -d "$cdate")
    #./obsprep_combine -basedate $basedate sob.${cdate}.nc $odir/$cdate.nc


    # do the superobbing based on input files
    for f in $OBSIN_DIR/$yr/$cdate/rads_adt_*.nc; do
        echo "ln -sf $f ."
        ln -sf $f .
        f2=`echo $f|rev|cut -d "/" -f 1 |rev`
        echo "./obsprep_adt sob.$f2 $f"
        ./obsprep_adt sob.$f2 $f
    done

    exit 0


    # combine all the satellite tracks into a single day
    odir=$OBSOUT_DIR/$yr/$cdate
    mkdir -p $odir
 
    basedate=$(date "+%Y,%m,%d,0,0,0" -d "$cdate")
    echo "./obsprep_combine -basedate $basedate sob.*.nc $odir/$cdate.nc"
    ./obsprep_combine -basedate $basedate sob.*.nc $odir/$cdate.nc

    rm -f rads_adt_*.nc
    rm -f sob.*.nc

    
    # end of loop, prepare for the next date
    cdate=$(date -d "$cdate + 1 day" "+%Y%m%d")
done

