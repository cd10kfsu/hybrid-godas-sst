#/bin/sh
set -e

#Configurations
START_DATE=20060603
END_DATE=20060606

ROOT_DIR=$(pwd)
WORK_DIR=$ROOT_DIR/obs_nc2dat_cfs
OBSIN_DIR=$ROOT_DIR/DATA/obs/sst_cmc0d2  #obs in (.nc)
OBSOUT_DIR=$ROOT_DIR/DATA/obs/sst_cmc0d2/CFSOBS

basehour="12"
ulimit -s unlimited

# setup working directory
mkdir -p $WORK_DIR
cd $WORK_DIR/
# copy executable file 
cp $ROOT_DIR/build/obsprep_nc2dat_cfs ./ 


# Loop for all obs
cdate=$START_DATE
while [[ $cdate -le $END_DATE ]]
do
	echo Processing $cdate
        yr=${cdate:0:4}
        yrmn=${cdate:0:6}
        odir=$OBSOUT_DIR/$yr/$yrmn/$cdate
	mkdir -p $odir

	# run nc2dat: ./obsprep_nc2dat <file in> <file out> 
	for f in $OBSIN_DIR/$yr/$cdate/*.nc; do
	    echo $f
	    ./obsprep_nc2dat_cfs $f out.${cdate}${basehour}.dat
        done

        mv out.${cdate}${basehour}.dat $odir/${cdate}.dat

	# end of loop, prepare for the next date
	cdate=$(date -d "$cdate + 1 day" "+%Y%m%d")
done

echo Finish running! Check out the results





