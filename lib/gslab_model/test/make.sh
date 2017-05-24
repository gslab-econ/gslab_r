export LOG=make.log
export OUT=output
export SCRATCH=.

rm $LOG
mkdir -p $OUT-data
mkdir -p $OUT-log
mkdir -p $OUT-results
mkdir -p $SCRATCH/$OUT-data

echo "Starting at " $(date +%D:%H:%M:%S) >> $LOG

export TESTING=TRUE

Rscript run_suite.R >> $LOG 2>&1

rm -rf $OUT-data
rm -rf $OUT-log
rm -rf $OUT-results
rm -rf $SCRATCH/$OUT-data

echo "Finished at " $(date +%D:%H:%M:%S) >> $LOG