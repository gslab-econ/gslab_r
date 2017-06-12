import os
import datetime

# Run the tests
source = 'run_suite.R'
log    = 'test.log'
os.system('rm %s' %log)
os.system('Rscript %s >> %s 2>&1' %(source, log))
