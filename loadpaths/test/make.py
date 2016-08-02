import os, datetime

if os.path.isfile("./make.log"):
	os.system("rm -r ./make.log")

log = open("./make.log", "a")
log.write("Starting at " + datetime.datetime.now().strftime("%m/%d/%y:%H:%M:%S") + "\n")
log.close()

log = open("./make.log", "a")
os.system("Rscript test.R  >>  ./make.log")
log.close()

log = open("./make.log", "a")
log.write("Finished at " + datetime.datetime.now().strftime("%m/%d/%y:%H:%M:%S") + "\n")
log.close() 	