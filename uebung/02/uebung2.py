#!/usr/bin/env python

from subprocess import call, Popen, PIPE
from itertools import product
from numpy import mean, std 
import csv

prog = "uebung2"
num_procs = ["2", "4", "8"]
methods = ["send", "ssend", "bsend"]
sizes = ["1024", "1048576", "33554432"]

iterations = 10 #Number of iterations for each command

results = [] 

out = csv.writer(open("local.csv", "wb"))

for p in product(num_procs, methods, sizes):
    cmd = "mpirun -np " + p[0] + " ./" + prog + " --method=" + p[1] + " --size=" + p[2]
    print cmd
    seconds = []
    for i in range(iterations):
        proc = Popen(cmd, shell=True, stdout=PIPE)
        output = proc.communicate()
        #Fetch the time from the output.
        #I know this is extremly ugly but it works.
        duration = output[0][14:-3]
        #print seconds
        seconds.append(float(duration))
    #compute mean duration and standard derivation for the command   
    out.writerow([p[0], p[1], p[2], mean(seconds), std(seconds)])
