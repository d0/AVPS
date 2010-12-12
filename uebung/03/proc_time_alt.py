#!/usr/bin/env python

import sys 
from subprocess import call, Popen, PIPE
from numpy import mean, std, median

prog = "./proc_time_alt"
num_procs = 500

results = [] 

#read num_procs from cmd line args
assert len(sys.argv) <= 2
if (len(sys.argv) == 2):
    num_procs = int(sys.argv[1])

for i in range(num_procs):
    proc = Popen(prog, shell=True, stdout=PIPE)
    output = proc.communicate()
    results.append(int(output[0][:-1]))

print "N=" + str(num_procs)
print "Mean: " + str(mean(results))
print "Median: " + str(median(results))
print "Std. derivation: " + str(std(results))
