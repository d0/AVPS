/*
 * proc_time.c - assignment 3.1 a)
 *
 * compile:  gcc -O2 proc_time.c -o proc_time -Wall -Wextra -std=c99
 * run:      ./proc_time num_procs
 *          
 * notes:   Spawn nprocs processes. Measure the time needed and compute
 *          the mean time needed to create one process.   
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <time.h>
#include <sys/types.h>
#include <sys/time.h>

#define STD_ITER 500

/* Print usage */
void usage(void) {
    printf("proc_time takes at most one parameter.\n\
Usage is nprocs n, where n defines the number of processes to be created.\n\
If you don't specifiy n, %d processes will be created.\n\
Of course nprocs must be an integer\n", STD_ITER);
    return;
}

/* Difference between two timesteps in microseconds*/
uint64_t time_diff(struct timeval start, struct timeval stop) {
    return (stop.tv_sec * 1000000 + stop.tv_usec) - (start.tv_sec * 1000000 + start.tv_usec);
}

int main(int argc, char** argv) {
    pid_t pid;
    struct timeval start, stop;
    uint64_t delta_t = 0;
    int i, nprocs = STD_ITER;

    /* Parse command line arguments */
    if (argc == 2) {
        nprocs = atoi(argv[1]);
        if (nprocs < 2) {
            usage();
            exit(-1);
        }
    } else if (argc > 2) {

        usage();
        exit(-1);
    }

    gettimeofday(&start, NULL);
    for (i = 0; i < nprocs; i++) {
        pid = fork();
        if (pid != 0) { /* child quits immidiately. Also exit on error */
           return 0; 
        }
        /* parent continues to run */
    }
    gettimeofday(&stop, NULL);
    delta_t = time_diff(start, stop); 
    
    printf("Needed %u microseconds for spawning %d processes. Mean time for process creation: %lf microseconds\n",
            (uint32_t) delta_t, nprocs, (double) delta_t /  (double) nprocs);

    return 0;
}

