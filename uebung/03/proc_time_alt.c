/*
 * proc_time_alt.c - alternative solution for assignment 3.1 a)
 *
 * compile:  gcc -O2 proc_time_alt.c -o proc_time_alt -Wall -Wextra -std=c99
 * run:      ./proc_time
 *
 *          
 * notes:   Measure the time for one fork() call
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <time.h>
#include <sys/types.h>
#include <sys/time.h>

/* Difference between two timesteps in microseconds*/
uint64_t time_diff(struct timeval start, struct timeval stop) {
    return (stop.tv_sec * 1000000 + stop.tv_usec) - (start.tv_sec * 1000000 + start.tv_usec);
}

int main(int argc, char** argv) {
    pid_t pid;
    struct timeval start, stop;
    uint64_t delta_t = 0;

    gettimeofday(&start, NULL);
    pid = fork();
    gettimeofday(&stop, NULL);
    delta_t = time_diff(start, stop); 

    if (pid < 0) {
        exit(-1);
    } else if (pid == 0) { /* parent */
        printf("%u\n", (uint32_t) delta_t);
    }
    
    return 0;
}

