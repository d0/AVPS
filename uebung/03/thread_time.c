/*
 * thread_time.c - assignment 3.1 b)
 *
 * compile:  gcc -O2 thread_time.c -o thread_time -Wall -Wextra -std=c99 -lpthread
 * run:      ./thread_time num_threads
 *        
 * notes:   Spawn num_threads threads. Measure the time needed and compute
 *          the mean time needed to create one thread.   
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <time.h>
#include <sys/types.h>
#include <sys/time.h>
#include <pthread.h>

#define STD_ITER 500

/* Print usage */
void usage(void) {
    printf("thread_time takes at most one parameter.\n\
Usage is nthreads n, where n defines the number of threads to be created.\n\
If you don't specifiy n, %d threads will be created.\n\
Of course nthreads must be an integer\n", STD_ITER);
    return;
}

/* Difference between two timesteps in microseconds*/
uint64_t time_diff(struct timeval start, struct timeval stop) {
    return (stop.tv_sec * 1000000 + stop.tv_usec) - (start.tv_sec * 1000000 + start.tv_usec);
}

/* Exit immidiately */
void *run_thread(void *tid) {
    pthread_exit(0);
}

int main(int argc, char** argv) {
    pthread_t *threads;
    struct timeval start, stop;
    uint64_t delta_t = 0;
    int i, nthreads = STD_ITER;

    /* Parse command line arguments */
    if (argc == 2) {
        nthreads = atoi(argv[1]);
        if (nthreads < 2) {
            usage();
            exit(-1);
        }
    } else if (argc > 2) {

        usage();
        exit(-1);
    }

    threads = (pthread_t*) malloc(nthreads * sizeof(pthread_t));

    gettimeofday(&start, NULL);
    for (i = 0; i < nthreads; i++) {
        pthread_create(&threads[i], NULL, run_thread, NULL);
        /* parent continues to run */
    }
    gettimeofday(&stop, NULL);
  
    for (i = 0; i < nthreads; i++)
        pthread_join(threads[i], NULL);

    delta_t = time_diff(start, stop); 
    printf("Needed %u microseconds for spawning %d threads. Mean time for thread creation: %lf microseconds\n",
            (uint32_t) delta_t, nthreads, (double) delta_t /  (double) nthreads);

    free(threads);
    return 0;
}

