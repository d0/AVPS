/*
 * ue41.c - assignment 4.1
 * {belikov|birkholz|oepen}@informatik.hu-berlin.de
 *
 * compile:  gcc -O2 -fopenmp ue41.c -o ue41 -Wall -Wextra -std=c99
 * run:      ./ue41 <P> <N>
 *
 * notes:   create array of size N with a[i] = 1 and sum using OpenMP
 *          fork/join parallelism, P specifies the number of threads to be used
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <omp.h>

#define DEFAULT_ARR_SIZE 1000000
#define DECIMAL 10


int main(int argc, char *argv[]) {
    long arr_size = 0, sum;
    int  *a, nthreads = 1, pad_size = 0, chunk_size = 1, i;
    double elapsed = 0.0;

    if (argc > 1) {
        nthreads = strtol(argv[1], NULL, DECIMAL);
        if (nthreads <= 0) {
            nthreads = omp_get_num_procs();
            fprintf(stderr, "WARNING: number of threads must be positive integer (set to %d)\n", nthreads);
        }
        else if (nthreads > omp_get_max_threads()) {
            nthreads = omp_get_max_threads();
            fprintf(stderr, "WARNING: specified thread number too large (set to %d)\n", nthreads);
        }
    }
    else {
        nthreads = omp_get_num_procs(); /* default: set to number of PEs */
    }
    omp_set_num_threads(nthreads);

    if (argc > 2) {
        arr_size = strtol(argv[2], NULL, DECIMAL);
    }

    if (arr_size <= 0) {
        arr_size = DEFAULT_ARR_SIZE;
    }

    if (arr_size % nthreads) {
        pad_size   = nthreads - arr_size % nthreads;
    }
    chunk_size = (arr_size + pad_size) / nthreads;

    if ((a = malloc((arr_size + pad_size) * sizeof(int))) == NULL) {
        fprintf(stderr, "FATAL ERROR: out of memory\n");
        exit(EXIT_FAILURE);
    }

    elapsed -= omp_get_wtime();

    for (i = 0; i < arr_size; ++i) {
        a[i] = 1;
    }

    if (pad_size) {                /* set pad elements to 0  */
        memset(&a + arr_size, 0, pad_size * sizeof(int));
    }

    sum = 0;
    #pragma omp parallel for default(shared) private(i) \
                             schedule(static, chunk_size) reduction(+:sum)
    for (i = 0; i < arr_size + pad_size; ++i) {
        sum += a[i];
    }

    elapsed += omp_get_wtime();

    assert(sum == arr_size); /* verify the result */

    printf("It took %lf seconds to sum up %ld ones using %d threads.\n", elapsed, arr_size, nthreads);

    free(a);
    return EXIT_SUCCESS;
}