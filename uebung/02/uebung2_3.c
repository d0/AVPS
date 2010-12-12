/*
 * uebung_2_3.c: assignment 2.3
 *
 * compile:  mpicc -O2 -o deadlock deadlock.c
 * run:      mpirun -machinefile <mf> -np <nprocs> ./deadlock 
 *
 * authors: Evgenij Belikov (belikov@informatik.hu-berlin.de)
 *          Jan Birkholz (jbirkhol@informatik.hu-berlin.de)
 *          Dominik Oepen (oepen@informatik.hu-berlin.de)
 *          
 * notes:  Simulate the deadlock or reversed message ordering
 *         problems discussed in the lecture.
 *
 *         For the deadlock problem one solution is implemented:
 *         by using buffered send methods the problem is avoided 
 *
 *         For the reversed message order three solutions are implemented:
 *          -Using synchronization via MPI_Barrier. This can be done at two 
 *           differents point in the programm flow
 *          -Using only synchronous communication
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <mpi.h>

#define TAG 0
#define DEADLOCK 0
#define ORDER 1

static void err(const char *msg);
static void print_usage();

static int debug = 0;
static int help = 0;
static char *s = NULL;

static const char helptext[] = "MPI programm to sum up the elements of an array.\n\
Options: \n\
\t -h, --help:          show this help\n\
\t -d, --debug:         enable debug mode\n\
\t -p, --problem=prob:  problem to be simulated. Possible values are deadlock and order\n";

static const struct option long_options[] =
{
    {"debug",   no_argument,        0, 'd'},
    {"help",    no_argument,        0, 'h'},
    {"problem", required_argument,  0, 'p'},
    {"solution",required_argument,  0, 's'},
    {0,         0,                  0,  0 }
};

void err(const char *msg) {
    printf("%s", msg);
    MPI_Finalize();
    exit(-1);
}

void print_usage() {
    printf("%s", helptext);
    return;
}

int main(int argc, char** argv) {
    int myrank, nprocs, i, problem = -1, solution = 0;
    MPI_Status stat;

    /* MPI Initialisation */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);

    /* Parse command line arguments */
    while (optind < argc) {
        i = getopt_long(argc, argv, "dhp:", long_options, NULL);
        if (i == -1)
            break;
        switch(i) {
            case 'd':
                debug++;
                break;
            case 'h':
                help++;
                break;
            case 'p':
                s = optarg;
                if (s) {
                    if (!strcmp(s, "deadlock"))
                        problem = DEADLOCK;
                    else if (!strcmp(s, "order"))
                        problem = ORDER;
                    else
                        err("Unknown problem\n");
                }
            case 's':
                s = optarg;
                if (s) {
                    solution = atoi(s);
                    if ((solution < 0 || solution > 3))
                        err("Unknown solution\n");
                }
            default:
                break;
        }
    }

    if (help) {
        if (myrank == 0) /* Only print usage once */
            print_usage();
        MPI_Finalize();
        return 0;
    }
    
    /* Sanity check */
    if ((problem == DEADLOCK && nprocs < 2) || (problem == ORDER && nprocs < 3))
        err("Not enough processes\n");

    if (problem == DEADLOCK) { /* Deadlock simulation */
        if (myrank == 0) { /* rank 0 */
            double time;

            time = MPI_Wtime();

            /* Send one integer to process 1 */
            i = 42;
            if (solution == 1) {
                void *buf = NULL;
                int size = 0;
                size = sizeof(int) + MPI_BSEND_OVERHEAD; 
                buf = malloc(size);
                if (!buf)
                    err("Failed to allocate memory\n");
                MPI_Buffer_attach(buf, size);
                MPI_Bsend(&i, 1, MPI_INT, (myrank + 1) % 2, TAG, MPI_COMM_WORLD);
                MPI_Buffer_detach(buf, &size);
                free(buf);
            } else {
                MPI_Ssend(&i, 1, MPI_INT, (myrank + 1) % 2, TAG, MPI_COMM_WORLD);
            }
            /* Receive one integer from process 1 */
            MPI_Recv(&i, 1, MPI_INT, (myrank + 1) % 2, TAG, MPI_COMM_WORLD, &stat);

            printf("Process %d: Done\n", myrank);

        } else if (myrank == 1) { /* rank 1 */
            double time;

            time = MPI_Wtime();
            /* Receive one integer from process 1 (reversed order if solution == 2) */
            if (solution == 2)
                MPI_Recv(&i, 1, MPI_INT, (myrank + 1) % 2, TAG, MPI_COMM_WORLD, &stat);

            /* Send one integer to process 1 */
            i = 42;
            if (solution == 1) {
                void *buf = NULL;
                int size = 0;
                size = sizeof(int) + MPI_BSEND_OVERHEAD; 
                buf = malloc(size);
                if (!buf)
                    err("Failed to allocate memory\n");
                MPI_Buffer_attach(buf, size);
                MPI_Bsend(&i, 1, MPI_INT, (myrank + 1) % 2, TAG, MPI_COMM_WORLD);
                MPI_Buffer_detach(buf, &size);
                free(buf);
            } else {
                MPI_Ssend(&i, 1, MPI_INT, (myrank + 1) % 2, TAG, MPI_COMM_WORLD);
            }

            /* Receive one integer from process 1 (reversed order if solution == 2) */
            if (solution != 2)
                MPI_Recv(&i, 1, MPI_INT, (myrank + 1) % 2, TAG, MPI_COMM_WORLD, &stat);

            printf("Process %d: Done\n", myrank);
        } else { /* rank > 1 */
            /* Do nothing */
        }
    } else if (problem == ORDER) { /* Out of order message receipt simulation */
        if (myrank == 0) {
            i = 42;
            /* Send message to process 2 */
            if (solution == 3)
                MPI_Ssend(&i, 1, MPI_INT, 2, TAG, MPI_COMM_WORLD);
            else
                MPI_Send(&i, 1, MPI_INT, 2, TAG, MPI_COMM_WORLD);
            
            if (solution == 1)
                MPI_Barrier(MPI_COMM_WORLD);
            
            i = 23;
            /* Send message to process 1 */
            MPI_Ssend(&i, 1, MPI_INT, 1, TAG, MPI_COMM_WORLD);
            if (solution == 2)
                MPI_Barrier(MPI_COMM_WORLD);

        } else if (myrank == 1) {
            if (solution == 1)
                MPI_Barrier(MPI_COMM_WORLD);
            
            /* Receive message from process 0 */
            MPI_Recv(&i, 1, MPI_INT, 0, TAG, MPI_COMM_WORLD, &stat);
            
            if (solution == 2)
                MPI_Barrier(MPI_COMM_WORLD);
            
            /* Send message to process 2 */
            if (solution == 3)
                MPI_Ssend(&i, 1, MPI_INT, 2, TAG, MPI_COMM_WORLD);
            else
                MPI_Send(&i, 1, MPI_INT, 2, TAG, MPI_COMM_WORLD);
        } else if (myrank == 2) {
           
            /* Receive message from process ? */
            MPI_Recv(&i, 1, MPI_INT, MPI_ANY_SOURCE, TAG, MPI_COMM_WORLD, &stat);
            printf("Received message:%d from process %d\n", i, stat.MPI_SOURCE);
            
            if ((solution == 1) || (solution == 2))
                MPI_Barrier(MPI_COMM_WORLD);
            
            /* Receive message from process ? */
            MPI_Recv(&i, 1, MPI_INT, MPI_ANY_SOURCE, TAG, MPI_COMM_WORLD, &stat);
            printf("Received message:%d from process %d\n", i, stat.MPI_SOURCE);
        } else { /* rank > 3*/
            /* Do nothing */
        }
    }

    MPI_Finalize();
    return 0;
}

