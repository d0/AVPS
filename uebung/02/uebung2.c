/*
 * uebung2.c - assignment 2
 *
 * compile:  mpicc -O2 -o uebung1 uebung2.c
 * run:      mpirun -machinefile <mf> -np <nprocs> ./uebung2 -s array_size -d -m method
 *
 * authors: Evgenij Belikov (belikov@informatik.hu-berlin.de)
 *          Jan Birkholz (jbirkhol@informatik.hu-berlin.de)
 *          Dominik Oepen (oepen@informatik.hu-berlin.de)
 *          
 * notes:   The first nprocs % array_size processes receive one more
 *          element then the rest.
 *          The master process (rank 0) does not receive a chunk of data
 *          but does only distribute the data, gather the partial sums and
 *          add them up to find the total sum (Boss-Worker model). 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <mpi.h>

#define DEFAULT_SIZE 1024
#define TAG 0

enum methods {SEND, SSEND, BSEND, RSEND};

static void err(const char *msg);
static void print_usage();

static int debug = 0;
static int help = 0;
static char *s = NULL;

static const char helptext[] = "MPI programm to sum up the elements of an array.\n\
Options: \n\
\t -h, --help: show this help\n\
\t -d, --debug: enable debug mode\n\
\t -s, --size=num: size of the array\n\
\t -m, --method=meth: method to be used for sending. Valid methods are send, ssend, bsend and rsend\n";

static const struct option long_options[] = 
{
    {"debug",   no_argument,        0, 'd'},
    {"help",    no_argument,        0, 'h'},
    {"size",    required_argument,  0, 's'},
    {"method",  required_argument,  0, 'm'},
    {0,         0,                  0,  0}
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
    int myrank, nprocs, array_size = DEFAULT_SIZE, i, chunk_size, remainder;
    int sum = 0, global_sum = 0, buf_size;
    int *array = NULL, *pos = NULL, *buf = NULL; 
    MPI_Status stat;
    enum methods method = SEND;

    /* MPI Initialisation */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);

    /* Parse command line arguments */
    while (optind < argc) {
        i = getopt_long(argc, argv, "dhs:m:", long_options, NULL);
        if (i == -1)
            break;
        switch(i) {
            case 'd':
                debug++;
                break;
            case 'h':
                help++;
                break;
            case 's':
                s = optarg;
                if (s) {
                    array_size = atoi(s);
                    if (array_size <= 0)
                        err("Invalid array size\n");
                }
                break;
            case 'm':
                s = optarg;
                if (s) {
                    if (!strcmp(s, "send"))
                        method = SEND;
                    else if (!strcmp(s, "ssend"))
                        method = SSEND;
                    else if (!strcmp(s, "bsend"))
                        method = BSEND;
                    else if (!strcmp(s, "rsend"))
                        method = RSEND;
                    else 
                        printf("Unknown method %s. Using default method send\n", s);
                }
                break;                        
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

    /* Allocate memory for the array */
    array = (int*) malloc(array_size * sizeof(int));
    if (!array)
        err("Failed to allocate memory\n");

    /* Sanity check */
    if (nprocs < 2)
        err("Not enough processes\n");

    if (myrank == 0) { /* rank 0 */
        double time;
        time = MPI_Wtime();
      
        /* Allocate memory for buffered send if needed */
        if (method == BSEND) {
            buf_size = array_size * sizeof(int) + nprocs * MPI_BSEND_OVERHEAD;
            buf = (int*) malloc(buf_size);
            if (!buf)
                err("Failed to allocate memory\n");
            MPI_Buffer_attach(buf, buf_size);
        }
      
        if (debug) { 
            printf("Started at time: %f, using an array of size %d and %d processes\n", time, array_size, nprocs);
            printf("Using method: ");
            switch (method) {
                case SEND:
                    printf("MPI_SEND\n");
                    break;
                case SSEND:
                    printf("MPI_Ssend\n");
                    break;
                case BSEND:
                    printf("MPI_Bsend\n");
                    break;
                case RSEND:
                    printf("MPI_Rsend\n");
                    break;
                default:
                    err("Unknown method\n");
            }
        }
        
        for (i = 0; i < array_size; i++)
            array[i] = 1;

        /* Compute chunk size for each process. */
        chunk_size = array_size / (nprocs - 1);
        remainder = array_size % (nprocs - 1);
        if (debug)
            printf("chunk_size: %d, remainder: %d\n", chunk_size, remainder);

        pos = array;

        /* Distribute the array among the processes. If the array cannot be
         * distributed equally, the first processes recieve one additional
         * element */
        for(i=0; i<remainder; i++) {
            switch (method) {
                case SEND:
                    MPI_Send(pos, chunk_size + 1, MPI_INT, i + 1, TAG, MPI_COMM_WORLD);
                    break;
                case SSEND:
                    MPI_Ssend(pos, chunk_size + 1, MPI_INT, i + 1, TAG, MPI_COMM_WORLD);
                    break;
                case BSEND:
                    MPI_Bsend(pos, chunk_size + 1, MPI_INT, i + 1, TAG, MPI_COMM_WORLD);
                    break;
                case RSEND:
                    /* Synchronize */
                default:
                    err("Unknown send method\n");
        }
            pos += chunk_size + 1;
        }
        for(i=remainder; i<nprocs - 1; i++) {
            switch (method) {
                case SEND:
                    MPI_Send(pos, chunk_size, MPI_INT, i + 1, TAG, MPI_COMM_WORLD);
                    break;
                case SSEND:
                    MPI_Ssend(pos, chunk_size, MPI_INT, i + 1, TAG, MPI_COMM_WORLD);
                    break;
                case BSEND:
                    MPI_Bsend(pos, chunk_size, MPI_INT, i + 1, TAG, MPI_COMM_WORLD);
                    break;
                case RSEND:
                    /* Synchronize */
                default:
                    err("Unknown send method\n");
        }
            pos += chunk_size;
        }

        /* Receive the sums from the other processes and add them up*/
        for(i=1; i<nprocs; i++) {
            MPI_Recv(&sum, 1, MPI_INT, i, TAG, MPI_COMM_WORLD, &stat);
            global_sum += sum;
        }

        /* Check the total sum and output the result */
        if (debug) {
            if (global_sum == array_size)
                printf("Received the correct sum of %d\n", global_sum);
            else
                printf("Received a sum of %d, expected %d\n", global_sum, array_size);
        }

        if (method == BSEND) {
            MPI_Buffer_detach(buf, &buf_size);
            free(buf);
        }

        printf("Time elapsed: %f s\n", MPI_Wtime() - time);

    } else { /*rank > 0 */

        if (method == BSEND) {
            buf_size = array_size * sizeof(int) + MPI_BSEND_OVERHEAD;
            buf = (int*) malloc(buf_size);
            if (!buf)
                err("Failed to allocate memory\n");
            MPI_Buffer_attach(buf, buf_size);
        }

        /* Receive the input. If the method MPI_Rsend is used we use a 
         * non-blocking receive and synchronise with the sender to guarantee,
         * that the receive is posted before the ready send is called */
        if (method == RSEND) {
        } else {
            MPI_Recv(array, array_size, MPI_INT, 0, TAG, MPI_COMM_WORLD, &stat);
            MPI_Get_count(&stat, MPI_INT, &chunk_size);
        }
        if (debug)
            printf("Process %d received %d elements\n", myrank, chunk_size);

        /* Add up the elements */
        for(i=0; i < chunk_size; i++)
            sum += array[i];

        if (debug)
            printf("Process %d computed a sum of %d elements\n", myrank, sum);

        /* Send the result to process 0 */
        switch(method) {
            case SEND:
                MPI_Send(&sum, 1, MPI_INT, 0, TAG, MPI_COMM_WORLD);
                break;
            case SSEND:
                MPI_Ssend(&sum, 1, MPI_INT, 0, TAG, MPI_COMM_WORLD);
                break;
            case BSEND:
                MPI_Bsend(&sum, 1, MPI_INT, 0, TAG, MPI_COMM_WORLD);
                break;
            case RSEND:
                break;
            default:
                err("Unknown method\n");
        }
    
        if (method == BSEND) {
            MPI_Buffer_detach(buf, &buf_size);
            free(buf);
        }
    
    }

    /* Clean up */
    free(array);
    MPI_Finalize();
    return 0;
}

