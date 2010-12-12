#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <time.h>

#define NUM_PHIL 5 
#define MAX_THINK 5
#define MAX_EAT 5
#define TIME_TO_RUN 20

static pthread_t phil[NUM_PHIL];
static pthread_mutex_t forks[NUM_PHIL];

short timeout = 1;

void eat(int tid, int *time_eaten) {
    int left = tid, right = (tid + 1) % NUM_PHIL;
    int r = 0;
    
    /* Pick up both fork. This may result in a deadlock if all philosphers 
       decide to eat at the same time */
    printf("Philosopher %d is picking up fork %d and %d\n", tid, left, right);
    pthread_mutex_lock(&forks[left]);
    pthread_mutex_lock(&forks[right]);
   
    /* Eat between 1 and MAX_EAT seconds */ 
    r = rand() % MAX_EAT + 1;
    printf("Philosopher %d is eating for %d s\n", tid, r);
    sleep(r);
    printf("Philosopher %d has finished eating\n", tid);
    *time_eaten += r;
    
    /* Put down forks*/
    pthread_mutex_unlock(&forks[left]);
    pthread_mutex_unlock(&forks[right]);

    return;
}

void think(int tid) {
    int r = 0;

    /* Think between 1 and MAX_THINK seconds */
    r = rand() % MAX_THINK + 1;
    printf("Philosopher %d is thinking for %d s\n", tid, r);
    sleep(r);

    return;
}

void *philosophise(void *arg) {
    int *tid = (int *) arg;
    int time_eaten = 0;

    /* timeout may only be written to by the parent process. We
       therefore don't need to use synchronize access to this variable.
       A lost update only results in one additional eat/think cycle */
    while (timeout) {
        think(*tid);
        eat(*tid, &time_eaten);
    }

    printf("Philosopher %d has eaten %d seconds during this program run\n", *tid, time_eaten);
    pthread_exit(NULL);
}

int main() {
    int i, tids[NUM_PHIL];

    /* Initialise, make sure all forks are available before the philosophers 
       start eating */
    for (i = 0; i < NUM_PHIL; i++) {
        if(pthread_mutex_init(&forks[i], NULL))
            exit(-1);
    }

    sleep(1);

    for (i = 0; i < NUM_PHIL; i++) {
        tids[i] = i;
        if(pthread_create(&phil[i], NULL, philosophise, (void *) &tids[i]))
            exit(-2);
    }

    /* Let the threads run */
    sleep(TIME_TO_RUN);

    /* Signal program end to the threads */
    timeout = 0;

    /* Clean up */
    for (i = 0; i < NUM_PHIL; i++) {
        pthread_join(phil[i], NULL);
    }
    for (i = 0; i < NUM_PHIL; i++) {
        pthread_mutex_destroy(&forks[i]);
    }

    return 0;
}
