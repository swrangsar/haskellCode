/* linux version; posix semaphores; unnamed semaphores */

/* gcc dinPhiloUnnamed.c -o dinPhiloUnnamed -Wall -pthread -lrt */


#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <semaphore.h>
#include <sys/types.h>


#define N   5   /* number of philosophers */
#define LEFT    (i+N-1)%N
#define RIGHT   (i+1)%N
#define THINKING    0
#define HUNGRY      1
#define EATING      2


int state[N];
sem_t mutex;
sem_t forks[N];


void take_forks(int i);
void test(int i);
void think(int i);
void eat(int i);
void put_forks(int i);
void *philosopher(void *threadid);





int main(int argc, char *argv[])
{
    pthread_t threads[N];
    int rc,i;
    long t;
    void *status;
    
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
    
    for (i=0; i < N; i++) {
        rc = sem_init(&forks[i], 0, 0);
        if (rc) {
            fprintf(stderr, "Error creating semaphore: %s\n", strerror(errno));
            exit(EXIT_FAILURE);
        }
    }
    rc = sem_init(&mutex, 0, 1);
    if (rc) {
        fprintf(stderr, "Error creating semaphore: %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    }

    for(t=0; t<N; t++) {
        printf("In main: creating thread %ld\n", t);
        rc = pthread_create(&threads[t], &attr, philosopher, (void *)t);
        if (rc) {
            printf("ERROR; return code from pthread_create() is %d\n", rc);
            exit(-1);
        }
    }
    pthread_attr_destroy(&attr);
    
    for(t=0; t<N; t++) {
        rc = pthread_join(threads[t], &status);
        if (rc) {
            printf("ERROR; return code from pthread_join() is %d\n", rc);
            exit(-1);
        }
        printf("Main: completed join with thread %ld having a status of %ld\n",t,(long)status);
    }
    
    for (i=0; i < N; i++) {
        sem_destroy(&forks[i]);
    }
    sem_destroy(&mutex);
    
    printf("Main: program completed. Exiting.\n");
    pthread_exit(NULL);
}


void test(int i)
{
    if (state[i] == HUNGRY && state[LEFT] != EATING && state[RIGHT] != EATING) {
        state[i] = EATING;
        sleep(2);
        printf("Philosopher %d takes fork %d and %d\n", i+1, LEFT+1, i+1);
        printf("Philosopher %d is Eating\n", i+1);
        sem_post(&forks[i]);
    }
}

void take_forks(int i)
{
    sem_wait(&mutex);
    state[i] = HUNGRY;
    printf("Philosopher %d is Hungry\n", i+1);
    test(i);
    sem_post(&mutex);
    sem_wait(&forks[i]);
    sleep(1);
}


void put_forks(int i)
{
    sem_wait(&mutex);
    state[i] = THINKING;
    printf("Philosopher %d is putting fork %d and %d down\n", i+1, LEFT+1, i+1);
    printf("Philosopher %d is Thinking\n", i+1);
    test(LEFT);
    test(RIGHT);
    sem_post(&mutex);
}


void *philosopher(void *threadid)
{
    int i;
    i = (long)threadid;
    
    while (1) {
        sleep(1);
        take_forks(i);
        sleep(0);
        put_forks(i);
    }    
    pthread_exit(NULL);
}
