#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <semaphore.h>

#define N   5   /* number of philosophers */
#define LEFT    (i+N-1)%N
#define RIGHT   (i+1)%N
#define THINKING    0
#define HUNGRY      1
#define EATING      2


pthread_mutex_t mutex;

int state[N];
sem_t *forks[N];
const char semname[][N] = {"sem0", "sem1", "sem2", "sem3", "sem4"};


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
    
    pthread_mutex_init(&mutex, NULL);
    for (i=0; i < N; i++) {
        forks[i] = sem_open(semname[i], O_CREAT);
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
    
    pthread_mutex_destroy(&mutex);
    
    printf("Main: program completed. Exiting.\n");
    pthread_exit(NULL);
}


void test(int i)
{
    if (state[i] == HUNGRY && state[LEFT] != EATING && state[RIGHT] != EATING) {
        sem_post(forks[i]);
    }
}

void take_forks(int i)
{
    pthread_mutex_lock(&mutex);
    state[i] = HUNGRY;
    test(i);
    pthread_mutex_unlock(&mutex);
    sem_wait(forks[i]);
    printf("Philosopher #%d took forks.\n", i);
}


void put_forks(int i)
{
    pthread_mutex_lock(&mutex);
    state[i] = THINKING;
    test(LEFT);
    test(RIGHT);
    printf("Philosopher #%d put forks.\n", i);
    pthread_mutex_unlock(&mutex);
}

void think(int i)
{
    int c = 0;
    state[i] = THINKING;
    while (c++ < N) {
        printf("Philosopher #%d is thinking.\n", i);
        usleep(100000);
    }
}


void eat(int i)
{
    int c = 0;
    while (c++ < N) {
        printf("Philosopher #%d is eating.\n", i);
        usleep(100000);
    }
}

void *philosopher(void *threadid)
{
    long i;
    i = (int)threadid;
    
    while (1) {
        think(i);
        take_forks(i);
        eat(i);
        put_forks(i);
    }    
    pthread_exit(NULL);
}
