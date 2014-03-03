#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>

#define N   5   /* number of philosophers */
#define LEFT    (i+N-1)%N
#define RIGHT   (i+1)%N
#define THINKING    0
#define HUNGRY      1
#define EATING      2


pthread_mutex_t mutex;

int state[N];
sem_t s[N];


void take_forks(long i);
void test(long i);
void put_forks(long i);
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
        sem_init(&s[i], 0, 0);
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


void test(long i)
{
    if (state[i] == HUNGRY && state[LEFT] != EATING && state[RIGHT] != EATING) {
        sem_post(&s[i]);
    }
}

void take_forks(long i)
{
    pthread_mutex_lock(&mutex);
    state[i] = HUNGRY;
    test(i);
    pthread_mutex_unlock(&mutex);
    sem_wait(&s[i]);
    printf("Philosopher #%ld took forks.\n", i);
}


void put_forks(long i)
{
    pthread_mutex_lock(&mutex);
    state[i] = THINKING;
    test(LEFT);
    test(RIGHT);
    printf("Philosopher #%ld put forks.\n", i);
    pthread_mutex_unlock(&mutex);
}


void *philosopher(void *threadid)
{
    long i;
    i = (long)threadid;
    
    while (1) {
        printf("Philosopher #%ld is thinking.\n", i);
        take_forks(i);
        printf("Philosopher #%ld is eating.\n", i);
        put_forks(i);
    }    
    pthread_exit(NULL);
}
