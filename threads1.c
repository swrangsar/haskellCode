#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define NUM_THREADS 5

pthread_mutex_t philoMutex;


void *printHello(void *threadid);


int main(int argc, char *argv[])
{
    pthread_t threads[NUM_THREADS];
    int rc;
    long t;
    void *status;
    
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
    
    pthread_mutex_init(&philoMutex, NULL);

    for(t=0; t<NUM_THREADS; t++) {
        printf("In main: creating thread %ld\n", t);
        rc = pthread_create(&threads[t], &attr, printHello, (void *)t);
        if (rc) {
            printf("ERROR; return code from pthread_create() is %d\n", rc);
            exit(-1);
        }
    }
    pthread_attr_destroy(&attr);
    
    for(t=0; t<NUM_THREADS; t++) {
        rc = pthread_join(threads[t], &status);
        if (rc) {
            printf("ERROR; return code from pthread_join() is %d\n", rc);
            exit(-1);
        }
        printf("Main: completed join with thread %ld having a status of %ld\n",t,(long)status);
    }
    
    pthread_mutex_destroy(&philoMutex);
    
    printf("Main: program completed. Exiting.\n");
    pthread_exit(NULL);
}




void *printHello(void *threadid)
{
    long tid;
    int count = 0;
    tid = (long)threadid;
    
    while (count++ < 2) {
        pthread_mutex_lock(&philoMutex);
        printf("Philosopher #%ld picked left fork!\n", tid);
        printf("Philosopher #%ld picked right fork!\n", tid);
        printf("Philosopher #%ld started eating!\n", tid);
        usleep(500000);
        printf("Philosopher #%ld is done eating!\n", tid);
        pthread_mutex_unlock(&philoMutex);
    }   

    pthread_exit(NULL);
}
