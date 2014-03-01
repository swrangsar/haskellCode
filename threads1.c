#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define NUM_THREADS 5

void *printHello(void *threadid);


int main(int argc, char *argv[])
{
    pthread_t threads[NUM_THREADS];
    int rc;
    long t;

    for(t=0; t<NUM_THREADS; t++) {
        printf("In main: creating thread %ld\n", t);
        rc = pthread_create(&threads[t], NULL, printHello, (void *)t);
        if (rc) {
            printf("ERROR; return code from pthread_create() is %d\n", rc);
            exit(-1);
        }
    }

    pthread_exit(NULL);
}




void *printHello(void *threadid)
{
    long tid;
    tid = (long)threadid;
    while (1) {
        printf("Hello! It's me, thread #%ld!\n", tid);
        printf("LOL afaik #%ld!\n", tid);
        sleep(1);
    }

    pthread_exit(NULL);
}
