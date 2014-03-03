/*
cat semtest.c

source:
"Program using Semaphores runs fine on Linux...unexpected results on Mac osX",
http://stackoverflow.com/questions/4136181/program-using-semaphores-runs-fine-on-linux-unexpected-results-on-mac-osx

compiled on Mac OS X 10.6.8 with:
gcc -ansi -pedantic -std=gnu99 -Os -Wall -Wextra -Wshadow -l pthread -o semtest semtest.c

./semtest

*/


#include <semaphore.h>
#include <sys/types.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <stdlib.h>

void* function1();
void* function2();

// shared values
volatile int X;
volatile int Y;

// declare semaphores
//sem_t s1;
//sem_t s2;
sem_t *s1;
sem_t *s2;
static const char *semname1 = "Semaphore1";
static const char *semname2 = "Semaphore2";

int main(void)
{

   void* status;

   pthread_t thread1;
   pthread_t thread2;
   srand(time(NULL));

   /*
   // initialize semaphores to zero
   sem_init(&s1, 0, 0);
   sem_init(&s2, 0, 0);
   */

   s1 = sem_open(semname1, O_CREAT, 0777, 0);
   if (s1 == SEM_FAILED)
   {
      fprintf(stderr, "%s\n", "ERROR creating semaphore semname1");
      exit(EXIT_FAILURE);
   }

   s2 = sem_open(semname2, O_CREAT, 0777, 0);
   if (s2 == SEM_FAILED)
   {
      fprintf(stderr, "%s\n", "ERROR creating semaphore semname2");
      exit(EXIT_FAILURE);
   }


   pthread_create(&thread1, NULL, function1, NULL);
   pthread_create(&thread2, NULL, function2, NULL);

   pthread_join(thread1, &status);
   pthread_join(thread2, &status);

   //sem_destroy(&s1);
   //sem_destroy(&s2);
   sem_unlink(semname1);
   sem_unlink(semname2);

   return 0;

}

void* function1()
{
   while(1)
   {
   X = rand()%1000; // write 
   printf("After thread ID A writes to X, X = %d\n", X);
   //sem_post(&s1); // signal
   //sem_wait(&s2); // wait
   sem_post(s1); // signal
   sem_wait(s2); // wait
   printf("After thread ID A reads from Y, Y = %d\n", Y); // read
   sleep(3);
   }   
}

void* function2()
{
   while(1)
   {
    //sem_wait(&s1); // wait
    sem_wait(s1); // wait
    printf("After thread ID B reads from X, X = %d\n", X); // read
    Y = rand()%1000; // write
    printf("After thread ID B write to Y, Y = %d\n", Y);
    //sem_post(&s2); // signal
    sem_post(s2); // signal
    sleep(3);
   }
}
