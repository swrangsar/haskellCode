#include <stdio.h>
#include <stdlib.h>

#define NUM 1024


int main()
{
    double num  = NUM;
    unsigned char *s     = (unsigned char *)&num;
    int len     = sizeof(num)/sizeof(unsigned char);
    printf("length: %d\n", len);
    char *newnum    = malloc(sizeof(char)*len);
    unsigned char byte, currSeven, currLast;
    
    int i = 0;
    while (i < len) {
        byte = *(s+i);
        currSeven = byte >> 1;
        if (i>0) {
            newnum[i] = currLast | currSeven;
        } else {
            newnum[i] = currSeven;
        }
        
        
        currLast  = byte << 7;
        i++;
    }
    newnum[0]   = currLast | newnum[0];
    printf("The new double is: %f\n", *((double *)newnum) );
    i = 0;
    while (i < len) {
        printf("%u ", (unsigned int)newnum[i]);
        i++;
    }
    printf("\n");
    i= 0;
    while (i<len) {
        printf("%u ", (unsigned int)s[i]);
        i++;
    }
    printf("\n");
    
    if (newnum) free(newnum);
    return 0;
}