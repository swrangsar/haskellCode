#include <stdio.h>



int main()
{
    char chars[] = {23, 45, 67};
    int i = 0;
    while (i++ < 3) {
        char second = ((i*8) % 6);
        if (second ==0) second = 6;
        char first = ((i-1)*8)% 6;
        char ones = 255;
        char j = chars[i-1] >> second;
   
        if (i < 2) {
            printf("\nchar is: %c", j);
            printf("\nnum is: %d", j);
            
        } else {
            char k;
            k = chars[i-2] << (8-first);
            k = k >> (8-first);
            k = k << (8-second);
            printf("\nchar is: %c", j|k);
            printf("\nnum is: %d", j|k);            
        }
    }
    printf("\n");
    return 0;
}

