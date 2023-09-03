#include<stdio.h>

void print(int i){
    printf("%d\n", i);
}

void main(){
    int i;
    i = 0;
    print(i);
    i += 4;
    print(i);
    i += i += 5;
    print(i);
}