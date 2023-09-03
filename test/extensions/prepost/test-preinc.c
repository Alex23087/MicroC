#include<stdio.h>

void print(int i){
    printf("%d\n", i);
}

void main(){
    int i;
    i = 0;
    ++i;
    print(i);
    int b;
    b = ++i;
    print(i);
    print(b);
    i = ++i;
    print(i);
    b = (++i) + (++i);
    print(i);
    print(b);
}