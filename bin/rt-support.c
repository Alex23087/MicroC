#include <stdio.h>
#include <stdlib.h>

// TO BE COMPLETED
int getint(){
    char buffer[32];
    if(fgets(buffer, 32, stdin) == NULL)
      return 0;
    return atoi(buffer);
}

void print(int n){
  printf("%d\n", n);
}

void printfloat(float f){
  printf("%f\n", f);
}

void prints(char* str){
  printf("%s", str);
}

int ftoi(float f){
  return (int) f;
}

float itof(int i){
  return (float) i;
}