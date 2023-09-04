#include<float.mci>

// Compute an approximation of pi using Leibniz's formula
float pi(int iter){
    float acc;
    acc = 0f;
    int sign;
    sign = 1;
    int i;
    i = 1;
    while(i < iter * 2){
        acc += (1/itof(i)) * sign;
        sign *= -1;
        i += 2;
    }
    return acc * 4;
}

void main(){
    float f;
    f = pi(999999);
    printfloat(f);
}