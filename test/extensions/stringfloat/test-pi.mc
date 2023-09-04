#include<float.mci>

// Compute an approximation of pi using Leibniz's formula
float pi(int iter){
    float acc;
    acc = 0f;
    int sign;
    sign = 1;
    int i;
    for(i = 1; i < iter * 2; i += 2){
        acc += (1/itof(i)) * sign;
        sign *= -1;
    }
    return acc * 4;
}

void main(){
    float f;
    f = pi(999999);
    printfloat(f);
}