#include <float.mci>

void main(){
    int n;
    int m;
    m = n = 7;

    float f;
    float f2;
    f2 = f = .9;

    print(n+m);
    printfloat(n+f);
    printfloat(m+f);
    printfloat(f+f2);

    printfloat(ftoi(f) + f2);
    printfloat(itof(ftoi(f)) + f2);
    print(ftoi(f + f2));
}