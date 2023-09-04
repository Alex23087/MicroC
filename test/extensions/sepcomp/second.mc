int pow(int base, int exp){
    int out;
    out = base;
    while(exp > 1){
        out *= base;
        exp--;
    }
    return out;
}
