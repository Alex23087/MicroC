extern void prints(char s[]);

void strcat(char out[], char in1[], char in2[]){
    int i;
    for(i = 0; in1[i] != '\0'; i++){
        out[i] = in1[i];
    }
    int j;
    for(j = 0; in2[j] != '\0'; j++){
        out[i+j] = in2[j];
    }
}

void strzero(char s[], int len){
    while(len --> 0){
        s[len] = '\0';
    }
}