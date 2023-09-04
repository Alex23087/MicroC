#include <string.mci>

void main(){
    char str1[6];
    char str2[7];
    char str3[13];
    str1 = "Hello";
    str2 = "World!";
    strzero(str3, 14);
    strcat(str3, str1, " ");
    strcat(str3, str3, str2);

    prints(str3);
    prints("\n");
}
