/* return the nth fibonacci number */

unsigned long int fibonacci(long int n) {
long int a = 0;
long int b = 1;
long int counter = 1;

while (counter < n){
long int tmp = a;
a = b;
b += tmp;
counter++;
}

return b;
}
