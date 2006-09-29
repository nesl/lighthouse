int add(int a, int b) {
    return a + b;
}

int main() {
    
    int a = 3;
    int b = 14;
    int sum;
    
    sum = add(&a, b);

    return 0;
}
