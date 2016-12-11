message = "Hello, World!"
sayHello = print message

double x = x * 2

factorial x
    | x == 0 = 1
    | x > 0 = x * factorial(x-1)
