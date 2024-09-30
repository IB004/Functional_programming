from math import sqrt

def largest_prime_factor(x):
    if is_prime(x):
        return x
    divisor = 2
    while divisor <= sqrt(x):
        if x == divisor:
            return divisor
        if x % divisor == 0:
            return largest_prime_factor(x // divisor)
        divisor = get_next_prime(divisor)

def get_next_prime(x):
    x += 1
    while not is_prime(x):
        x += 1
    return x

def is_prime(x):
    if x < 2:
        return False
    divisor = 2
    while divisor <= sqrt(x):
        if x % divisor == 0:
            return False
        divisor += 1
    return True

x = 600851475143
print(largest_prime_factor(x)) #6857
