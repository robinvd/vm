def fib(n):
    if n <= 2:
        return n

    return fib(n-2) + fib(n-1)

def main():
    res = 20
    total = 0
    i = 1
    print("start")

    while i <= 1000:
        i = i+1
        total += fib(res)

    print("end")
    print(total)

if __name__ == "__main__":
    main()