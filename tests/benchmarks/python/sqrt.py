import math

def sqrt(n):
    x = pow(2, 31)
    y = 0
    while True:
        y = math.floor((x + math.floor(n/x)) / 2)
        if y >= x:
            return x

        x = y

def main():
    res = 10485760000
    total = 0
    i = 1
    print("start")

    while i <= 100000:
        i = i+1
        total += sqrt(res)

    print("end")
    print(total)

if __name__ == "__main__":
    main()