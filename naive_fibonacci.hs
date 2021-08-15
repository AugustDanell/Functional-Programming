naive_fibonacci:: Integer -> Integer
naive_fibonacci 0 = 0
naive_fibonacci 1 = 1
naive_fibonacci 2 = 1
naive_fibonacci x = naive_fibonacci(x-1) + naive_fibonacci(x-2)

main = do
print(naive_fibonacci 5)
print(naive_fibonacci 8)
