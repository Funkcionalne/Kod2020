from functools import reduce

print(reduce((lambda x, y: x + y), [1, 2, 3, 4]))

# print(reduce((lambda x, y: x - y), [1, 2, 3, 4]))
print(reduce((lambda x, y: 10*x + y), [1, 2, 3, 4]))
# reduce(f, [x1,x2, ..., xn]) = f(...f(f(x1,x2),x3), ... xn)
 
print(reduce((lambda x, y: x * y), list(range(1,101))))

print(reduce((lambda x, y: x / y), [64,4,2,8]))

