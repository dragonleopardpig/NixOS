import numpy as np

a = np.arange(1, 10)
print(a)

x = range(1, 10)
print(x)    # x is an iterator
print(list(x))

# further arange examples:
x = np.arange(10.4)
print(x)
x = np.arange(0.5, 10.4, 0.8)
print(x)

np.arange(12.04, 12.84, 0.08)

x = np.arange(0.5, 10.4, 0.8, int)
print(x)

import numpy as np

# 50 values between 1 and 10:
print(np.linspace(1, 10))
# 7 values between 1 and 10:
print(np.linspace(1, 10, 7))
# excluding the endpoint:
print(np.linspace(1, 10, 7, endpoint=False))

import numpy as np

samples, spacing = np.linspace(1, 10, retstep=True)
print(spacing)
samples, spacing = np.linspace(1, 10, 20, endpoint=True, retstep=True)
print(spacing)
samples, spacing = np.linspace(1, 10, 20, endpoint=False, retstep=True)
print(spacing)

import numpy as np
x = np.array(42)
print("x: ", x)
print("The type of x: ", type(x))
print("The dimension of x:", np.ndim(x))

F = np.array([1, 1, 2, 3, 5, 8, 13, 21])
V = np.array([3.4, 6.9, 99.8, 12.8])
print("F: ", F)
print("V: ", V)
print("Type of F: ", F.dtype)
print("Type of V: ", V.dtype)
print("Dimension of F: ", np.ndim(F))
print("Dimension of V: ", np.ndim(V))

A = np.array([ [3.4, 8.7, 9.9], 
               [1.1, -7.8, -0.7],
               [4.1, 12.3, 4.8] ])
print(A)
print(A.ndim)

B = np.array([ [[111, 112], [121, 122]],
               [[211, 212], [221, 222]],
               [[311, 312], [321, 322]] ])
print(B)
print(B.ndim)

x = np.array([ [67, 63, 87],
               [77, 69, 59],
               [85, 87, 99],
               [79, 72, 71],
               [63, 89, 93],
               [68, 92, 78] ])

print(np.shape(x))

print(x.shape)

x.shape = (3, 6)
print(x)

x.shape = (2, 9)
print(x)

x = np.array(11)
print(np.shape(x))

B = np.array([ [[111, 112, 113], [121, 122, 123]],
               [[211, 212, 213], [221, 222, 223]],
               [[311, 312, 313], [321, 322, 323]],
               [[411, 412, 413], [421, 422, 423]] ])

print(B.shape)

F = np.array([1, 1, 2, 3, 5, 8, 13, 21])
# print the first element of F
print(F[0])
# print the last element of F
print(F[-1])

A = np.array([ [3.4, 8.7, 9.9], 
               [1.1, -7.8, -0.7],
               [4.1, 12.3, 4.8]])

print(A[1][0])

tmp = A[1]
print(tmp)
print(tmp[0])

print(A[1, 0])

S = np.array([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
print(S[2:5])
print(S[:4])
print(S[6:])
print(S[:])

A = np.array([
[11, 12, 13, 14, 15],
[21, 22, 23, 24, 25],
[31, 32, 33, 34, 35],
[41, 42, 43, 44, 45],
[51, 52, 53, 54, 55]])

print(A[:3, 2:])

print(A[3:, :])

print(A[:, 4:])

X = np.arange(28).reshape(4, 7)
print(X)

print(X[::2, ::3])

print(X[::, ::3])

A = np.array(
    [ [ [45, 12, 4], [45, 13, 5], [46, 12, 6] ], 
      [ [46, 14, 4], [45, 14, 5], [46, 11, 5] ], 
      [ [47, 13, 2], [48, 15, 5], [52, 15, 1] ] ])

A[1:3, 0:2]  # equivalent to A[1:3, 0:2, :]

A = np.array([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
S = A[2:6]
S[0] = 22
S[1] = 23
print(A)

A = np.arange(12)
B = A.reshape(3, 4)
A[0] = 42
print(B)

np.may_share_memory(A, B)

import numpy as np

E = np.ones((2,3))
print(E)

F = np.ones((3,4),dtype=int)
print(F)

Z = np.zeros((2,4))
print(Z)

x = np.array([2,5,18,14,4])
E = np.ones_like(x)
print(E)

Z = np.zeros_like(x)
print(Z)

np.empty((2, 4))

import numpy as np

x = np.array([[42,22,12],[44,53,66]], order='F')
y = x.copy()

x[0,0] = 1001
print(x)

print(y)

print(x.flags['C_CONTIGUOUS'])
print(y.flags['C_CONTIGUOUS'])

import numpy as np

x = np.array([[42,22,12],[44,53,66]], order='F')
y = x.copy()
x[0,0] = 1001
print(x)

print(y)

print(x.flags['C_CONTIGUOUS'])
print(y.flags['C_CONTIGUOUS'])

import numpy as np

np.identity(4)

import numpy as np

np.eye(5, 8, k=1, dtype=int)

import numpy as np

i16 = np.dtype(np.int16)
print(i16)

lst = [ [3.4, 8.7, 9.9], 
        [1.1, -7.8, -0.7],
        [4.1, 12.3, 4.8] ]

A = np.array(lst, dtype=i16)

print(A)

import numpy as np

dt = np.dtype([('density', np.int32)])

x = np.array([(393,), (337,), (256,)],
             dtype=dt)

print(x)

print("\nThe internal representation:")
print(repr(x))

print(x['density'])

dt = np.dtype([('density', 'i4')])
x = np.array([(393,), (337,), (256,)],
             dtype=dt)
print(x)

# little-endian ordering
dt = np.dtype('<d')
print(dt.name, dt.byteorder, dt.itemsize)

# big-endian ordering
dt = np.dtype('>d')  
print(dt.name, dt.byteorder, dt.itemsize)

# native byte ordering
dt = np.dtype('d') 
print(dt.name, dt.byteorder, dt.itemsize)

dt = np.dtype([('country', 'S20'), ('density', 'i4'), ('area', 'i4'), ('population', 'i4')])
population_table = np.array([
    ('Netherlands', 393, 41526, 16928800),
    ('Belgium', 337, 30510, 11007020),
    ('United Kingdom', 256, 243610, 62262000),
    ('Germany', 233, 357021, 81799600),
    ('Liechtenstein', 205, 160, 32842),
    ('Italy', 192, 301230, 59715625),
    ('Switzerland', 177, 41290, 7301994),
    ('Luxembourg', 173, 2586, 512000),
    ('France', 111, 547030, 63601002),
    ('Austria', 97, 83858, 8169929),
    ('Greece', 81, 131940, 11606813),
    ('Ireland', 65, 70280, 4581269),
    ('Sweden', 20, 449964, 9515744),
    ('Finland', 16, 338424, 5410233),
    ('Norway', 13, 385252, 5033675)],
    dtype=dt)
print(population_table[:4])

print(population_table['density'])
print(population_table['country'])
print(population_table['area'][2:5])

dt = np.dtype([('country', np.compat.unicode, 20), 
               ('density', 'i4'), 
               ('area', 'i4'), 
               ('population', 'i4')])
population_table = np.array([
    ('Netherlands', 393, 41526, 16928800),
    ('Belgium', 337, 30510, 11007020),
    ('United Kingdom', 256, 243610, 62262000),
    ('Germany', 233, 357021, 81799600),
    ('Liechtenstein', 205, 160, 32842),
    ('Italy', 192, 301230, 59715625),
    ('Switzerland', 177, 41290, 7301994),
    ('Luxembourg', 173, 2586, 512000),
    ('France', 111, 547030, 63601002),
    ('Austria', 97, 83858, 8169929),
    ('Greece', 81, 131940, 11606813),
    ('Ireland', 65, 70280, 4581269),
    ('Sweden', 20, 449964, 9515744),
    ('Finland', 16, 338424, 5410233),
    ('Norway', 13, 385252, 5033675)],
    dtype=dt)
print(population_table[:4])

np.savetxt("population_table.csv",
           population_table,
           fmt="%s;%d;%d;%d",           
           delimiter=";")

dt = np.dtype([('country', np.compat.unicode, 20), ('density', 'i4'), ('area', 'i4'), ('population', 'i4')])

x = np.genfromtxt("population_table.csv",
               dtype=dt,
               delimiter=";")

dt = np.dtype([('country', np.compat.unicode, 20), ('density', 'i4'), ('area', 'i4'), ('population', 'i4')])

x = np.loadtxt("population_table.csv",
               dtype=dt,
               converters={0: lambda x: x.decode('utf-8')},
               delimiter=";")

print(x)

import numpy as np
lst = [2,3, 7.9, 3.3, 6.9, 0.11, 10.3, 12.9]
v = np.array(lst)
v = v + 2
print(v)

print(v * 2.2)

print(v - 1.38)

print(v ** 2)
print(v ** 1.5)

lst = [2,3, 7.9, 3.3, 6.9, 0.11, 10.3, 12.9]
res = []
for val in lst:
    res.append(val + 2)

print(res)

res = [ val + 2 for val in lst]
print(res)

v = np.random.randint(0, 100, 1000)

%timeit v + 1

lst = list(v)

%timeit [ val + 2 for val in lst]

import numpy as np

A = np.array([ [11, 12, 13], [21, 22, 23], [31, 32, 33] ])
B = np.ones((3,3))

print("Adding to arrays: ")
print(A + B)

print("\nMultiplying two arrays: ")
print(A * (B + 1))

np.dot(A, B)

print(np.dot(3, 4))
x = np.array([3])
y = np.array([4])
print(x.ndim)
print(np.dot(x, y))

x = np.array([3, -2])
y = np.array([-4, 1])
print(np.dot(x, y))

A = np.array([ [1, 2, 3], 
               [3, 2, 1] ])
B = np.array([ [2, 3, 4, -2], 
               [1, -1, 2, 3],
               [1, 2, 3, 0] ])

# es muss gelten:
print(A.shape[-1] == B.shape[-2], A.shape[1]) 
print(np.dot(A, B))

import numpy as np
X = np.array( [[[3, 1, 2],
                [4, 2, 2],
                [2, 4, 1]],

               [[3, 2, 2],
                [4, 4, 3],
                [4, 1, 1]],

               [[2, 2, 1],
                [3, 1, 3],
                [3, 2, 3]]])

Y = np.array( [[[2, 3, 1],
                [2, 2, 4],
                [3, 4, 4]],
            
               [[1, 4, 1],
                [4, 1, 2],
                [4, 1, 2]],
            
               [[1, 2, 3],
                [4, 1, 1],
                [3, 1, 4]]])


R = np.dot(X, Y)

print("The shapes:")
print(X.shape)
print(Y.shape)
print(R.shape)

print("\nThe Result R:")
print(R)

import numpy as np
X = np.array(
    [[[3, 1, 2],
      [4, 2, 2]],

     [[-1, 0, 1],
      [1, -1, -2]],
     
     [[3, 2, 2],
      [4, 4, 3]],

     [[2, 2, 1],
      [3, 1, 3]]])

Y = np.array(
    [[[2, 3, 1, 2, 1],
      [2, 2, 2, 0, 0],
      [3, 4, 0, 1, -1]],

     [[1, 4, 3, 2, 2],
      [4, 1, 1, 4, -3],
      [4, 1, 0, 3, 0]]])


R = np.dot(X, Y)



print("X.shape: ", X.shape, "   X.ndim: ", X.ndim)
print("Y.shape: ", Y.shape, "   Y.ndim: ", Y.ndim)
print("R.shape: ",     R.shape, "R.ndim: ", R.ndim)


print("\nThe result array R:\n")
print(R)

i = 0
for j in range(X.shape[1]):
    for k in range(Y.shape[0]):
        for m in range(Y.shape[2]):
            fmt = "    sum(X[{}, {}, :] * Y[{}, :, {}] :  {}"
            arguments = (i, j, k, m, sum(X[i, j, :] * Y[k, :, m]))
            print(fmt.format(*arguments))

print(R[0])

R2 = np.zeros(R.shape, dtype=np.int8)

for i in range(X.shape[0]):
    for j in range(X.shape[1]):
        for k in range(Y.shape[0]):
            for m in range(Y.shape[2]):
                R2[i, j, k, m] = sum(X[i, j, :] * Y[k, :, m])


print( np.array_equal(R, R2) )

import numpy as np

A = np.array([ [1, 2, 3], [2, 2, 2], [3, 3, 3] ])
B = np.array([ [3, 2, 1], [1, 2, 3], [-1, -2, -3] ])

R = A * B
print(R)

MA = np.mat(A)
MB = np.mat(B)

R = MA * MB
print(R)

import numpy as np

A = np.array([ [11, 12, 13], [21, 22, 23], [31, 32, 33] ])
B = np.array([ [11, 102, 13], [201, 22, 203], [31, 32, 303] ])

A == B

print(np.array_equal(A, B))
print(np.array_equal(A, A))

a = np.array([ [True, True], [False, False]])
b = np.array([ [True, False], [True, False]])

print(np.logical_or(a, b))
print(np.logical_and(a, b))

import numpy as np

A = np.array([ [11, 12, 13], [21, 22, 23], [31, 32, 33] ])
B = np.array([1, 2, 3])

print("Multiplication with broadcasting: ")
print(A * B)
print("... and now addition with broadcasting: ")
print(A + B)

B = np.array([[1, 2, 3],] * 3)
print(B)

B = np.array([1, 2, 3])
B[:, np.newaxis]

A * B[:, np.newaxis]

np.array([[1, 2, 3],] * 3).transpose()

A = np.array([10, 20, 30])
B = np.array([1, 2, 3])
A[:, np.newaxis]

A[:, np.newaxis] * B

import numpy as np

A = np.array([ [11, 12, 13], [21, 22, 23], [31, 32, 33] ])

B = np.array([1, 2, 3])

B = B[np.newaxis, :]
B = np.concatenate((B, B, B))

print("Multiplication: ")
print(A * B)
print("... and now addition again: ")
print(A + B)

import numpy as np

A = np.array([ [11, 12, 13], [21, 22, 23], [31, 32, 33] ])

B = np.tile(np.array([1, 2, 3]), (3, 1))

print(B)

print("Multiplication: ")
print(A * B)
print("... and now addition again: ")
print(A + B)

cities = ["Barcelona", "Berlin", "Brussels", "Bucharest",
          "Budapest", "Copenhagen", "Dublin", "Hamburg", "Istanbul",
          "Kiev", "London", "Madrid", "Milan", "Moscow", "Munich",
          "Paris", "Prague", "Rome", "Saint Petersburg", 
          "Stockholm", "Vienna", "Warsaw"]

dist2barcelona = [0,  1498, 1063, 1968, 
                  1498, 1758, 1469, 1472, 2230, 
                  2391, 1138, 505, 725, 3007, 1055, 
                  833, 1354, 857, 2813, 
                  2277, 1347, 1862]

dists =  np.array(dist2barcelona[:12])
print(dists)
print(np.abs(dists - dists[:, np.newaxis]))

A = np.array([ [[3, 4, 7], [5, 0, -1] , [2, 1, 5]],
      [[1, 0, -1], [8, 2, 4], [5, 2, 1]],
      [[2, 1, 3], [1, 9, 4], [5, -2, 4]]])

B = np.array([ [[3, 4, 7], [1, 0, -1], [1, 2, 3]] ])

B * A

B = np.array([1, 2, 3])

B = B[np.newaxis, :]
print(B.shape)
B = np.concatenate((B, B, B)).transpose()
print(B.shape)
B = B[:, np.newaxis]
print(B.shape)
print(B)

print(A * B)

import numpy as np

A = np.array([[[ 0,  1],
               [ 2,  3],
               [ 4,  5],
               [ 6,  7]],
              [[ 8,  9],
               [10, 11],
               [12, 13],
               [14, 15]],
              [[16, 17],
               [18, 19],
               [20, 21],
               [22, 23]]])

Flattened_X = A.flatten()
print(Flattened_X)

print(A.flatten(order="C"))
print(A.flatten(order="F"))
print(A.flatten(order="A"))

print(A.ravel())

print(A.ravel(order="A"))

print(A.ravel(order="F"))

print(A.ravel(order="A"))

print(A.ravel(order="K"))

X = np.array(range(24))
Y = X.reshape((3,4,2))
Y

x = np.array([11,22])
y = np.array([18,7,6])
z = np.array([1,3,5])
c = np.concatenate((x,y,z))
print(c)

x = np.array(range(24))
x = x.reshape((3,4,2))
y = np.array(range(100,124))
y = y.reshape((3,4,2))
z = np.concatenate((x,y))
print(z)

z = np.concatenate((x,y),axis = 1)
print(z)

x = np.array([2,5,18,14,4])
y = x[:, np.newaxis]
print(y)

A = np.array([3, 4, 5])
B = np.array([1, 9, 0])

print(np.row_stack((A, B)))

print(np.column_stack((A, B)))
np.shape(A)

A = np.array([[3, 4, 5],
              [1, 9, 0],
              [4, 6, 8]])
np.column_stack((A, A, A))

np.column_stack((A[0], A[0], A[0]))

np.dstack((A, A, A))

import numpy as np
x = np.array([ [1, 2], [3, 4]])
np.tile(x, (3,4))

import numpy as np

x = np.array([ 3.4])

y = np.tile(x, (5,)) 

print(y)

import numpy as np
x = np.array([[1, 2], [3, 4]])
print(np.tile(x, 2))

import numpy as np

x = np.array([[1, 2], [3, 4]])
print(np.tile(x, (2, 1)))

import numpy as np
import matplotlib.pyplot as plt

x = np.array([[1, 2], [3, 4]])

print(np.tile(x, (2, 2)))


