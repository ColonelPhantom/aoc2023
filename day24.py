#!/usr/bin/python3

from sys import stdin
import numpy as np
import itertools
import sympy

def readColl(s):
    [pos, vel] = s.strip().split("@")
    (pos, vel) = eval("((" + pos + "), (" + vel + "))")
    # print((vel, pos))
    return ((vel, pos))

def collision( point1, point2 ):
    ((a1,b1,c1), (x1,y1,z1)) = point1
    ((a2,b2,c2), (x2,y2,z2)) = point2
    return np.linalg.solve(
        [[a1, a2], [b1, b2], [c1, c2]],
        [x2 - x1, y2 - y1, z2 - z1]
    )

def part1(point1, point2):
    ((a1,b1,c1), (x1,y1,z1)) = point1
    ((a2,b2,c2), (x2,y2,z2)) = point2
    try:
        print([a1,a2], [b1,b2])
        print(x2-x1, y2-y1)
        [t,u] = np.linalg.solve(
            [[a1, a2], [b1, b2]],
            [x2-x1, y2 - y1]
        )
        u = -u
        x = a1*t + x1
        y = b1*t + y1
        # print(t,u, "", (x,y), (a2*u+x2, b2*u+y2))
        if (x < 200000000000000 or x > 400000000000000 or
            y < 200000000000000 or y > 400000000000000):
            # print("Out of bounds")
            return False
        elif t < 0 or u < 0:
            # print("Crossing in the past")
            return False
        else:
            return True

    except np.linalg.LinAlgError as e:
        print(e)
        return False


hs = list(map(readColl, stdin))
counter = 0
for (a,b) in itertools.combinations(hs, 2):
    # print((a,b))
    if part1(a,b):
        counter += 1
    # print()
print("part 1:", counter)

# solve for xt, yt, zt, at, bt, ct, t1..t300
# matrix = []
# vector = []
# size = len(hs)
# for (i,h) in enumerate(hs):
#     ((a1,b1,c1), (x1,y1,z1)) = h
#     row = np.repeat(0, size+6)
#     row[0] = 1
#     row[
xt, yt, zt, at, bt, ct = sympy.symbols("xt yt zt at bt ct")
equations = []
for (i,h) in enumerate(hs[:3]):
    ((a1,b1,c1), (x1,y1,z1)) = h
    ti = sympy.symbols(f"t{i}")
    equations.append(sympy.Eq(x1 + a1*ti, xt+at*ti))
    equations.append(sympy.Eq(y1 + b1*ti, yt+bt*ti))
    equations.append(sympy.Eq(z1 + c1*ti, zt+ct*ti))

print(len(equations))

sol = sympy.solve(equations)[0]
print(sol)
print(sol[xt], sol[yt], sol[zt])
print("part 2:", sol[xt]+sol[yt]+sol[zt])
# print(sympy.nsolve(equations, (xt,yt,zt,at,bt,ct), (0,0,0,0,0,0)))