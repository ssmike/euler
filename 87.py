from math import ceil, pow


class BitSet:
    cell_len = 32

    def __init__(self, n):
        self.storage = [0] * (n//self.cell_len + 1)

    def set(self, n):
        cell_idx, bit_idx = n // self.cell_len, n % self.cell_len
        self.storage[cell_idx] |= 1 << bit_idx

    def get(self, n):
        cell_idx, bit_idx = n // self.cell_len, n % self.cell_len
        return (self.storage[cell_idx] >> bit_idx) & 1


def primes_list(lim):
    res = []
    ls = BitSet(lim)
    for i in range(2, lim):
        if ls.get(i) == 0:
            res.append(i)

        j = i * i
        while j < lim:
            ls.set(j)
            j += i
    return res

lim = 50 * 10**6

fltr = BitSet(lim)

primes = primes_list(lim)
lim4 = ceil(pow(lim, 1/4) + 2)
lim3 = ceil(pow(lim, 1/3) + 2)
lim2 = ceil(pow(lim, 1/2) + 2)

assert lim4 ** 4 > lim
assert lim3 ** 3 > lim
assert lim2 ** 2 > lim

res = 0


def flt(lim):
    return filter(lambda x: x < lim, primes)


for i in flt(lim4):
    for j in flt(lim3):
        tostop = False
        for k in flt(lim2):
            val = i**4+j**3+k**2
            if val >= lim:
                if k == 1:
                    tostop = True
                break
            res += 1 - fltr.get(val)
            fltr.set(val)

        if tostop:
            break

print(res)
