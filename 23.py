lim = 28123 + 1
abundant_sums = [0] * lim
for i in range(1, lim):
    j = i * 2
    while j < lim:
        abundant_sums[j] += i
        j += i

abundant = [i for i in range(1, lim) if abundant_sums[i] > i]


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


result_set = BitSet(lim)

print('abundants ' + str(len(abundant)))
for i in abundant:
    for j in abundant:
        if i + j < lim:
            result_set.set(i + j)

res = 0
for i in range(1, lim):
    if result_set.get(i) == 0:
        res += i
print(res)
