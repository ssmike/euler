s = open('/home/ssmike/Downloads/p022_names.txt').read()
names = [token[1:-1] for token in s.split(',')]
names.sort()
res = 0


def alphabet_value(c):
    al = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    i = al.find(c)
    assert i >= 0, c + ' not found'
    return i + 1


for i in range(len(names)):
    val = 0
    for c in names[i]:
        val += alphabet_value(c)
    res += (i + 1) * val

print(res)
