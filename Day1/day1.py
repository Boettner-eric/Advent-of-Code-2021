last = 0
total = 0
with open('day1.txt') as f:
    a = f.readline()
    b = f.readline()
    line = f.readline()
    lineSum = int(a) + int(b) + int(line)
    x = [int(a), int(b), int(line)]
    current = sum(x)
    while line:
        line = f.readline()
        if (line != ''):
            x.pop(0)
            x.append(int(line))
            if (sum(x) > current):
                total += 1
            current = sum(x);
            #1581, 1618
print(total)
