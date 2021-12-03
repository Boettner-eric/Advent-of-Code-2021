total = -1 # -1 instead of 0 for first value
current = 0
size = 1 # change for part1 vs part2 [1,3]
x = []
with open('day1.txt') as f:
    lines = f.readlines()
    for line in lines:
        x.append(int(line))
        if (len(x) >= size):
            if (sum(x) > current):
                total += 1
            current = sum(x);
            x.pop(0)
print(total)
