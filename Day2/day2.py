distance = 0
depth = 0
with open('day2.txt') as f:
    lines = f.readlines()
    for line in lines:
        command, data = line.split(' ')
        if (command == 'forward'):
            distance += int(data)
        elif (command == 'down'):
            depth += int(data)
        elif (command == 'up'):
            depth -= int(data)
        else:
            print(f'error {command}')
print(f'{depth} {distance}: {depth * distance}')

distance = 0
depth = 0
aim = 0
with open('day2.txt') as f:
    lines = f.readlines()
    for line in lines:
        command, data = line.split(' ')
        if (command == 'forward'):
            distance += int(data)
            depth += aim * int(data)
        elif (command == 'down'):
            aim += int(data)
        elif (command == 'up'):
            aim -= int(data)
        else:
            print(f'error {command}')
print(f'{depth} {distance} : {depth * distance}')
