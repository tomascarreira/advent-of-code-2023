lines = [
	"467..114..",
	"...*......",
	"..35..633.",
	"......#...",
	"617*......",
	".....+.58.",
	"..592.....",
	"......755.",
	"...$.*....",
	".664.598.."
]

def to_int(num):
	n = 0
	for i,d in enumerate(num):
		n += int(num[i]) * (10**(len(num) - i - 1))
	return n

def get_num(lin, col, lines):
	i = 1
	start = col
	while (col - i >= 0):
		if not lines[lin][col - i].isdigit():
			break
		i += 1

	start = col - i + 1

	num = []
	i = 0
	while (start + i < len(lines[0])):
		if not lines[lin][start+i].isdigit():
			break
		num.append(lines[lin][start+i])
		i += 1


	return to_int(num) 


def get_numbers_around(lin, col, lines):
	nums = []

	if lines[lin][col - 1].isdigit():
		nums.append(get_num(lin, col-1, lines))

	if lines[lin][col + 1].isdigit():
		nums.append(get_num(lin, col+1, lines))

	if not lines[lin-1][col].isdigit() and lines[lin-1][col-1].isdigit() and lines[lin-1][col+1].isdigit():
		nums.append(get_num(lin-1, col-1, lines))
		nums.append(get_num(lin-1, col+1, lines))
	elif lines[lin-1][col-1].isdigit():
		nums.append(get_num(lin-1, col-1, lines))
	elif lines[lin-1][col].isdigit():
		nums.append(get_num(lin-1, col, lines))
	elif lines[lin-1][col+1].isdigit():
		nums.append(get_num(lin-1, col+1, lines))

	if not lines[lin+1][col].isdigit() and lines[lin+1][col-1].isdigit() and lines[lin+1][col+1].isdigit():
		nums.append(get_num(lin+1, col-1, lines))
		nums.append(get_num(lin+1, col+1, lines))
	elif lines[lin+1][col-1].isdigit():
		nums.append(get_num(lin+1, col-1, lines))
	elif lines[lin+1][col].isdigit():
		nums.append(get_num(lin+1, col, lines))
	elif lines[lin+1][col+1].isdigit():
		nums.append(get_num(lin+1, col+1, lines))

	return nums

with open("day3.input") as f:
	lines = f.read().splitlines()

sum = 0

for i,l in enumerate(lines):
	for j,c in enumerate(l):
		if c == "*":
			numbers = get_numbers_around(i, j, lines)
			if len(numbers) == 2:
				sum += numbers[0] * numbers[1]

print(sum)
