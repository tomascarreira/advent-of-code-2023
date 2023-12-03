test = [
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
		n += num[i] * (10**(len(num) - i - 1))
	return n

def is_part_number(pos, lines):
	if (pos[0] < 0 or pos[0] >= len(lines)):
		return False
	if (pos[1] < 0 or pos[1] >= len(lines[0])):
		return False

	c = lines[pos[0]][pos[1]]
	return not (c.isdigit() or c == ".")


with open("day3.input") as f:
	test = f.read().splitlines()
			
parsing_num = False
num = []
part_num = False

sum = 0;

for i,l in enumerate(test):
	if part_num:
		sum += to_int(num)
	num = []
	part_num = False
	for j,c in enumerate(l):
		if c.isdigit() and not parsing_num:
			parsing_num = True
			num.append(int(c))
			if is_part_number((i-1, j), test):
				part_num = True
			if is_part_number((i-1, j-1), test):
				part_num = True
			if is_part_number((i, j-1), test):
				part_num = True
			if is_part_number((i+1, j-1), test):
				part_num = True
			if is_part_number((i+1, j), test):
				part_num = True

		elif c.isdigit() and parsing_num:
			num.append(int(c))
			if is_part_number((i-1, j), test):
				part_num = True
			if is_part_number((i+1, j), test):
				part_num = True

		elif not c.isdigit() and parsing_num:
			parsing_num = False
			if is_part_number((i-1, j), test):
				part_num = True
			if is_part_number((i, j), test):
				part_num = True
			if is_part_number((i+1, j), test):
				part_num = True
			if part_num:
				print(num, to_int(num))
				sum += to_int(num)
			num = []
			part_num = False

print(sum)
