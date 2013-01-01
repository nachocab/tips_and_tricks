# debugging
import pdb; pdb.set_trace()
h # help
s # step
n # next
unt # until
r # return
c # continue
j LINE # jump to line number without executing
l # list source code
p EXPR # eval expression

# python console
python

# range
range(10) # [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

# list comprehensions (inline for)
[i for i in range(10) if i%2 == 0] # [0, 2, 4, 6, 8]

# sum list (arrays are called lists)
sum([0, 2, 4, 6, 8]) # 20

# make a copy of a list
a = b = [] # both point to the same list
b = a[:] # b is a copy of list a


# print a number with a string
print('The value of x is ' + str(x) + ', and y is ' + str(y))

# if else
if "something":
    something
else:
    something_else
