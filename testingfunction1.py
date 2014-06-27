
#
mylist = raw_input('input numbers as x, y: ' )
mylist = map(int, mylist.split()) #remap input to integer list
print (type(mylist))
print mylist
#
x = mylist[0]
print(x)
#
y = mylist[1]
print(y)
#
def medi(x,y):
    if x+y < 5:
        print ('little un')
        return(x+y)
    return(x)
x = str(medi(x,y))
print("my medi is:") + x