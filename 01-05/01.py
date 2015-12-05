t=open("1.in").read()
m={"(":1,")":-1}
s=lambda i:sum(map(lambda c:m[c] if c in m else 0, t[:i]))
print("1:", sum(map(lambda c:m[c] if c in m else 0, t)))
print("2:", *filter(lambda x:x[0]==-1,map(lambda y:(s(y),y),range(len(t)))))
