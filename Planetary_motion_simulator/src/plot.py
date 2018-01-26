import os
import os.path
import numpy as np
import pylab as pl

directory = os.path.dirname(os.path.realpath(__file__))
output = open(os.path.join(os.path.dirname(__file__), os.pardir, 'run', 'output.dat'))
data = output.readlines()
output.close()

n = 0
x = []
y = []
#z = []

line = data[0]
word = line.split()
n = int(word[1])

for i in range(0, len(data)):
    linei = data[i]
    for wordi in linei.split():
        if (wordi == 'Positions_of_objects:'):
            for j in range(1, n+1):
                linej = data[i+j]
                part = linej.split()
                xj = part[2]
                yj = part[3]
#                zj = part[4]
                x.append(float(xj))
                y.append(float(yj))
#                z.append(float(zj))

nx = len(x)
ny = len(y)
nmax = max(x) + 20*(10**9)

pl.figure(figsize=(12, 12))
pl.axes([0.06,0.06,0.9,0.9])
pl.xlim([-nmax,nmax])
pl.ylim([-nmax,nmax])
for i in range(0,n):
    pl.plot(x[i::n],y[i::n])
    pl.plot(x[nx-(1+i)],y[ny-(1+i)], 'o')
pl.savefig('solarsystem.jpg')

pl.xlim([-nmax/18,nmax/18])
pl.ylim([-nmax/18,nmax/18])
pl.savefig('innerplanets.jpg')

pl.xlim([-nmax/700,nmax/700])
pl.ylim([-nmax/700,nmax/700])
pl.savefig('centerstar.jpg')

# for (x,z)-coordinates:
#pl.figure(figsize=(12, 12))
#pl.axes([0.06,0.06,0.9,0.9])
#pl.xlim([-nmax,nmax])
#pl.ylim([-nmax,nmax])
#for i in range(0,n):
#    pl.plot(x[i::n],z[i::n])
#    pl.plot(x[nx-(1+i)],z[ny-(1+i)], 'o')
#pl.savefig('solarsystem_xz.jpg')

print ""
print ""
print "Saved files 'solarsystem.jpg', 'innerplanets.jpg' and 'centerstar.jpg' to directory: ", directory
