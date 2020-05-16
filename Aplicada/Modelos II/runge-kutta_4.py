import numpy as np, math as m
import matplotlib.pyplot as plt

def f(t, y):
    #return -y**3+2*y**2-1.32*y+0.288
    return (0.8-y)*(0.6-y)**2

y0 = 0

a, b = 0, 20 # 1000, 10000
n = 100000 # 1000000

def runge_kutta4(f, a, b, n, y0):

    u = [y0]

    h = (b-a)/n
    
    for j in range(n):
        K1= f(a+j*h, u[j])
        K2= f(a+j*h+h/2, u[j]+h*K1/2)
        K3= f(a+j*h+h/2, u[j]+h*K2/2)
        K4= f(a+j*h+h, u[j]+h*K3)
              
        u.append(u[j]+h*(K1+2*K2+2*K3+K4)/6)

    return u


h = (b-a)/n

u = runge_kutta4(f, a, b, n, y0)

print(u[-1])

r = [a+i*h for i in range(n+1)]

plt.plot(r, u)

plt.show()
