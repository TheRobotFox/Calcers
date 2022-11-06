# Calcers
Simple Expression Parser made while learing Rust

### Handels standard operators
```
6/2*(1+2)
= 9
2^3
= 8
3!
= 6
```
Just enter an Expression and see if it works... most of the time it does

### Variables
```
a=7+1
a
= 8
b = a*2
b
= 16
a =1
b
= 2
```
Variables don't store values but expressions, which only get evaluated when asked to.
This means they will automatically update when you change other variables.

### Functions
```
f(x)=x^2
f(2)
= 4
f(x=3)
= 9
```
Function are now a thing! They are basically just Variables with automatic parameter mapping
eg. F(idx_0 -> a, idx_1 -> b, ...). This means that you can call all Variables with Parameter mapping.
Since normal variables are zero parameter functions there is no automated mapping, but the named mapping
works for all defined objects.
```
F=m*a
F(m=100,a=9.81)
= 981

F(m,a) = m*a
F(100,9.81)
= 981
```
Works the same!

### Use previous result as Variable
```
a=ans*2
1
= 1
a
= 2
a
= 4
```
This can be very fun, like my old pocket Calculators!
You just have to do...
```
a=ans+1
0
= 0
a
= 1

= 2

= 3
```
...and hold enter.
You just made poor man's CookieClickers!!! (Amazing)

### Predefined constants
```
r = 1.5
A = PI*r^2
A
= 7.0685834705770345
V = A*h
h = 5
V
= 35.34291735288517
```
Ah yes my favourite Number! 'e' (Eulers constant) is also a thing, but not much more yet!

### Commands
to delete all defined variables
```
:clear
```
for storing the environment (all defined variables) to the file 'env'
```
:save env
```
load environment from file 'env2'
```
:load env2
```
well...
```
:exit
```

