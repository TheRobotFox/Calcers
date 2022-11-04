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
This means they will automatically update when you change other variables, providing the basic
functionallity of functions.

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

