#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Author:     Edoardo Costantini
Project:    Learning Python
Created:    2021-03-24
"""

## if, elif, else
# In general, the expression goes as follows:
# if condition :
#       expression
# elif condition :
#       expression
# else :
#       expression    
# For example:
z = 6
if z % 2 == 0 :
    print("checking " + str(z))
    print("z is even")
elif z % 3 == 0 :
    print("z is divisible by 3")
else : 
    print("z is neither divisible by 2 bor by 3")
    
# Note:
# a) Indentation is what marks the membership of a line to the if statement
#       (which is kinda dumb if you ask me)
# b) if the first condition is met, Python stops and will not return the
#       subsequnt one, even if true (e.g. try z = 6 in the code above)