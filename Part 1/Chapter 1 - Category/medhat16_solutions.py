# no. 1
id = lambda a: a # identity function


# no. 2
# composition
def compose(f, g):
  def f_of_g(x):
    return f(g(x))
  return f_of_g

f = lambda a: a + 1
g = lambda a: a * 2
print(compose(f, g)(5))



# no. 3
# testing composing identity

x = 5
f = lambda a: a + 1

print(True if compose(id , f)(x) == compose(f , id)(x) 
                  and compose(id , f)(x) == f(x) 
                  else False)

