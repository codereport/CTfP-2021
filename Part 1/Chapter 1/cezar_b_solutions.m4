changequote([,])dnl
# Ex. 1
define([identity],[$1])dnl
identity(1)

# Ex. 2, 3
define([strip],$1)dnl
define([compose],[ifelse($#,1,[[strip($1($]][[*))]],[[strip($1]]([compose(shift($@))])[[)]])])dnl

define([f],[[A$1,B$2]])dnl
define([g],[[X$1$2,Y$3]])dnl
define([h],[[M$1$2,N$3,O$4]])dnl
define([u],compose([f],[g],[h]))dnl
u(1,2,3,4)

# Ex. 4
[A category where objects are websites and there is an arrow from a website A to a website B if A can ping/traceroute B.]

# Ex. 5
[No because friendship is not composable.]

# Ex. 6
[Objects are vertices, need self loops and if there is a directed edge A->B and B->C then we have a directed edge A->C. Ex. Complete graphs.]
