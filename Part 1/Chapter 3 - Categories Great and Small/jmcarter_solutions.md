## Solutions to challenges 3.6

### Q.1 Generate a Free category from:
#### a) A graph with one node and no edges
Simply add identity to the node
#### b) A graph with one node and one (directed) edge (hint: this edge can be composed with itself)
Simply add identity as well as as many arrows as needed to get identity or a cycle from composition of beginning arrow with itself (could be infinite). To make it simple, assume that first arrow composed with itself is identity (morphism1^2 = Id) or itself (morphism1^2 = morphism1).
#### c) A graph with two nodes and a single arrow between them
Simply add identity on each node.
#### d) A graph with a single node and 26 arrows marked with the letters of the alphabet: a, b, c ... z.
Add identity (arrow with empty character), plus arrow of concatenation of each other arrow. Creates an infinite amount of arrows, representing every combination of lowercase letters, without spaces.

### Q.2 What kind of order is this?
#### a) A set of sets with the inclusion relation: 𝐴 is included in 𝐵 if every element of 𝐴 is also an element of 𝐵.
This is a partial Order. If A is included in B, and B is included in A, then A == B. Also, it cannot be a total order because some sets have no relation (they are disjoint, have no interception).
#### b) C++ types with the following subtyping relation: T1 is a subtype of T2 if a pointer to T1 can be passed to a function that expects a pointer to T2 without triggering a compilation error.
This is a partial order. Not total order because some types have no relation. partial because if T1 is a subtype of T2 and T2 is a subtype of T1, then T1 == T2. Except if we accept cstyle casts, or reinterpret casts. In that case, it is a preorder.

### Q.3 Considering that Bool is a set of two values True and False, show that it forms two (set-theoretical) monoids with respect to, respectively, operator && (AND) and || (OR).
#### a) && (AND), using 1 as True, and 0 as False
+ Id :: 1
+ 1 && 1 = 1
+ 1 && 0 = 0; 0 && 1 = 0
+ 0 && 0 = 0
+ This Monoid is equivalent (isomorphic) to set (0, 1) with multiplication

#### b) || (OR), using 1 as True, and 0 as False
+ ID :: 0
+ 1 || 1 = 1
+ 0 || 1 = 1, 1 || 0 = 1
+ 0 || 0 = 0
+ This Monoid is equivalent to set (0, 1) with addition with ceiling at 1 (i.e. x = 1 if x > 0 else 0)

### Q.4 Represent the Bool monoid with the AND operator as a category: List the morphisms and their rules of composition.
There are 2 morphisms in this monoid.
+ &&1 (and with True), also known as the Identity morphism
+ &&0 (and with False)

The composition table:
+ &&1 . &&0 = &&0
+ &&0 . &&0 = &&0
+ &&1 . &&1 = &&1  

### Q.5 Represent addition modulo 3 as a monoid category.
There are 3 morphisms in this monoid.
+ +0 (adding by 0), also known as the Identity morphism for this object
+ +1 (adding by 1), which converts (0 -> 1 -> 2 -> 0), which is equivalent to positive cycling
+ +2 (adding by 2), which converts (0 -> 2 -> 1 -> 0), which is equivalent to negative cycling

The composition table:
+ +1 . +1 = +2
+ +2 . +2 = +1
+ +1 . +2 = +0
