#include <string>
#include <functional>
#include <iostream>

// #1 
template <class T> auto identity(T &&a) 
{
    return std::forward<T>(a);
}


class SomeClass
{
public:
    SomeClass() {}
    SomeClass(int x) : x(x) { }
    SomeClass(const SomeClass &obj)
    {
        // std::cout << "Copying someclass..." << obj.x << std::endl;
        x = obj.x;
    }
    int x = 3;
};

void testIdentity()
{
    int i = identity(5);
    std::cout << "Before: " << 5 << " After: " << i << std::endl;
    
    double d = identity(5.0);
    std::cout << "Before: " << 5.0 << " After: " << d << std::endl;
   
    std::string someStr("someStr");
    std::string str = identity(someStr);
    std::cout << "Before: " << someStr  << " After: " << str << std::endl;
   
    SomeClass c = SomeClass(5);
    const SomeClass &cc = identity(c);
    std::cout << "Before: " << c.x << " After: " << cc.x << std::endl;

    int *p = identity(&i);
    std::cout << "Before: " << i << " After: " << *p << std::endl;

    int &r = i;
    std::cout << "Before: " << i << " After: " << r << std::endl;
}

int succ(int n)
{
    return n + 1;
}

int sum(int n, int m)
{
    return n + m;
}

// #2
template <class F, class G> 
auto compose(F f, G g) 
{
    return [f,g](auto... args) { return f(g(args...)); };
}


void testComposition()
{
    auto succTwice = compose(succ, succ); 
    std::cout << succTwice(5) << std::endl;

    auto succOfSum = compose(succ, sum); 
    std::cout << succOfSum(5,6) << std::endl;
}

// #3
void testCompositionRespectsIdentity()
{
      // these and similar attempts don't seem to get past compiler. Not super familiar with template programming
//    auto doubleIdentityInt = compose(identity, identity);
//    auto test = identity(compose); 
    
}

int main()
{
    testIdentity();
    testComposition(); 
    testCompositionRespectsIdentity();
    return 0;
}

// #4
// From the viewpoint of a web-browser this makes sense.
// Though there is something to be said for a link that leads to nowhere.
// Failure to produce an object (page/file) by following a link might not be considered closed.
// Not sure what category theory would say about this, maybe there is room for "impurity".
//
// #5
// Seems reasonable. Probably room for argument that there are also more implicit morphisms than friendships.
// "People you may know" feature provides a way to produce profiles that aren't friendships.
// Not sure where the identity function fits in here though. 
// Does page refresh count? This seems like a different layer and maybe not applicable.
//
// #6
// I can't think of a case of a directed graph where it wouldn't be considered a category.
