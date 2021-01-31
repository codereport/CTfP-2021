#include <string>
#include <functional>
#include <iostream>

// copies in and out with UserDefined classes 
// #1 
template <class T> T identity(T a)
{
    return a;
}

class SomeClass
{
public:
    SomeClass() {}
    SomeClass(int x) : x(x) { }
    SomeClass(const SomeClass &obj)
    {
        std::cout << "Copying someclass..." << obj.x << std::endl;
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
    SomeClass cc = identity(c);
    std::cout << "Before: " << c.x << " After: " << cc.x << std::endl;

    int *p = identity(&i);
    std::cout << "Before: " << i << " After: " << *p << std::endl;
    int &r = i;
    std::cout << "Before: " << i << " After: " << r << std::endl;

    //int &rr = r;
    // int &rr = identity(r);
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
template <class T1, class T2, class T3> 
std::function<T3(T1)> compose(const std::function<T3(T2)> & f, const std::function<T2(T1)> & g) 
{
    return [&](T1 n) { return f(g(n)); };
}

void testComposition()
{
    std::cout << succ(5) << std::endl;
    std::cout << succ(succ(5)) << std::endl;

    auto doubleSucc = compose<int,int,int>(succ, succ);
    std::cout << doubleSucc(5) << std::endl;

    //auto succOfSum = compose<int, int, int>(succ, sum);
//    std::cout << succOfSum(5,6);

}

// #3
void testCompositionRespectsIdentity()
{
    
}

int main()
{
//    testIdentity();
    testComposition(); 
//    testCompositionRespectsIdentity();
    return 0;
}
