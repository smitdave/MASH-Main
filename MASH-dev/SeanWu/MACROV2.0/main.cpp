#include <iostream>
#include <memory>

int main() {

    std::shared_ptr<double> p = std::make_shared<double>(5);

    std::cout << *p << std::endl;

    std::cout << "Hello, World!" << std::endl;
    return 0;
}