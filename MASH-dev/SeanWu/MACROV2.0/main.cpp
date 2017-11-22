#include <iostream>
#include <memory>

int main() {

    std::shared_ptr<double> p = std::make_shared(new double* x(5));

    std::cout << "Hello, World!" << std::endl;
    return 0;
}