#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <numeric>

struct Cuboid
{
    long x1, x2, y1, y2, z1, z2;
    bool action;

    long volume() const
    {
        return (x2 - x1 + 1) * (1 + y2 - y1) * (1 + z2 - z1) * (action ? 1 : -1);
    }

    std::string toString() const
    {
        std::ostringstream ss;
        ss << "x=(" << x1 << "," << x2 << ") y=("
           << y1 << "," << y2 << ") z=("
           << z1 << "," << z2 << ") " << (action ? "On" : "Off")
           << " vol=" << volume() << std::endl;
        return ss.str();
    }
};

std::vector<Cuboid> readInput(const std::string &fileName)
{
    std::ifstream file;
    file.open(fileName);
    std::vector<Cuboid> cuboids;
    std::string token;
    char discard;
    while (file >> token)
    {
        Cuboid c;
        c.action = token == "on";
        file >>
            discard >> discard >> c.x1 >> discard >> discard >> c.x2 >> discard >>
            discard >> discard >> c.y1 >> discard >> discard >> c.y2 >> discard >>
            discard >> discard >> c.z1 >> discard >> discard >> c.z2;
        cuboids.push_back(c);
    }
    return cuboids;
}

Cuboid intersect(Cuboid c1, Cuboid c2)
{
    Cuboid newCuboid;
    newCuboid.x1 = c1.x1 < c2.x1 ? c2.x1 : c1.x1;
    newCuboid.x2 = c1.x2 > c2.x2 ? c2.x2 : c1.x2;
    newCuboid.y1 = c1.y1 < c2.y1 ? c2.y1 : c1.y1;
    newCuboid.y2 = c1.y2 > c2.y2 ? c2.y2 : c1.y2;
    newCuboid.z1 = c1.z1 < c2.z1 ? c2.z1 : c1.z1;
    newCuboid.z2 = c1.z2 > c2.z2 ? c2.z2 : c1.z2;
    newCuboid.action = !c1.action;
    return newCuboid;
}

bool shouldIntersect(Cuboid c1, Cuboid c2)
{
    return !(c1.x2 < c2.x1 || c1.x1 > c2.x2 ||
             c1.y2 < c2.y1 || c1.y1 > c2.y2 ||
             c1.z2 < c2.z1 || c1.z1 > c2.z2);
}

unsigned long solve(const std::vector<Cuboid> &cuboids)
{
    std::vector<Cuboid> results;
    for (int i = 0; i < cuboids.size(); ++i)
    {
        auto resultSize = results.size();
        for (int j = 0; j < resultSize; ++j)
        {
            if (shouldIntersect(results[j], cuboids[i]))
            {
                results.push_back(intersect(results[j], cuboids[i]));
            }
        }
        if (cuboids[i].action)
        {
            results.push_back(cuboids[i]);
        }
    }

    return std::accumulate(
        results.begin(),
        results.end(),
        0UL,
        [](auto acc, auto c)
        { return c.volume() + acc; });
}

unsigned long problem1(const std::vector<Cuboid> &cuboids)
{
    std::vector<Cuboid> cuboidsSmall;
    std::copy_if(
        cuboids.begin(),
        cuboids.end(),
        std::back_inserter(cuboidsSmall),
        [](const auto &c)
        { return c.x1 >= -50 && c.x2 <= 50 && c.y1 >= -50 && c.y2 <= 50 && c.z1 >= -50 && c.z2 <= 50; });
    return solve(cuboidsSmall);
}

unsigned long problem2(const std::vector<Cuboid> &cuboids)
{
    return solve(cuboids);
}

int main(int argc, const char *argv[])
{
    auto cuboids = readInput("input.txt");
    std::cout << problem1(cuboids) << std::endl;
    std::cout << problem2(cuboids) << std::endl;
    return 0;
}
