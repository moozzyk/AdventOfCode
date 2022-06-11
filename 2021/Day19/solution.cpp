#include <vector>
#include <tuple>
#include <iostream>
#include <algorithm>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <set>

enum class Transposition
{
    xyz,
    zxy,
    yzx
};

struct Coordinates
{
    int x;
    int y;
    int z;

    Coordinates transpose(Transposition transposition) const
    {
        switch (transposition)
        {
        case Transposition::xyz:
            return Coordinates{x, y, z};
        case Transposition::zxy:
            return Coordinates{z, x, y};
        case Transposition::yzx:
            return Coordinates{y, z, x};
        default:
            throw std::logic_error("Unsupported rotation");
        }
    }

    Coordinates rotateRight() const
    {
        return {x, -z, y};
    }
};

using Beacons = std::vector<Coordinates>;

bool operator<(const Coordinates &l, const Coordinates &r)
{
    return l.x < r.x || (l.x == r.x && l.y < r.y) || (l.x == r.x && l.y == r.y && l.z < r.z);
};

bool operator==(const Coordinates &l, const Coordinates &r)
{
    return l.x == r.x && l.y == r.y && l.z == r.z;
}

Coordinates operator-(const Coordinates &l, const Coordinates &r)
{
    return {l.x - r.x, l.y - r.y, l.z - r.z};
}

Coordinates operator+(const Coordinates &l, const Coordinates &r)
{
    return {l.x + r.x, l.y + r.y, l.z + r.z};
}

Coordinates operator*(const Coordinates &l, const Coordinates &r)
{
    return {l.x * r.x, l.y * r.y, l.z * r.z};
}

void print(const Coordinates &c)
{
    std::cout << c.x << "," << c.y << "," << c.z << std::endl;
}

void print(const Beacons &beacons)
{
    for (const auto &c : beacons)
    {
        print(c);
    }
    std::cout << std::endl;
}

void print(const std::vector<Beacons> &scanners)
{
    for (const auto &s : scanners)
    {
        print(s);
    }
}

std::vector<Beacons> transpose(const Beacons &sourceBeacons)
{
    std::vector<Beacons> result;
    for (auto transposition : {Transposition::xyz, Transposition::zxy, Transposition::yzx})
    {
        std::vector<Beacons> newBeacons(8);
        for (const auto &beacon : sourceBeacons)
        {
            auto beaconPositive = beacon.transpose(transposition);
            auto beaconNegative = beaconPositive * Coordinates{-1, 1, -1};

            newBeacons[0].push_back(beaconPositive);
            newBeacons[1].push_back(newBeacons[0].back().rotateRight());
            newBeacons[2].push_back(newBeacons[1].back().rotateRight());
            newBeacons[3].push_back(newBeacons[2].back().rotateRight());

            newBeacons[4].push_back(beaconNegative);
            newBeacons[5].push_back(newBeacons[4].back().rotateRight());
            newBeacons[6].push_back(newBeacons[5].back().rotateRight());
            newBeacons[7].push_back(newBeacons[6].back().rotateRight());
        }
        result.insert(result.end(), newBeacons.begin(), newBeacons.end());
    }
    return result;
}

std::vector<Beacons> read_input(const std::string &file_name)
{
    std::ifstream file;
    file.open(file_name);
    std::vector<Beacons> scanners;
    std::string line;

    while (std::getline(file, line))
    {
        Beacons beacons;
        while (std::getline(file, line))
        {
            if (line.size() == 0)
            {
                break;
            }
            std::stringstream ss(line);

            int x, y, z;
            char c;
            ss >> x >> c >> y >> c >> z;
            beacons.push_back({x, y, z});
        }
        scanners.push_back(beacons);
    }
    return scanners;
}

std::vector<Coordinates> findCommonBeacons(Beacons b1, Beacons b2)
{
    std::sort(b1.begin(), b1.end());
    std::sort(b2.begin(), b2.end());

    Beacons result;
    std::set_intersection(b1.begin(), b1.end(), b2.begin(), b2.end(), std::back_inserter(result));
    return result;
}

std::pair<Beacons, Coordinates> findBestTransposition(const Beacons &b1, const Beacons &b2, int &overlap)
{
    overlap = 0;
    Beacons bestChoice;
    Coordinates bestChoiceDelta;
    for (const auto &startBeacon : b1)
    {
        Beacons newB2;
        newB2.reserve(b2.size());
        for (const auto &b : b2)
        {
            newB2.clear();
            auto delta = startBeacon - b;
            for (int i = 0; i < b2.size(); i++)
            {
                newB2.push_back(b2[i] + delta);
            }
            auto commonBeacons = findCommonBeacons(b1, newB2);
            if (commonBeacons.size() > overlap)
            {
                overlap = commonBeacons.size();
                bestChoice = newB2;
                bestChoiceDelta = delta;
            }
        }
    }
    return {bestChoice, bestChoiceDelta};
}

std::pair<std::vector<Beacons>, std::vector<Coordinates>> getMapFragments(const std::vector<Beacons> &input)
{
    std::vector<std::vector<Beacons>> transpositions{input.size()};
    for (int i = 1; i < input.size(); i++)
    {
        transpositions[i] = transpose(input[i]);
    }

    std::vector<Beacons> fragments{input.size()};
    fragments[0] = input[0];
    std::vector<Coordinates> scannerPositions{input.size()};
    scannerPositions[0] = Coordinates{0, 0, 0};
    for (int i = 0; i < fragments.size(); i++)
    {
        for (int fragIdx = 0; fragIdx < fragments.size(); fragIdx++)
        {
            if (fragments[fragIdx].empty())
            {
                continue;
            }

            for (int candidate = 0; candidate < fragments.size(); candidate++)
            {
                if (!fragments[candidate].empty())
                {
                    continue;
                }

                for (const auto &transposition : transpositions[candidate])
                {
                    int overlap;
                    const auto &[r, pos] = findBestTransposition(fragments[fragIdx], transposition, overlap);
                    if (overlap >= 12)
                    {
                        fragments[candidate] = r;
                        scannerPositions[candidate] = pos;
                        break;
                    }
                }
            }
        }
    }

    return {fragments, scannerPositions};
}

int problem1(const std::vector<Beacons> &fragments)
{
    std::set<Coordinates> map;
    for (const auto &f : fragments)
    {
        for (const auto &b : f)
        {
            map.insert(b);
        }
    }
    return map.size();
}

int problem2(const std::vector<Coordinates> &scannerPositions)
{
    int maxDist = 0;
    for (int i = 0; i < scannerPositions.size(); i++)
    {
        for (int j = i + 1; j < scannerPositions.size(); j++)
        {
            int dist =
                std::abs(scannerPositions[i].x - scannerPositions[j].x) +
                std::abs(scannerPositions[i].y - scannerPositions[j].y) +
                std::abs(scannerPositions[i].z - scannerPositions[j].z);
            maxDist = std::max(dist, maxDist);
        }
    }
    return maxDist;
}

int main(int argc, const char *argv[])
{
    auto input = read_input("input.txt");
    const auto &[mapFragments, scannerPositions] = getMapFragments(input);
    std::cout << "Problem 1: " << problem1(mapFragments) << std::endl;
    std::cout << "Problem 2: " << problem2(scannerPositions) << std::endl;
}