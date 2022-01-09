#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <numeric>
#include <unordered_map>
#include <tuple>
#include <queue>
#include <unordered_set>

using diagram = std::vector<std::string>;

diagram readInput(const std::string &fileName)
{
    std::ifstream file;
    file.open(fileName);
    std::string line;
    std::vector<std::string> input;
    while (std::getline(file, line))
    {
        input.push_back(line);
    }
    return input;
}

void printDiagram(const diagram &d)
{
    for (const auto &s : d)
    {
        std::cout << s << std::endl;
    }
}

int getDistance(int x1, int y1, int x2, int y2, const diagram &d)
{
    int distance = 0;
    for (int y = y1 - 1; y >= 1; y--, distance++)
    {
        if (d[y][x1] != '.')
        {
            return -1;
        }
    }

    if (x1 < x2)
    {
        for (int x = x1 + 1; x <= x2; x++, distance++)
        {
            if (d[1][x] != '.')
            {
                return -1;
            }
        }
    }
    else
    {
        for (int x = x1 - 1; x >= x2; x--, distance++)
        {
            if (d[1][x] != '.')
            {
                return -1;
            }
        }
    }

    for (int y = 2; y <= y2; y++, distance++)
    {
        if (d[y][x2] != '.')
        {
            return -1;
        }
    }

    return distance;
}

int getCost(char a, int distance)
{
    return distance * pow(10, a - 'A');
}

bool isInFinalPosition(int x2, int y2, char a, const diagram &d)
{
    int targetX = 3 + (a - 'A') * 2;
    if (targetX != x2)
    {
        return false;
    }

    for (int y = y2 + 1; y <= d.size() - 2; y++)
    {
        if (d[y][x2] != a)
        {
            return false;
        }
    }
    return true;
}

void getFinalPositionMoves(int x1, int y1, const diagram &d, std::vector<std::tuple<int, int, int> > &moves)
{
    int a = d[y1][x1];
    int x2 = 3 + (a - 'A') * 2;
    int distance = 0;

    for (int y = y1 - 1; y >= 1; y--)
    {
        if (d[y][x1] != '.')
        {
            return;
        }
        distance++;
    }

    int step = x1 < x2 ? 1 : -1;
    for (int x = x1 + step; x != x2; x += step)
    {
        if (d[1][x] != '.')
        {
            return;
        }
        distance++;
    }

    int y2 = 1;
    for (; y2 <= d.size() - 2; ++y2)
    {
        if (d[y2][x2] != '.')
        {
            break;
        }
        distance++;
    }
    y2--;

    if (y2 <= 1)
    {
        return;
    }
    if (isInFinalPosition(x2, y2, a, d))
    {
        moves.push_back({x2, y2, getCost(a, distance)});
    }
}

void getHallwayMoves(int x1, int y1, const diagram &d, std::vector<std::tuple<int, int, int> > &moves)
{
    int distance = 0;
    int a = d[y1][x1];

    for (int y = y1 - 1; y >= 1; y--)
    {
        if (d[y][x1] != '.')
        {
            return;
        }
        distance++;
    }

    for (int x = x1 - 1; x >= 1; x--)
    {
        if (d[1][x] != '.')
        {
            break;
        }
        if (x != 3 && x != 5 && x != 7 && x != 9)
        {
            moves.push_back({x, 1, getCost(a, distance + x1 - x)});
        }
    }

    for (int x = x1 + 1; x <= 11; x++)
    {
        if (d[1][x] != '.')
        {
            break;
        }
        if (x != 3 && x != 5 && x != 7 && x != 9)
        {
            moves.push_back({x, 1, getCost(a, distance + x - x1)});
        }
    }
}

std::vector<std::tuple<int, int, int> > getValidMoves(int x1, int y1, const diagram &d)
{
    std::vector<std::tuple<int, int, int> > moves;
    if (isInFinalPosition(x1, y1, d[y1][x1], d))
    {
        return moves;
    }

    moves.reserve(20);
    getFinalPositionMoves(x1, y1, d, moves);
    if (!moves.empty() || y1 == 1)
    {
        return moves;
    }

    getHallwayMoves(x1, y1, d, moves);
    return moves;
}

bool solved(const diagram &d)
{
    bool res = true;
    for (int y = 2; y <= d.size() - 2; y++)
    {
        res = res && d[y][3] == 'A' && d[y][5] == 'B' && d[y][7] == 'C' && d[y][9] == 'D';
    }
    return res;
}

void solve(diagram &d, int cost, int &minCost, int depth)
{
    // std::cout << depth << "| " << cost << std::endl;
    // printDiagram(d);
    // std::cout << "====" << std::endl;

    if (cost >= minCost)
        return;

    if (solved(d))
    {
        std::cout << "Solved @: " << cost << std::endl;
        minCost = std::min(cost, minCost);
        return;
    }

    for (int x1 = 1; x1 < 12; ++x1)
        for (int y1 = d.size() - 2; y1 > 0; --y1)
        {
            auto a = d[y1][x1];
            if (a == 'A' || a == 'B' || a == 'C' || a == 'D')
            {
                auto moves = getValidMoves(x1, y1, d);
                for (auto [x2, y2, moveCost] : moves)
                {
                    d[y1][x1] = '.';
                    d[y2][x2] = a;
                    solve(d, cost + moveCost, minCost, depth + 1);
                    d[y2][x2] = '.';
                    d[y1][x1] = a;
                }
            }
        }
}

int problem(const std::string &inputName)
{
    auto d = readInput(inputName);
    int minCost = INT_MAX;
    solve(d, 0, minCost, 0);
    return minCost;
}

int main(int argc, const char *argv[])
{
    std::cout << "Problem 1: " << problem("input.txt") << std::endl;
    std::cout << "Problem 2: " << problem("input1.txt") << std::endl;
}