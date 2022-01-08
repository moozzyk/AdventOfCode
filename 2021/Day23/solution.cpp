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

using position_key = std::tuple<int, int, char>;
struct position_hash
{
    std::size_t operator()(const position_key &t) const
    {
        auto [x, y, a] = t;
        return x << 8 | y << 4 | a;
    }
};

void printMoves(const std::vector<std::tuple<int, int, int> > &moves)
{
    for (auto [x, y, cost] : moves)
    {
        std::cout << "(" << x << "," << y << ")@" << cost << " ";
    }
}

std::unordered_map<position_key, std::vector<std::pair<int, int> >, position_hash> generateValidMoves()
{
    /*
  0123456789012
0 #############
1 #...........#
2 ###B#C#A#D###
3   #B#C#D#A#
4   #########
*/
    std::unordered_map<position_key, std::vector<std::pair<int, int> >, position_hash> moves;
    for (auto x : {1, 2, 4, 6, 8, 10, 11})
    {
        moves[std::make_tuple(x, 1, 'A')] = {{3, 2}, {3, 3}};
        moves[std::make_tuple(x, 1, 'B')] = {{5, 2}, {5, 3}};
        moves[std::make_tuple(x, 1, 'C')] = {{7, 2}, {7, 3}};
        moves[std::make_tuple(x, 1, 'D')] = {{9, 2}, {9, 3}};
    }

    std::vector<std::pair<int, int> > validHallwayPositions = {{1, 1}, {2, 1}, {4, 1}, {6, 1}, {8, 1}, {10, 1}, {11, 1}};
    std::vector<std::pair<int, int> > aPositions = {{3, 2}, {3, 3}};
    std::vector<std::pair<int, int> > bPositions = {{5, 2}, {5, 3}};
    std::vector<std::pair<int, int> > cPositions = {{7, 2}, {7, 3}};
    std::vector<std::pair<int, int> > dPositions = {{9, 2}, {9, 3}};

    for (auto x : {3, 5, 7, 9})
        for (auto y : {2, 3})
            for (auto a : {'A', 'B', 'C', 'D'})
            {
                auto k = std::make_tuple(x, y, a);
                moves[k] = validHallwayPositions;
                if (a == 'A' && x != 3)
                {
                    moves[k].push_back({3, 2});
                    moves[k].push_back({3, 3});
                }
                if (a == 'A' && x == 3 && y == 3)
                {
                    moves[k] = std::vector<std::pair<int, int> >{};
                }
                if (a == 'B' && x != 5)
                {
                    moves[k].push_back({5, 2});
                    moves[k].push_back({5, 3});
                }
                if (a == 'B' && x == 5 && y == 3)
                {
                    moves[k] = std::vector<std::pair<int, int> >{};
                }
                if (a == 'C' && x != 7)
                {
                    moves[k].push_back({7, 2});
                    moves[k].push_back({7, 3});
                }
                if (a == 'C' && x == 7 && y == 3)
                {
                    moves[k] = std::vector<std::pair<int, int> >{};
                }
                if (a == 'D' && x != 9)
                {
                    moves[k].push_back({9, 2});
                    moves[k].push_back({9, 3});
                }
                if (a == 'D' && x == 9 && y == 3)
                {
                    moves[k] = std::vector<std::pair<int, int> >{};
                }
            }
    // for (auto it = moves.begin(); it != moves.end(); it++)
    // {
    //     auto [x, y, a] = it->first;
    //     if (a != 'A')
    //         continue;
    //     std::cout << "(" << x << "," << y << "), " << a << ": {";
    //     for (auto [x1, y1] : it->second)
    //     {
    //         std::cout << "(" << x1 << "," << y1 << ") ";
    //     }
    //     std::cout << "}" << std::endl;
    // }
    return moves;
}

int getDistance(int x1, int y1, int x2, int y2, const diagram &d)
{
    std::queue<std::tuple<int, int, int> > q;
    std::unordered_set<position_key, position_hash> visited;
    q.push({x1, y1, 0});
    while (!q.empty())
    {
        auto [x, y, dist] = q.front();
        q.pop();
        if (visited.count({x, y, '_'}) > 0)
            continue;
        if (dist > 0 && d[y][x] != '.')
            continue;
        if (x == x2 && y == y2)
            return dist;
        visited.insert({x, y, '_'});
        q.push({x + 1, y, dist + 1});
        q.push({x - 1, y, dist + 1});
        q.push({x, y + 1, dist + 1});
        q.push({x, y - 1, dist + 1});
    }

    return -1;
}

bool moveCost(int x1, int y1, int x2, int y2, const diagram &d, int &cost)
{
    char a = d[y1][x1];

    // don't allow leaving once in place
    if (y1 == 3 && x1 == ((a - 'A') * 2 + 3))
        return false;

    // don't allow leaving once in place
    if (y1 == 2 && x1 == ((a - 'A') * 2 + 3) && d[3][x1] == a)
        return false;

    // don't allow entering as second if first not in place
    if (y2 == 2 && d[3][x2] != a)
        return false;

    auto distance = getDistance(x1, y1, x2, y2, d);
    if (distance < 0)
        return false;

    cost = distance * pow(10, a - 'A');
    return true;
}

std::vector<std::tuple<int, int, int> > getValidMoves(int x1, int y1, const diagram &d)
{
    static auto moves = generateValidMoves();
    char a = d[y1][x1];

    int cost;
    int targetX = 3 + (a - 'A') * 2;

    if (moveCost(x1, y1, targetX, 3, d, cost))
    {
        return {{targetX, 3, cost}};
    }

    if (moveCost(x1, y1, targetX, 2, d, cost))
    {
        return {{targetX, 2, cost}};
    }

    const auto &candidates = moves[std::make_tuple(x1, y1, a)];

    std::vector<std::tuple<int, int, int> > validMoves;
    for (auto [x2, y2] : candidates)
    {
        if (moveCost(x1, y1, x2, y2, d, cost))
        {
            validMoves.push_back({x2, y2, cost});
        }
    }
    std::sort(
        validMoves.begin(),
        validMoves.end(),
        [](auto f, auto s)
        { return std::get<2>(f) < std::get<2>(s); });
    return validMoves;
}

bool solved(const diagram &d)
{
    return d[2][3] == 'A' && d[2][5] == 'B' &&
           d[2][7] == 'C' && d[2][9] == 'D' &&
           d[3][3] == 'A' && d[3][5] == 'B' &&
           d[3][7] == 'C' && d[3][9] == 'D';
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
        std::cout << "====" << std::endl;
        return;
    }

    for (int x1 = 1; x1 < 12; ++x1)
        for (int y1 = 3; y1 > 0; --y1)
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

int main(int argc, const char *argv[])
{
    auto d = readInput("input.txt");

    // generateValidMoves();
    // printMoves(getValidMoves(7, 2, d));
    int minCost = 15000;
    solve(d, 0, minCost, 0);
    std::cout << minCost << std::endl;
}