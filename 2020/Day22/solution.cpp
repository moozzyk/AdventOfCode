#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

std::vector<std::vector<int>> read_cards(const std::string& file_name) {
  std::ifstream file;
  file.open(file_name);
  std::string line;
  std::vector<std::vector<int>> hands;
  while (std::getline(file, line)) {
    if (line == "") {
      continue;
    }
    if (line.find("Player") != std::string::npos) {
      hands.push_back({});
    } else {
      hands.back().push_back(std::stoi(line));
    }
  }
  return hands;
}

void play_combat(std::deque<int>& hand1, std::deque<int>& hand2) {
  while (!(hand1.empty() || hand2.empty())) {
    auto c1 = hand1.front();
    auto c2 = hand2.front();
    hand1.pop_front();
    hand2.pop_front();
    if (c1 > c2) {
      hand1.push_back(c1);
      hand1.push_back(c2);
    } else {
      hand2.push_back(c2);
      hand2.push_back(c1);
    }
  }
}

int score(const std::deque<int>& hand) {
  int i = 1;
  int result = 0;
  for (auto it = hand.rbegin(); it != hand.rend(); it++, i++) {
    result += i * *it;
  }
  return result;
}

int problem1(const std::vector<int>& hand1, const std::vector<int>& hand2) {
  std::deque<int> h1(hand1.begin(), hand1.end());
  std::deque<int> h2(hand2.begin(), hand2.end());

  play_combat(h1, h2);
  return score(h1.empty() ? h2 : h1);
}

std::deque<int> play_recursive_combat(std::deque<int>& h1, std::deque<int>& h2);

bool sub_game(const std::deque<int>& hand1, const std::deque<int>& hand2) {
  std::deque<int> h1(hand1.begin() + 1, hand1.begin() + 1 + hand1.front());
  std::deque<int> h2(hand2.begin() + 1, hand2.begin() + 1 + hand2.front());
  play_recursive_combat(h1, h2);
  return h2.empty();
}

std::deque<int> play_recursive_combat(std::deque<int>& h1, std::deque<int>& h2) {
  std::unordered_set<std::string> past_hands;

  for (int round = 0;; round++) {
    std::stringstream h1_hash, h2_hash;
    std::copy(h1.begin(), h1.end(), std::ostream_iterator<int>(h1_hash, "_"));
    std::copy(h2.begin(), h2.end(), std::ostream_iterator<int>(h2_hash, "_"));
    std::string hash = h1_hash.str() + "|" + h2_hash.str();
    if (past_hands.find(hash) != past_hands.end()) {
      h1.clear();
      h2.clear();
      return h1;
    }
    past_hands.insert(hash);
    auto c1 = h1.front();
    auto c2 = h2.front();
    bool first_won = false;
    if (h1.size() > c1 && h2.size() > c2) {
      first_won = sub_game(h1, h2);
    } else {
      first_won = c1 > c2;
    }
    h1.pop_front();
    h2.pop_front();
    if (first_won) {
      h1.push_back(c1);
      h1.push_back(c2);
    } else {
      h2.push_back(c2);
      h2.push_back(c1);
    }
    if (h1.empty()) {
      return h2;
    }
    if (h2.empty()) {
      return h1;
    }
  }
}

int problem2(const std::vector<int>& hand1, std::vector<int>& hand2) {
  std::deque<int> h1(hand1.begin(), hand1.end());
  std::deque<int> h2(hand2.begin(), hand2.end());
  auto winner_hand = play_recursive_combat(h1, h2);
  return score(winner_hand);
}

int main(int argc, const char* argv[]) {
  auto hands = read_cards("input.txt");
  std::cout << problem1(hands[0], hands[1]) << std::endl;
  std::cout << problem2(hands[0], hands[1]) << std::endl;
  return 0;
}