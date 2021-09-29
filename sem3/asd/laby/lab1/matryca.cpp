#include <iostream>
#include <algorithm>


int length, distance;
char lastChar = '#';
std::string pattern;

int main() {

    std::cin >> pattern;
    length = pattern.size();
    distance = length - 1;

    for (int i = 0, index = 0; i < distance; i++) {
        if (pattern[i] != '*') {
            if (lastChar != pattern[i] && lastChar != '#') {
                distance = std::min(distance, i - index - 1);
            }

            lastChar = pattern[i];
            index = i;
        }
    }

    std::cout << length - distance;
}