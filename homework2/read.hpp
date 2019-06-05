#pragma once

#include <fstream>
#include <utility>
#include <vector>
#include "Point.tpp"

namespace read_test_data {
constexpr std::size_t N = 4;

using Point = k_means::Point<N>;

using Points = k_means::Points<N>;

enum class Label {
    Setosa,
    Versicolour,
    Virginica,
    Unknown
};

using Labels = std::vector<Label>;

std::pair<Points, Labels> read(const char* filePath);
}

namespace read_test_data {
namespace __hidden {
using RowData = std::pair<Point, Label>;

void ignoreHeaderLine(std::ifstream& input);

Label labelFromKindString(const char* kindStr);

Point readData(std::ifstream& input);

Label readLabel(std::ifstream& input);

RowData readRow(std::ifstream& input);
}
}