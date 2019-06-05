#include <cstring>
#include "read.hpp"

static constexpr std::size_t maxLabelStringLength = 13;

static constexpr std::size_t headerLineLength = 71;

std::pair<read_test_data::Points, read_test_data::Labels> read_test_data::read(const char* filePath) {
    std::ifstream input(filePath);
    __hidden::ignoreHeaderLine(input);
    Points points;
    Labels labels;
    while(input.peek() != EOF) {
        __hidden::RowData rowData = __hidden::readRow(input);
        points.push_back(rowData.first);
        labels.push_back(rowData.second);
    }
    return {std::move(points), std::move(labels)};
}

void read_test_data::__hidden::ignoreHeaderLine(std::ifstream& input) {
    input.ignore(headerLineLength, '\n');
}

read_test_data::Label read_test_data::__hidden::labelFromKindString(const char* kindStr) {
    if(std::strcmp(kindStr, "\"setosa\"") == 0) {
        return Label::Setosa;
    }
    if(std::strcmp(kindStr, "\"versicolor\"") == 0) {
        return Label::Versicolour;
    }
    if(std::strcmp(kindStr, "\"virginica\"") == 0) {
        return Label::Virginica;
    }
    return Label::Unknown;
}

read_test_data::Point read_test_data::__hidden::readData(std::ifstream& input) {
    input.ignore();
    while(input.get() != ',');
    double sepalLength;
    double sepalWidth;
    double petalLength;
    double petalWidth;
    input >> sepalLength;
    input.clear();
    input.ignore();
    input >> sepalWidth;
    input.clear();
    input.ignore();
    input >> petalLength;
    input.clear();
    input.ignore();
    input >> petalWidth;
    input.clear();
    input.ignore();
    return {sepalLength, sepalWidth, petalLength, petalWidth};
}

read_test_data::Label read_test_data::__hidden::readLabel(std::ifstream& input) {
    char kindStr[maxLabelStringLength];
    input.getline(kindStr, maxLabelStringLength);
    return  labelFromKindString(kindStr);
}

read_test_data::__hidden::RowData read_test_data::__hidden::readRow(std::ifstream& input) {
    return {readData(input), readLabel(input)};
}