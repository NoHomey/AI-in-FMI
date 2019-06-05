#include <iostream>
#include "k_means.tpp"
#include "read.hpp"

constexpr std::size_t N = read_test_data::N;
constexpr std::size_t K = 3;

using Point = read_test_data::Point;
using Points = read_test_data::Points;
using Label = read_test_data::Label;
using Labels = read_test_data::Labels;
using Cluster = k_means::Cluster<N>;
using Clusters = k_means::Clusters<K, N>;
using Dist = k_means::Dist<N>;
using Score = std::pair<double, std::size_t>;

double l1_dist(const Point& p1, const Point& p2) {
    return k_means::l1_dist<N>(p1, p2);
}

double l2_dist(const Point& p1, const Point& p2) {
    return k_means::l2_dist<N>(p1, p2);
}

double l3_dist(const Point& p1, const Point& p2) {
    return k_means::l3_dist<N>(p1, p2);
}

double l4_dist(const Point& p1, const Point& p2) {
    return k_means::lq_dist<N, 4>(p1, p2);
}

double l_infinity_dist(const Point& p1, const Point& p2) {
    return k_means::l_infinity_dist<N>(p1, p2);
}

double s_dist(const Point& p1, const Point& p2) {
    return k_means::s_dist<N>(p1, p2);
}

Score score(const std::size_t correct, const std::size_t all, const char* classLabel) {
    const std::size_t incorrect = all - correct;
    const double wrong = static_cast<double>(incorrect) / all;
    std::cout << classLabel << ": " << wrong << std::endl;
    return {wrong, correct};
}

Score clusterScore(const Cluster& cluster, const Labels& labels) {
    std::size_t setosaCount = 0;
    std::size_t versicolorCount = 0;
    std::size_t virginicaCount = 0;
    for(std::size_t index : cluster.second) {
        if(labels[index] == Label::Setosa) {
            ++setosaCount;
        }
        if(labels[index] == Label::Versicolour) {
            ++versicolorCount;
        }
        if(labels[index] == Label::Virginica) {
            ++virginicaCount;
        }
    }
    const std::size_t clusterSize = cluster.second.size();
    if((setosaCount >= versicolorCount) && (setosaCount >= virginicaCount)) {
        return score(setosaCount, clusterSize, "Setosa");
    }
    if((versicolorCount >= setosaCount) && (versicolorCount >= virginicaCount)) {
        return score(versicolorCount, clusterSize, "Versicolor");
    }
    return score(virginicaCount, clusterSize, "Virginica"); 
}

template<Dist dist>
std::array<Score, K> scores(const std::pair<Points, Labels>& read) {
    Clusters clusters = k_means::k_means<K, N, dist, 4, 200, 10>(read.first);
    std::array<Score, K> score;
    for(std::size_t c = 0; c < K; ++c) {
        score[c] = clusterScore(clusters[c], read.second);
    }
    return std::move(score);
}

template<Dist dist>
double score(const std::pair<Points, Labels>& read) {
    std::array<Score, K> cs = scores<dist>(read);
    double sum = 0;
    for(Score score : cs) {
        sum += (static_cast<double>(score.second) / read.first.size());
    }
    return sum;
} 

int main() {
    std::pair<Points, Labels> read = read_test_data::read("./iris.csv");
    std::cout << "L1" << std::endl;
    std::cout << score<l1_dist>(read) << std::endl;
    std::cout << "L2" << std::endl;
    std::cout << score<l2_dist>(read) << std::endl;
    std::cout << "L3" << std::endl;
    std::cout << score<l3_dist>(read) << std::endl;
    std::cout << "L4" << std::endl;
    std::cout << score<l4_dist>(read) << std::endl;
    std::cout << "L_infinity" << std::endl;
    std::cout << score<l_infinity_dist>(read) << std::endl;
    std::cout << "S" << std::endl;
    std::cout << score<s_dist>(read) << std::endl;
}