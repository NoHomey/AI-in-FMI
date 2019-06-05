#pragma once

#include <cstddef>
#include <array>
#include <cmath>

namespace k_means {
template<std::size_t N> using Point = std::array<double, N>;

template<std::size_t N> using Points = std::vector<Point<N>>;

template<std::size_t N> using Dist = double(*)(const Point<N>&, const Point<N>&);

template<std::size_t N>
double l1_dist(const Point<N>& p1, const Point<N> p2);

template<std::size_t N>
double l2_dist(const Point<N>& p1, const Point<N> p2);

template<std::size_t N>
double l3_dist(const Point<N>& p1, const Point<N> p2);

template<std::size_t N, std::size_t q>
double lq_dist(const Point<N>& p1, const Point<N> p2);

template<std::size_t N>
double l_infinity_dist(const Point<N>& p1, const Point<N> p2);

template<std::size_t N>
double s_dist(const Point<N>& p1, const Point<N> p2);
}

template<std::size_t N>
double k_means::l1_dist(const Point<N>& p1, const Point<N> p2) {
    double dist = 0;
    for(std::size_t i = 0; i < N; ++i) {
        dist += std::abs(p1[i] - p2[i]);
    }
    return dist;
}

template<std::size_t N>
double k_means::l2_dist(const Point<N>& p1, const Point<N> p2) {
    return std::sqrt(s_dist(p1, p2));
}

template<std::size_t N>
double k_means::l3_dist(const Point<N>& p1, const Point<N> p2) {
    double dist = 0;
    for(std::size_t i = 0; i < N; ++i) {
        const double dist_i = std::abs(p1[i] - p2[i]);
        dist += dist_i * dist_i * dist_i;
    }
    return std::cbrt(dist);
}

template<std::size_t N, std::size_t q>
double k_means::lq_dist(const Point<N>& p1, const Point<N> p2) {
    double dist = 0;
    for(std::size_t i = 0; i < N; ++i) {
        dist += std::pow(std::abs(p1[i] - p2[i]), q);
    }
    return std::pow(dist, 1 / static_cast<double>(q));
}

template<std::size_t N>
double k_means::l_infinity_dist(const Point<N>& p1, const Point<N> p2) {
    double dist = 0;
    for(std::size_t i = 0; i < N; ++i) {
        dist = std::max(dist, std::abs(p1[i] - p2[i]));
    }
    return dist;
}

template<std::size_t N>
double k_means::s_dist(const Point<N>& p1, const Point<N> p2) {
    double dist = 0;
    for(std::size_t i = 0; i < N; ++i) {
        const double dist_i = p1[i] - p2[i];
        dist += dist_i * dist_i;
    }
    return dist;
}