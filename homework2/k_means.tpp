#pragma once

#include <cstddef>
#include <array>
#include <vector>
#include <tuple>
#include <utility>
#include <cassert>
#include <future>
#include "Point.tpp"
#include "double_cmp.hpp"

namespace k_means {
using Indexes = std::vector<std::size_t>;

template<std::size_t N> using Cluster = std::pair<Point<N>, Indexes>;

template<std::size_t K, std::size_t N> using Clusters = std::array<Cluster<N>, K>;

template<std::size_t K, std::size_t N, Dist<N> dist, std::size_t H, std::size_t I, std::size_t D>
Clusters<K, N> k_means(const Points<N>& points);
}

namespace k_means {
namespace __hidden {
template<std::size_t N> using Mean = std::pair<Point<N>, std::size_t>;

template<std::size_t K, std::size_t N> using Means = std::array<Mean<N>, K>;

template<std::size_t K> using DistsToMeans = std::array<double, K>;

template<std::size_t K> using MeansDists = std::array<DistsToMeans<K>, K>;

using MinDist = std::tuple<std::size_t, std::size_t, double>;

template<std::size_t K, std::size_t N, Dist<N> dist>
Means<K, N> initialMeans(const Points<N>& points);

template<std::size_t K, std::size_t N, Dist<N> dist, std::size_t H>
Means<K, N> centroids(const Points<N>& points, const Means<K, N>& means);

template<std::size_t K, std::size_t N, Dist<N> dist, std::size_t D>
bool areClose(const Means<K, N>& means1, const Means<K, N>& means2);

template<std::size_t K, std::size_t N, Dist<N> dist>
Clusters<K, N> partition(const Points<N> points, const Means<K, N>& means);

template<std::size_t K, std::size_t N, Dist<N> dist, std::size_t H>
std::array<std::future<Means<K, N>>, H> portionMeans(const Points<N>& points, const Means<K, N>& means);

template<std::size_t K, std::size_t N, std::size_t H>
Means<K, N> newMeansFromPortionMeans(std::array<std::future<Means<K, N>>, H>&& futers);

template<std::size_t K, std::size_t N, Dist<N> dist>
bool hasClose(const Mean<N>& mean, const Means<K, N>& means, const double e);

template<std::size_t K, std::size_t N>
Means<K, N> firstAsMeans(const Points<N>& points);

template<std::size_t K, std::size_t N, Dist<N> dist>
MeansDists<K> dists(const Means<K, N>& means);

template<std::size_t K, std::size_t N, Dist<N> dist>
DistsToMeans<K> distsToMeans(const Point<N>& point, const Means<K, N>& means);

template<std::size_t K>
MinDist minDist(const MeansDists<K>& dists);

template<std::size_t K>
std::pair<std::size_t, double> minDist(const DistsToMeans<K>& dists);

template<std::size_t K, std::size_t N, Dist<N> dist>
void updateMeans(Means<K, N>& means, MeansDists<K>& dists, MinDist& minMeansDist, const Point<N>& point, const std::size_t i);

template<std::size_t K, std::size_t N, Dist<N> dist>
void updateMeans(Means<K, N>& means, MeansDists<K>& dists, MinDist& minMeansDist, const Point<N>& point, const DistsToMeans<K>& distsToMeans);

template<std::size_t N>
Mean<N> updateMean(const Mean<N>& mean, const Point<N>& point);

template<std::size_t N>
Mean<N> mergeMeans(const Mean<N>& m1, const Mean<N> m2);

template<std::size_t N>
Mean<N> pointMean(const Point<N>& point);
}
}

template<std::size_t K, std::size_t N, k_means::Dist<N> dist, std::size_t H, std::size_t I, std::size_t D>
k_means::Clusters<K, N>
k_means::k_means(const Points<N>& points) {
    __hidden::Means<K, N> means = __hidden::initialMeans<K, N, dist>(points);
    for(std::size_t i = 0; i < I; ++i) {
        const __hidden::Means<K, N> newMeans = __hidden::centroids<K, N, dist, H>(points, means);
        if(__hidden::areClose<K, N, dist, D>(newMeans, means)) {
            means = std::move(newMeans);
            break;
        }
        means = std::move(newMeans);
    }
    return __hidden::partition<K, N, dist>(points, means);
}

template<std::size_t K, std::size_t N, k_means::Dist<N> dist>
k_means::__hidden::Means<K, N>
k_means::__hidden::initialMeans(const Points<N>& points) {
    Means<K, N> means = firstAsMeans<K, N>(points);
    MeansDists<K> dists = __hidden::dists<K, N, dist>(means);
    MinDist minMeansDist = minDist<K>(dists);
    const std::size_t count = points.size();
    for(std::size_t i = 0; i < count; ++i) {
        const Point<N>& point = points[i];
        const DistsToMeans<K> distsFromPointToMeans = distsToMeans<K, N, dist>(point, means);
        const std::pair<std::size_t, double> minDistToMeans = minDist<K>(distsFromPointToMeans);
        if(less(minDistToMeans.second, std::get<2>(minMeansDist))) {
            updateMeans<K, N, dist>(means, dists, minMeansDist, point, minDistToMeans.first);
        } else {
            updateMeans<K, N, dist>(means, dists, minMeansDist, point, distsFromPointToMeans);
        }
    }
    return means;
}

template<std::size_t K, std::size_t N, k_means::Dist<N> dist, std::size_t H>
k_means::__hidden::Means<K, N> 
k_means::__hidden::centroids(const k_means::Points<N>& points, const k_means::__hidden::Means<K, N>& means) {
    return newMeansFromPortionMeans<K, N, H>(portionMeans<K, N, dist, H>(points, means));
}

template<std::size_t K, std::size_t N, k_means::Dist<N> dist, std::size_t D>
bool
k_means::__hidden::areClose(const k_means::__hidden::Means<K, N>& means1, const k_means::__hidden::Means<K, N>& means2) {
    static_assert(D > 0);
    const double e = std::pow(0.1, -static_cast<double>(D));
    for(const Mean<N>& mean : means1) {
        if(!hasClose<K, N, dist>(mean, means2, e)) {
            return false;
        }
    }
    return true;
}

template<std::size_t K, std::size_t N, k_means::Dist<N> dist>
k_means::Clusters<K, N>
k_means::__hidden::partition(const k_means::Points<N> points, const k_means::__hidden::Means<K, N>& means) {
    Clusters<K, N> clusters;
    for(std::size_t i = 0; i < K; ++i) {
        clusters[i].first = means[i].first;
    }
    const std::size_t count = points.size();
    for(std::size_t index = 0; index < count; ++index) {
        const Point<N>& point = points[index];
        std::size_t centroid = 0;
        double d = dist(point, means[0].first);
        for(std::size_t j = 1; j < K; ++j) {
            const double dj = dist(point, means[j].first);
            if(less(dj, d) || (equal(dj, d) && (clusters[j].second.size() < clusters[centroid].second.size()))) {
                centroid = j;
                d = dj;
            }
        }
        clusters[centroid].second.push_back(index);
    }
    return clusters;
}

template<std::size_t K, std::size_t N, k_means::Dist<N> dist, std::size_t H>
std::array<std::future<k_means::__hidden::Means<K, N>>, H>
k_means::__hidden::portionMeans(const Points<N>& points, const Means<K, N>& means) {
    static_assert(H > 0);
    const auto task = [&points, &means](const std::size_t start, const std::size_t end) -> Means<K, N> {
        Means<K, N> newMeans;
        for(Mean<N>& mean : newMeans) {
            mean.second = 0;
        }
        for(std::size_t i = start; i < end; ++i) {
            const k_means::Point<N>& point = points[i];
            std::size_t centroid = 0;
            double d = dist(point, means[centroid].first);
            for(std::size_t j = 1; j < K; ++j) {
                const double dj = dist(point, means[j].first);
                if(less(dj, d) || (equal(dj, d) && (newMeans[j].second < newMeans[centroid].second))) {
                    centroid = j;
                    d = dj;
                }
            }
            if(newMeans[centroid].second == 0) {
                newMeans[centroid] = pointMean<N>(point);
            } else {
                newMeans[centroid] = updateMean<N>(newMeans[centroid], point);
            }
        }
        return newMeans;
    };
    const std::size_t count = points.size();
    const std::size_t portion =  count / H;
    std::array<std::future<Means<K, N>>, H> futers;
    for(std::size_t h = 0; h < H; ++h) {
        const std::size_t start = h * portion;
        const std::size_t end = h == (H - 1) ? count : start + portion;
        futers[h] = std::async(std::launch::async, task, start, end);
    }
    return std::move(futers);
}

template<std::size_t K, std::size_t N, std::size_t H>
k_means::__hidden::Means<K, N>
k_means::__hidden::newMeansFromPortionMeans(std::array<std::future<Means<K, N>>, H>&& futers) {
    Means<K, N> newMeans = futers[0].get();
    for(std::size_t h = 1; h < H; ++h) {
        const Means<K, N> tempMeans = futers[h].get();
        for(std::size_t i = 0; i < K; ++i) {
            if((newMeans[i].second == 0) && (tempMeans[i].second != 0)) {
                newMeans[i] = tempMeans[i];
            } else if((newMeans[i].second != 0) && (tempMeans[i].second != 0)) {
                newMeans[i] = mergeMeans<N>(newMeans[i], tempMeans[i]);
            }
        }
    }
    return newMeans;
}

template<std::size_t K, std::size_t N, k_means::Dist<N> dist>
bool
k_means::__hidden::hasClose(const k_means::__hidden::Mean<N>& mean, const k_means::__hidden::Means<K, N>& means, const double e) {
    const Point<N>& point = mean.first;
    for(const Mean<N>& m : means) {
        if(less(dist(point, m.first), e)) {
            return true;
        }
    }
    return false;
}

template<std::size_t K, std::size_t N>
k_means::__hidden::Means<K, N>
k_means::__hidden::firstAsMeans(const Points<N>& points) {
    assert(points.size() >= K);
    Means<K, N> means;
    for(std::size_t i = 0; i < K; ++i) {
        means[i] = pointMean(points[i]);
    }
    return means;
}

template<std::size_t K, std::size_t N, k_means::Dist<N> dist>
k_means::__hidden::MeansDists<K>
k_means::__hidden::dists(const Means<K, N>& means) {
    MeansDists<K> dists;
    for(std::size_t i = 0; i < K; ++i) {
        for(std::size_t j = 0; j < i; ++j) {
            const double d = dist(means[i].first, means[j].first);
            dists[i][j] = d;
            dists[j][i] = d;
        }
        dists[i][i] = 0;
    }
    return dists;
}

template<std::size_t K, std::size_t N, k_means::Dist<N> dist>
k_means::__hidden::DistsToMeans<K>
k_means::__hidden::distsToMeans(const Point<N>& point, const Means<K, N>& means) {
    DistsToMeans<K> dists;
    for(std::size_t i = 0; i < K; ++i) {
        dists[i] = dist(point, means[i].first);
    }
    return dists;
}

template<std::size_t K>
k_means::__hidden::MinDist
k_means::__hidden::minDist(const MeansDists<K>& dists) {
    std::size_t from = 0;
    std::size_t to = 1;
    double dist = dists[from][to];
    for(std::size_t i = 0; i < K; ++i) {
        for(std::size_t j = 0; j < i; ++j) {
            if(less(dists[i][j], dist)) {
                from = i;
                to = j;
                dist = dists[from][to];
            }
        }
    }
    return {from, to, dist};
}

template<std::size_t K>
std::pair<std::size_t, double>
k_means::__hidden::minDist(const DistsToMeans<K>& dists) {
    std::size_t to = 0;
    double dist = dists[to];
    for(std::size_t i = 1; i < K; ++i) {
        if(less(dists[i], dist)) {
            to = i;
            dist = dists[to];
        }
    }
    return {to, dist};
}

template<std::size_t K, std::size_t N, k_means::Dist<N> dist>
void
k_means::__hidden::updateMeans( Means<K, N>& means
                               , MeansDists<K>& dists
                               , MinDist& minMeansDist
                               , const Point<N>& point
                               , const std::size_t i) {
    means[i] = updateMean(means[i], point);
    for(std::size_t j = 0; j < K; ++j) {
        if(i != j) {
            const double d = dist(means[i].first, means[j].first);
            dists[i][j] = d;
            dists[j][i] = d;
            if(less(d, std::get<2>(minMeansDist))) {
                minMeansDist = {i, j, d};
            }
        }
    }
}

template<std::size_t K, std::size_t N, k_means::Dist<N> dist>
void
k_means::__hidden::updateMeans( Means<K, N>& means
                               , MeansDists<K>& dists
                               , MinDist& minMeansDist
                               , const Point<N>& point
                               , const DistsToMeans<K>& distsToMeans) {
    const std::size_t from = std::get<0>(minMeansDist);
    const std::size_t to = std::get<1>(minMeansDist);
    means[from] = mergeMeans(means[from], means[to]);
    means[to] = pointMean(point);
    for(std::size_t i = 0; i < K; ++i) {
        if((i != from)) {
            const double d = dist(means[from].first, point);
            dists[from][i] = d;
            dists[i][from] = d;
        }
        if((i != to) && (i != from)) {
            dists[to][i] = distsToMeans[i];
            dists[i][to] = dists[to][i];
        }
    }
    minMeansDist = minDist(dists);
}

template<std::size_t N>
k_means::__hidden::Mean<N>
k_means::__hidden::updateMean(const Mean<N>& mean, const Point<N>& point) {
    return mergeMeans(mean, pointMean(point));
}

template<std::size_t N>
k_means::__hidden::Mean<N>
k_means::__hidden::mergeMeans(const Mean<N>& m1, const Mean<N> m2) {
    Point<N> m;
    const std::size_t count = m1.second + m2.second;
    const double a = static_cast<double>(m1.second) / count;
    const double b = static_cast<double>(m2.second) / count;
    for(std::size_t i = 0; i < N; ++i) {
        m[i] = a * m1.first[i] + b * m2.first[i];
    }
    return {m, count};
}

template<std::size_t N>
k_means::__hidden::Mean<N>
k_means::__hidden::pointMean(const Point<N>& point) {
    return {point, 1};
}