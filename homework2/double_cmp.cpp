#include "double_cmp.hpp"

static const double epsilon = 0.0000000000001;

bool equal(const double a, const double b, const double e) {
    return (a > (b - e)) && (a < (b + e));
}

bool equal(const double a, const double b) {
    return equal(a, b, epsilon);
}

bool less(const double a, const double b, const double e) {
    return a <= (b - e);
}

bool less(const double a, const double b) {
    return less(a, b, epsilon);
}

bool greater(const double a, const double b, const double e) {
    return a >= (b + e);
}

bool greater(const double a, const double b) {
    return greater(a, b, epsilon);
}