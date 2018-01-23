#include <iostream>
#include <iomanip>
#include <algorithm>
#include <unordered_set>
#include <random>
#include <chrono>

#include "matplotlibcpp.h"
#include "dynamic_connectivity.h"

namespace plt = matplotlibcpp;

std::vector<std::pair<size_t, size_t>> get_random_edges(size_t n, size_t m) {
    assert(n * (n - 1) >= 2 * m);
    static std::mt19937 gen(421987);
    std::vector<std::pair<size_t, size_t>> ans;
    for (size_t i = 0; i < n; ++i)
        for (size_t j = i + 1; j < n; ++j)
            ans.emplace_back(i, j);
    std::shuffle(ans.begin(), ans.end(), gen);
    ans.resize(m);
    return ans;
}

double benchmark(size_t n, size_t m, size_t its = 1) {
    assert(n * (n - 1) >= 2 * m);
    assert(its != 0);
    double sum = 0;
    for (size_t it = 0; it < its; ++it) {
        auto edges = get_random_edges(n, m);
        std::chrono::time_point<std::chrono::system_clock> start = std::chrono::system_clock::now();
        dc::dynamic_connectivity d(n);
        for (const auto& p : edges)
            d.link(p.first, p.second);
        for (const auto& p : edges)
            d.cut(p.first, p.second);
        std::chrono::time_point<std::chrono::system_clock> end = std::chrono::system_clock::now();
        std::chrono::duration<double> dt = end - start;
        sum += dt.count();
    }
    return sum / its;
}

template<typename asymptotic_t>
void benchmark_n(size_t n, asymptotic_t&& asymptotic, const std::string& filename = "") {
    std::vector<std::pair<size_t, double>> ans;
    size_t start = 2 * n;
    size_t finish = n * (n - 1) / 2 + 1;
    size_t step = (finish - start) / 300;
    for (size_t m = start; m < finish; m += step) {
        double time = benchmark(n, m, 10) / (2 * m);
        std::cout << std::setprecision(15) << "For m = " << m << ' ' << time << "seconds" << std::endl;
        ans.emplace_back(n + m, time / asymptotic(n + m));
    }
    std::sort(ans.begin(), ans.end());
    std::vector<size_t> t;
    std::vector<double> x;
    for (const auto& p : ans) {
        t.emplace_back(p.first);
        x.emplace_back(p.second);
    }
    plt::plot(t, x, "r-");
    plt::title("Dynamic connectivity asymptotic (n = " + std::to_string(n) + ")");
    plt::xlabel("|V| + |E|");
    plt::ylabel("$\\dfrac{seconds}{op} : log^2(m + n)$");
    if (filename.empty()) {
        plt::show();
    } else {
        plt::save(filename);
    }
    plt::clf();
}


int main() {
    auto asymptotic = [](size_t n) { return std::log(n) * std::log(n); };
    for (const size_t n : {100, 300, 500}) {
        benchmark_n(n, asymptotic, "graphics" + std::to_string(n) + ".png");
    }
}