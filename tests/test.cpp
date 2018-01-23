#include <gtest/gtest.h>
#include <unordered_set>
#include "dynamic_connectivity.h"

using namespace dc;

TEST(euler_tour_tree, check_vertices_index) {
    euler_tour_tree ett(5);
    EXPECT_THROW(ett.cut(3, 10), euler_tour_tree_exception);
    EXPECT_THROW(ett.cut(10, 3), euler_tour_tree_exception);
    EXPECT_THROW(ett.cut(10, 10), euler_tour_tree_exception);
    EXPECT_THROW(ett.cut(4, 5), euler_tour_tree_exception);
    EXPECT_NO_THROW(ett.cut(3, 2));

    EXPECT_THROW(ett.link(3, 10), euler_tour_tree_exception);
    EXPECT_THROW(ett.link(10, 3), euler_tour_tree_exception);
    EXPECT_THROW(ett.link(10, 10), euler_tour_tree_exception);
    EXPECT_THROW(ett.link(4, 5), euler_tour_tree_exception);
    EXPECT_NO_THROW(ett.link(3, 2));

    EXPECT_THROW(ett.are_connected(3, 10), euler_tour_tree_exception);
    EXPECT_THROW(ett.are_connected(10, 3), euler_tour_tree_exception);
    EXPECT_THROW(ett.are_connected(10, 10), euler_tour_tree_exception);
    EXPECT_THROW(ett.are_connected(4, 5), euler_tour_tree_exception);
    EXPECT_NO_THROW(ett.are_connected(3, 2));
}

TEST(euler_tour_tree, reflexive) {
    const size_t N = 10;
    euler_tour_tree ett(N);
    for (size_t i = 0; i < N; ++i) {
        EXPECT_TRUE(ett.are_connected(i, i));
    }
}

TEST(euler_tour_tree, empty) {
    const size_t N = 10;
    euler_tour_tree ett(N);
    for (size_t i = 0; i < N; ++i) {
        for (size_t j = 0; j < N; ++j) {
            EXPECT_EQ(ett.are_connected(i, j), i == j);
        }
    }
}

struct dsu {
    dsu(size_t n) : p(n) {
        std::iota(p.begin(), p.end(), 0ULL);
    }

    size_t get(size_t u) {
        return p[u] == u ? u : (p[u] = get(p[u]));
    }

    void link(size_t u, size_t v) {
        u = get(u);
        v = get(v);
        p[u] = v;
    }

    bool is_connected(size_t u, size_t v) {
        return get(u) == get(v);
    }

private:
    std::vector<size_t> p;
};

TEST(euler_tour_tree, only_link) {
    const size_t N = 10;
    euler_tour_tree ett(N);
    EXPECT_FALSE(ett.are_connected(0, 1));
    ett.link(0, 1);
    EXPECT_TRUE(ett.are_connected(0, 1));
    EXPECT_FALSE(ett.are_connected(0, 2));
    ett.link(1, 2);
    EXPECT_TRUE(ett.are_connected(0, 2));
}

TEST(euler_tour_tree, only_link_random) {
    const size_t N = 1000;
    euler_tour_tree ett(N);
    dsu d(N);
    const size_t M = 100000;
    for (size_t i = 0; i < M; ++i) {
        size_t u = rand_size_t(N);
        size_t v = rand_size_t(N);
        if (rand_size_t(4) == 0) {
            if (d.is_connected(u, v)) {
                --i;
                continue;
            }
            EXPECT_FALSE(d.is_connected(u, v));
            d.link(u, v);
            EXPECT_TRUE(d.is_connected(u, v));
            EXPECT_FALSE(ett.are_connected(u, v));
            ett.link(u, v);
            EXPECT_TRUE(ett.are_connected(u, v));
        } else {
            EXPECT_EQ(ett.are_connected(u, v), d.is_connected(u, v));
        }
    }
}

TEST(euler_tour_tree, link_cut) {
    const size_t N = 10;
    euler_tour_tree ett(N);
    EXPECT_FALSE(ett.are_connected(0, 1));
    ett.link(0, 1);
    EXPECT_TRUE(ett.are_connected(0, 1));
    ett.cut(0, 1);
    EXPECT_FALSE(ett.are_connected(0, 1));
}

struct stupid_dc {
    stupid_dc(size_t cnt_vertices, bool forest = true)
            : edges(cnt_vertices)
            , was(cnt_vertices)
            , forest(forest) {}

    void link(size_t u, size_t v) {
        if (forest && is_connected(u, v)) {
            throw euler_tour_tree_exception("Unsupported operation yet");
        }
        edges[u].insert(v);
        edges[v].insert(u);
    }

    void cut(size_t u, size_t v) {
        edges[u].erase(v);
        edges[v].erase(u);
    }

    bool is_connected(size_t u, size_t v) {
        std::fill(was.begin(), was.end(), false);
        return dfs(u, v);
    }

    bool exists(size_t u, size_t v) {
        return u == v || edges[u].count(v) != 0;
    }

    bool dfs(size_t u, size_t goal) {
        if (u == goal)
            return true;
        was[u] = true;
        for (size_t v: edges[u])
            if (!was[v] && dfs(v, goal))
                return true;
        return false;
    }

    std::vector<std::unordered_set<size_t>> edges;
    std::vector<bool> was;
    bool forest;
};

TEST(euler_tour_tree, link_cut_random) {
    const size_t N = 200;
    euler_tour_tree ett(N);
    stupid_dc dc(N);
    const size_t M = 100000;
    std::set<std::pair<size_t, size_t>> edges;
    for (size_t i = 0; i < M; ++i) {
        switch (rand_size_t(3)) {
            case 0: {
                size_t u = rand_size_t(N);
                size_t v = rand_size_t(N);
                if (!dc.is_connected(u, v)) {
                    dc.link(u, v);
                    ett.link(u, v);
                    edges.insert({u, v});
                }
                break;
            }
            case 1: {
                if (edges.empty()) {
                    continue;
                }

                auto uv = std::vector<edge_t>(edges.begin(), edges.end())[rand_size_t(edges.size())];
                edges.erase(uv);
                ett.cut(uv.first, uv.second);
                dc.cut(uv.first, uv.second);
                break;
            }
            case 2: {
                size_t u = rand_size_t(N);
                size_t v = rand_size_t(N);
                EXPECT_EQ(ett.are_connected(u, v), dc.is_connected(u, v));
                break;
            }
            default:
                break;
        }
    }
}

TEST(treap, constructor) {
    auto tn = new treap_node(1);
    tn->l = new treap_node(2);
    tn->r = new treap_node(2, 3);
    EXPECT_TRUE(tn->is_vertex());
    EXPECT_TRUE(tn->l->is_vertex());
    EXPECT_FALSE(tn->r->is_vertex());
    EXPECT_EQ(tn->l->u, 2);
    EXPECT_EQ(tn->u, 1);
    EXPECT_EQ(tn->r->u, 2);
    EXPECT_EQ(tn->r->v, 3);
}

std::vector<treap_node*> get_vector(size_t n) {
    std::vector<treap_node*> vec;
    for (size_t i = 0; i < n; ++i) {
        vec.push_back(new treap_node(i));
    }
    treap_node* treap = vec[0];
    for (size_t i = 1; i < n; ++i) {
        treap = merge(treap, vec[i]);
    }
    vec.push_back(treap);
    return vec;
}

void remove_vector(const std::vector<treap_node*>& vec, size_t n) {
    for (size_t i = 0; i < n; ++i) {
        delete vec[i];
    }
}

TEST(treap, get_cnt) {
    const size_t N = 1000;
    auto vec = get_vector(N);
    for (size_t i = 0; i < N; ++i) {
        EXPECT_EQ(get_cnt(vec[i]), i + 1);
    }
    remove_vector(vec, N);
}

TEST(treap, split_cnt) {
    const size_t N = 10000;
    auto vec = get_vector(N);
    auto treap = vec[N];
    const size_t M = 1000;
    for (size_t i = 0; i < M; ++i) {
        size_t cnt = rand_size_t(N) + 1;
        treap_node* a;
        treap_node* b;
        split_cnt(treap, a, b, cnt);
        EXPECT_EQ(size(a), cnt);
        EXPECT_EQ(size(b), N - cnt);
        treap = merge(a, b);
    }
    remove_vector(vec, N);
}

TEST(treap, split) {
    const size_t N = 100;
    auto vec = get_vector(N);
    auto treap = vec[N];
    vec.erase(vec.begin() + N);
    const size_t M = 100000;
    for (size_t i = 0; i < M; ++i) {
        size_t x = rand_size_t(N);
        treap_node* a;
        treap_node* b;
        split(vec[x], a, b);
        EXPECT_EQ(vec[x], get_rightmost(a));
        treap = merge(a, b);
        EXPECT_EQ(size(treap), N);
    }
    remove_vector(vec, N);
}

TEST(dc, check_vertices_index) {
    dynamic_connectivity dc(5);
    EXPECT_THROW(dc.cut(3, 10), dc_exception);
    EXPECT_THROW(dc.cut(10, 3), dc_exception);
    EXPECT_THROW(dc.cut(10, 10), dc_exception);
    EXPECT_THROW(dc.cut(4, 5), dc_exception);
    EXPECT_THROW(dc.cut(3, 2), dc_exception); // because we haven't edge (3, 2)

    EXPECT_THROW(dc.link(3, 10), dc_exception);
    EXPECT_THROW(dc.link(10, 3), dc_exception);
    EXPECT_THROW(dc.link(10, 10), dc_exception);
    EXPECT_THROW(dc.link(4, 5), dc_exception);
    EXPECT_NO_THROW(dc.link(3, 2));

    EXPECT_THROW(dc.are_connected(3, 10), dc_exception);
    EXPECT_THROW(dc.are_connected(10, 3), dc_exception);
    EXPECT_THROW(dc.are_connected(10, 10), dc_exception);
    EXPECT_THROW(dc.are_connected(4, 5), dc_exception);
    EXPECT_NO_THROW(dc.are_connected(3, 2));
}

TEST(dc, reflexive) {
    const size_t N = 10;
    dynamic_connectivity dc(N);
    for (size_t i = 0; i < N; ++i) {
        EXPECT_TRUE(dc.are_connected(i, i));
    }
}

TEST(dc, empty) {
    const size_t N = 10;
    dynamic_connectivity dc(N);
    for (size_t i = 0; i < N; ++i) {
        for (size_t j = 0; j < N; ++j) {
            EXPECT_EQ(dc.are_connected(i, j), i == j);
        }
    }
}

info_t get_min_info_long(treap_node* node) {
    if (!node)
        return INF;
    info_t ans = node->info;
    ans |= get_min_info_long(node->l);
    ans |= get_min_info_long(node->r);
    return ans;
}

TEST(treap, info) {
    size_t const N = 100;
    auto vec = get_vector(N);
    auto treap = vec[N];
    for (size_t i = 0; i < N; ++i) {
        vec[i]->info = {!bool(rand_size_t(N)), false};
        update_info_up(vec[i]);
    }
    size_t const M = 1000;
    for (size_t i = 0; i < M; ++i) {
        size_t cnt = rand_size_t(N) + 1;
        treap_node* a;
        treap_node* b;
        split_cnt(treap, a, b, cnt);
        EXPECT_EQ(get_min_info_long(a), get_best(a));
        EXPECT_EQ(get_min_info_long(b), get_best(b));
        treap = merge(a, b);
        EXPECT_EQ(get_min_info_long(treap), get_best(treap));
    }

    remove_vector(vec, N);
}

void test_dc_link_cut_random_forest(size_t N, size_t M) {
    dynamic_connectivity real_dc(N);
    stupid_dc dc(N);
    std::set<std::pair<size_t, size_t>> edges;
    for (size_t i = 0; i < M; ++i) {
        switch (rand_size_t(3)) {
            case 0: {
                size_t u = rand_size_t(N);
                size_t v = rand_size_t(N);
                if (!dc.is_connected(u, v)) {
                    dc.link(u, v);
                    real_dc.link(u, v);
                    edges.insert({u, v});
                }
                break;
            }
            case 1: {
                if (edges.empty()) {
                    continue;
                }

                auto uv = std::vector<edge_t>(edges.begin(), edges.end())[rand_size_t(edges.size())];
                edges.erase(uv);
                real_dc.cut(uv.first, uv.second);
                dc.cut(uv.first, uv.second);
                break;
            }
            case 2: {
                size_t u = rand_size_t(N);
                size_t v = rand_size_t(N);
                EXPECT_EQ(real_dc.are_connected(u, v), dc.is_connected(u, v));
                break;
            }
            default:
                break;
        }
    }
}

TEST(dc, link_cut_random_forest_v2_op1000000) {
    test_dc_link_cut_random_forest(2, 1000000);
}

TEST(dc, link_cut_random_forest_v3_op1000000) {
    test_dc_link_cut_random_forest(3, 1000000);
}

TEST(dc, link_cut_random_forest_v4_op1000000) {
    test_dc_link_cut_random_forest(4, 1000000);
}

TEST(dc, link_cut_random_forest_v5_op1000000) {
    test_dc_link_cut_random_forest(5, 1000000);
}

TEST(dc, link_cut_random_forest_v16_op1000000) {
    test_dc_link_cut_random_forest(16, 1000000);
}

TEST(dc, link_cut_random_forest_v100_op1000000) {
    test_dc_link_cut_random_forest(100, 1000000);
}

TEST(dc, link_cut) {
    dynamic_connectivity dc(3);
    dc.link(0, 1);
    EXPECT_TRUE(dc.are_connected(0, 1));
    EXPECT_FALSE(dc.are_connected(0, 2));
    EXPECT_FALSE(dc.are_connected(1, 2));
    dc.link(0, 2);
    EXPECT_TRUE(dc.are_connected(0, 2));
    EXPECT_TRUE(dc.are_connected(0, 1));
    EXPECT_TRUE(dc.are_connected(2, 1));
    dc.link(1, 2);
    EXPECT_TRUE(dc.are_connected(0, 2));
    EXPECT_TRUE(dc.are_connected(0, 1));
    EXPECT_TRUE(dc.are_connected(2, 1));
    dc.cut(0, 1);
    EXPECT_TRUE(dc.are_connected(0, 2));
    EXPECT_TRUE(dc.are_connected(0, 1));
    EXPECT_TRUE(dc.are_connected(2, 1));
    dc.cut(0, 2);
    EXPECT_FALSE(dc.are_connected(0, 2));
    EXPECT_FALSE(dc.are_connected(0, 1));
    EXPECT_TRUE(dc.are_connected(2, 1));
}

void test_dc_link_cut_random(size_t N, size_t M) {
    dynamic_connectivity real_dc(N);
    stupid_dc dc(N, false);
    std::set<std::pair<size_t, size_t>> edges;
    for (size_t i = 0; i < M; ++i) {
        switch (rand_size_t(3)) {
            case 0: {
                size_t u = rand_size_t(N);
                size_t v = rand_size_t(N);
                if (!dc.exists(u, v)) {
                    dc.link(u, v);
                    real_dc.link(u, v);
                    edges.insert({u, v});
                }
                break;
            }
            case 1: {
                if (edges.empty()) {
                    continue;
                }

                auto uv = std::vector<edge_t>(edges.begin(), edges.end())[rand_size_t(edges.size())];
                edges.erase(uv);
                real_dc.cut(uv.first, uv.second);
                dc.cut(uv.first, uv.second);
                break;
            }
            case 2: {
                size_t u = rand_size_t(N);
                size_t v = rand_size_t(N);
                EXPECT_EQ(real_dc.are_connected(u, v), dc.is_connected(u, v));
                break;
            }
            default:
                break;
        }
    }
}

TEST(dc, link_cut_random_v2_op1000000) {
    test_dc_link_cut_random(2, 1000000);
}

TEST(dc, link_cut_random_v3_op1000000) {
    test_dc_link_cut_random(3, 1000000);
}

TEST(dc, link_cut_random_v4_op1000000) {
    test_dc_link_cut_random(4, 1000000);
}

TEST(dc, link_cut_random_v5_op1000000) {
    test_dc_link_cut_random(5, 1000000);
}

TEST(dc, link_cut_random_v16_op1000000) {
    test_dc_link_cut_random(16, 1000000);
}

TEST(dc, link_cut_random_v100_op1000000) {
    test_dc_link_cut_random(100, 1000000);
}