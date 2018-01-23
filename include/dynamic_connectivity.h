#pragma once

#include <cstddef>
#include <stdexcept>
#include <functional>
#include <optional>
#include <random>
#include <unordered_map>
#include <fmt/format.h>

namespace dc {
    typedef std::pair<size_t, size_t> edge_t;

    static const size_t MAX_PRIORITY = 1UL << 30;

    size_t rand_size_t(size_t max) {
        static std::mt19937 mt19937(17023);
        static auto gen = std::bind(std::uniform_int_distribution<size_t>(), mt19937);
        return gen() % max;
    }

    struct info_t {
        info_t(bool on_last_level, bool has_aux)
                : on_last_level(on_last_level)
                , has_aux(has_aux) {}

        bool on_last_level;
        bool has_aux;

        info_t& operator|=(const info_t& b) {
            on_last_level |= b.on_last_level;
            has_aux |= b.has_aux;
            return *this;
        }
    };


    bool operator==(const info_t& a, const info_t& b) {
        return a.on_last_level == b.on_last_level && a.has_aux == b.has_aux;
    }

    struct treap_node {
        treap_node(size_t u) : treap_node(u, u) {};

        treap_node(size_t u, size_t v)
                : u(u)
                , v(v)
                , l(nullptr)
                , r(nullptr)
                , p(nullptr)
                , prior(rand_size_t(MAX_PRIORITY))
                , sz(1)
                , info(u != v, false)
                , best(info) {}

        size_t u, v;
        treap_node* l, * r, * p;
        size_t prior;
        size_t sz;
        info_t info;
        info_t best;

        bool is_vertex() {
            return u == v;
        }
    };

    static const info_t INF(false, false);

    size_t size(treap_node* node) {
        return node ? node->sz : 0;
    }

    info_t get_info(treap_node* node) {
        return node ? node->info : INF;
    }

    info_t get_best(treap_node* node) {
        return node ? node->best : INF;
    }

    void update_node_info(treap_node* node) {
        node->best = node->info;
        node->best |= get_best(node->l);
        node->best |= get_best(node->r);
    }

    void update(treap_node* node) {
        if (node) {
            if (node->l) node->l->p = node;
            if (node->r) node->r->p = node;
            node->p = nullptr;
            node->sz = size(node->l) + size(node->r) + 1;
            update_node_info(node);
        }
    }

    void update_info_up(treap_node* node) {
        while (node) {
            update_node_info(node);
            node = node->p;
        }
    }


    treap_node* merge(treap_node* l, treap_node* r) {
        if (!l) return r;
        if (!r) return l;
        if (l->prior >= r->prior) {
            l->r = merge(l->r, r);
            update(l);
            return l;
        } else {
            r->l = merge(l, r->l);
            update(r);
            return r;
        }
    }

    treap_node* merge_all(std::initializer_list<treap_node*> nodes) {
        treap_node* init = nullptr;
        return std::accumulate(nodes.begin(), nodes.end(), init, &merge);
    }

    size_t get_cnt(treap_node* v) {
        if (!v)
            return 0;
        size_t ans = 0;
        while (v) {
            ans += size(v->l) + 1;
            while (v->p && (v->p->l == v)) {
                v = v->p;
            }
            v = v->p;
        }
        return ans;
    }

    treap_node* get_root(treap_node* v) {
        while (v && v->p) {
            v = v->p;
        }
        return v;
    }

    treap_node* get_leftmost(treap_node* v) {
        while (v && v->l) {
            v = v->l;
        }
        return v;
    }

    treap_node* get_rightmost(treap_node* v) {
        while (v && v->r) {
            v = v->r;
        }
        return v;
    }

    void split_cnt(treap_node* v, treap_node*& l, treap_node*& r, size_t cnt) {
        if (v) {
            if (size(v->l) >= cnt) {
                split_cnt(v->l, l, v->l, cnt);
                update(v);
                r = v;
            } else {
                split_cnt(v->r, v->r, r, cnt - size(v->l) - 1);
                update(v);
                l = v;
            }
        } else {
            assert(cnt == 0);
            r = l = nullptr;
        }
    }

    void split(treap_node* v, treap_node*& l, treap_node*& r) {
        split_cnt(get_root(v), l, r, get_cnt(v));
    }

    treap_node* shift_to(treap_node* v) {
        treap_node* a;
        treap_node* b;
        split(v, a, b);
        return merge(b, a);
    }

    struct euler_tour_tree_exception
            : std::runtime_error {
        explicit euler_tour_tree_exception(const std::string& __arg) : runtime_error(__arg) {}
    };

    struct EdgeHash {
        int operator()(const edge_t& a) const {
            return static_cast<int>((a.first << 32) ^ a.second);
        }
    };

    struct dynamic_connectivity;
    struct aux_edge_handler;

    struct euler_tour_tree {
        friend dynamic_connectivity;
        friend aux_edge_handler;

        explicit euler_tour_tree(size_t cnt_vertices)
                : cnt_vertices_(cnt_vertices) {
            for (size_t i = 0; i < cnt_vertices_; ++i) {
                nodes_.push_back(new treap_node(i));
            }
        }

        ~euler_tour_tree() {
            for (auto node : nodes_) {
                delete node;
            }
            for (const auto& p : edge2node_) {
                delete p.second;
            }
        }

        void link(size_t u, size_t v, bool on_last_level = true) {
            check(u);
            check(v);
            if (are_connected(u, v)) {
                throw euler_tour_tree_exception("Linking connected vertices is unsupported operation");
            } else {
                auto uv = edge2node_[{u, v}] = new treap_node(u, v);
                auto vu = edge2node_[{v, u}] = new treap_node(v, u);
                uv->info.on_last_level = vu->info.on_last_level = on_last_level;
                merge_all({shift_to(nodes_[u]), uv, shift_to(nodes_[v]), vu});
            }
        }

        void cut(size_t u, size_t v) {
            check(u);
            check(v);
            if (!edge2node_.count({u, v})) {
                return;
            }
            auto uv = edge2node_[{u, v}];
            edge2node_.erase({u, v});
            auto vu = edge2node_[{v, u}];
            edge2node_.erase({v, u});
            auto treap = shift_to(uv);
            treap_node* a;
            treap_node* b;
            treap_node* c;
            treap_node* d;
            split_cnt(treap, treap, d, size(treap) - 1);
            split_cnt(treap, treap, c, get_cnt(vu));
            split_cnt(treap, a, b, size(treap) - 1);
            assert(d == uv);
            assert(b == vu);
            delete uv;
            delete vu;
        }

        bool are_connected(size_t u, size_t v) const {
            check(u);
            check(v);
            if (u == v) {
                return true;
            } else {
                return get_root(nodes_[u]) == get_root(nodes_[v]);
            }
        }

    private:
        inline void check(size_t u) const {
            if (u >= cnt_vertices_) {
                throw euler_tour_tree_exception(
                        fmt::format("Index of vertex {} must be less than {}", u, cnt_vertices_)
                );
            }
        }

        size_t cnt_vertices_;
        std::vector<treap_node*> nodes_;
        std::unordered_map<edge_t, treap_node*, EdgeHash> edge2node_;
    };

    struct dc_exception : public std::runtime_error {
        explicit dc_exception(const std::string& __arg) : runtime_error(__arg) {}
    };

    struct aux_edge_handler {
        explicit aux_edge_handler(euler_tour_tree& ett) : ett_(ett) {};

        void add_edge(size_t u, size_t v) {
            add_vertex_if_neccessary(u);
            edges[u].insert(v);
            add_vertex_if_neccessary(v);
            edges[v].insert(u);
        }

        void add_vertex_if_neccessary(size_t u) {
            if (!edges.count(u)) {
                ett_.nodes_[u]->info.has_aux = true;
                update_info_up(ett_.nodes_[u]);
            }
        }

        void remove_edge(size_t u, size_t v) {
            edges[u].erase(v);
            remove_vertex_if_neccessary(u);
            edges[v].erase(u);
            remove_vertex_if_neccessary(v);
        }

        void remove_vertex_if_neccessary(size_t u) {
            if (edges[u].size() == 0) {
                edges.erase(u);
                ett_.nodes_[u]->info.has_aux = false;
                update_info_up(ett_.nodes_[u]);
            }
        }

        std::unordered_map<size_t, std::unordered_set<size_t>> edges;
    private:
        euler_tour_tree& ett_;
    };

    struct dynamic_connectivity {
        explicit dynamic_connectivity(size_t cnt_vertices) : cnt_vertices_(cnt_vertices) {
            init_level(0);
        }

        void link(size_t u, size_t v) {
            check(u);
            check(v);
            if (u == v) {
                throw dc_exception("Loops are prohibited");
            }
            if (exists(u, v)) {
                throw dc_exception(fmt::format("Edge ({}, {}) already exists", u, v));
            }
            if (are_connected(u, v)) {
                aux_edge2layer_[{u, v}] = aux_edge2layer_[{v, u}] = 0;
                aux_edges_[0]->add_edge(u, v);
            } else {
                span_edge2layer_[{u, v}] = span_edge2layer_[{v, u}] = 0;
                etts_[0]->link(u, v);
            }
        }

        void cut(size_t u, size_t v) {
            check(u);
            check(v);
            if (u == v) {
                throw dc_exception("Loops are prohibited");
            }
            if (span_edge2layer_.count({u, v})) {
                remove_span_edge_from_level(u, v, span_edge2layer_[{u, v}]);
            } else if (aux_edge2layer_.count({u, v})) {
                size_t max_level = aux_edge2layer_[{u, v}];
                aux_edges_[max_level]->remove_edge(u, v);
                aux_edge2layer_.erase({u, v});
                aux_edge2layer_.erase({v, u});
            } else {
                throw dc_exception(fmt::format("Edge ({}, {}) does not exists", u, v));
            }
        }

        bool are_connected(size_t u, size_t v) const {
            check(u);
            check(v);
            return etts_[0]->are_connected(u, v);
        }

        bool exists(size_t u, size_t v) const {
            return span_edge2layer_.count({u, v}) || aux_edge2layer_.count({u, v});
        }

    private:
        void remove_span_edge_from_level(size_t u, size_t v, size_t level, std::optional<edge_t> edge = std::nullopt) {
            etts_[level]->cut(u, v);
            if (!edge) {
                treap_node* a = get_root(etts_[level]->nodes_[u]);
                treap_node* b = get_root(etts_[level]->nodes_[v]);
                assert(a != b);
                if (size(a) > size(b)) a = b;
                init_level(level + 1);
                up_span_from_level(a, level);
                edge = up_aux_from_level(a, a, level);
                if (edge) {
                    aux_edges_[level]->remove_edge(edge->first, edge->second);
                    aux_edge2layer_.erase({edge->first, edge->second});
                    aux_edge2layer_.erase({edge->second, edge->first});
                    span_edge2layer_[*edge] = span_edge2layer_[{edge->second, edge->first}] = level;
                    etts_[level]->link(edge->first, edge->second, true);
                }
            } else {
                etts_[level]->link(edge->first, edge->second, false);
            }
            if (level) {
                remove_span_edge_from_level(u, v, level - 1, edge);
            } else {
                span_edge2layer_.erase({u, v});
                span_edge2layer_.erase({v, u});
            }
        }

        std::optional<edge_t> up_aux_from_level(treap_node* v, treap_node* root, size_t level) {
            if (!v || !v->best.has_aux) return std::nullopt;
            std::optional < edge_t > ans;
            if (v->info.has_aux) {
                assert(v->v == v->u);
                std::vector<size_t> to_remove;
                for (size_t u : aux_edges_[level]->edges[v->v]) {
                    if (root == get_root(etts_[level]->nodes_[u])) {
                        to_remove.push_back(u);
                    } else {
                        ans = edge_t{v->v, u};
                        break;
                    }
                }
                for (size_t u : to_remove) {
                    aux_edges_[level]->remove_edge(v->v, u);
                    aux_edges_[level + 1]->add_edge(v->v, u);
                    ++aux_edge2layer_[{v->v, u}];
                    ++aux_edge2layer_[{u, v->v}];
                }
                if (ans) return ans;
            }
            if (ans = up_aux_from_level(v->l, root, level)) return ans;
            if (ans = up_aux_from_level(v->r, root, level)) return ans;
            return std::nullopt;
        }

        void up_span_from_level(treap_node* v, size_t level) {
            if (!v || !v->best.on_last_level) return;
            if (v->info.on_last_level) {
                up_span(v, level);
            }
            up_span_from_level(v->l, level);
            up_span_from_level(v->r, level);
            update_node_info(v);
        }

        void up_span(treap_node* v, size_t level) {
            assert(!v->is_vertex());
            ++(span_edge2layer_[{v->u, v->v}]);
            v->info.on_last_level = false;
            if (v->u < v->v) {
                etts_[level + 1]->link(v->u, v->v);
            }
        }

        void init_level(size_t level) {
            assert(level <= etts_.size());
            if (level == etts_.size()) {
                etts_.emplace_back(new euler_tour_tree(cnt_vertices_));
                aux_edges_.emplace_back(new aux_edge_handler(*(etts_[etts_.size() - 1])));
            }
        }

        inline void check(size_t u) const {
            if (u >= cnt_vertices_) {
                throw dc_exception(
                        fmt::format("Index of vertex {} must be less than {}", u, cnt_vertices_)
                );
            }
        }

        const size_t cnt_vertices_;
        std::vector<std::unique_ptr<euler_tour_tree>> etts_;
        std::unordered_map<edge_t, size_t, EdgeHash> span_edge2layer_;
        std::unordered_map<edge_t, size_t, EdgeHash> aux_edge2layer_;
        std::vector<std::unique_ptr<aux_edge_handler>> aux_edges_;
    };
}