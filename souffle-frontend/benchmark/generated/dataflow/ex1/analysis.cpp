
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
static const RamDomain RAM_BIT_SHIFT_MASK = RAM_DOMAIN_SIZE - 1;
struct t_btree_ui__0_1__11__10 {
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamUnsigned>(a[0]) < ramBitCast<RamUnsigned>(b[0])) ? -1 : (ramBitCast<RamUnsigned>(a[0]) > ramBitCast<RamUnsigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamUnsigned>(a[0]) < ramBitCast<RamUnsigned>(b[0]))|| (ramBitCast<RamUnsigned>(a[0]) == ramBitCast<RamUnsigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamUnsigned>(a[0]) == ramBitCast<RamUnsigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[2];
std::copy(ramDomain, ramDomain + 2, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1) {
RamDomain data[2] = {a0,a1};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_11(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_10(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 2 direct b-tree index 0 lex-order [0,1]\n";
ind_0.printStats(o);
}
};
struct t_btree_ii__0_1__11__10 {
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[2];
std::copy(ramDomain, ramDomain + 2, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1) {
RamDomain data[2] = {a0,a1};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_11(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_10(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 2 direct b-tree index 0 lex-order [0,1]\n";
ind_0.printStats(o);
}
};
struct t_btree_iii__0_1_2__110__111 {
using t_tuple = Tuple<RamDomain, 3>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2])) ? -1 : (ramBitCast<RamSigned>(a[2]) > ramBitCast<RamSigned>(b[2])) ? 1 :(0)));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1]))|| (ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1])) && ((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2]))));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]))&&(ramBitCast<RamSigned>(a[2]) == ramBitCast<RamSigned>(b[2]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[3];
std::copy(ramDomain, ramDomain + 3, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2) {
RamDomain data[3] = {a0,a1,a2};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_000(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_000(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_110(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_110(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_110(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_111(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_111(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_111(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 3 direct b-tree index 0 lex-order [0,1,2]\n";
ind_0.printStats(o);
}
};
struct t_btree_iiii__0_1_2_3__1110__1111 {
using t_tuple = Tuple<RamDomain, 4>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2])) ? -1 : (ramBitCast<RamSigned>(a[2]) > ramBitCast<RamSigned>(b[2])) ? 1 :((ramBitCast<RamSigned>(a[3]) < ramBitCast<RamSigned>(b[3])) ? -1 : (ramBitCast<RamSigned>(a[3]) > ramBitCast<RamSigned>(b[3])) ? 1 :(0))));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1]))|| (ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1])) && ((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2]))|| (ramBitCast<RamSigned>(a[2]) == ramBitCast<RamSigned>(b[2])) && ((ramBitCast<RamSigned>(a[3]) < ramBitCast<RamSigned>(b[3])))));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]))&&(ramBitCast<RamSigned>(a[2]) == ramBitCast<RamSigned>(b[2]))&&(ramBitCast<RamSigned>(a[3]) == ramBitCast<RamSigned>(b[3]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[4];
std::copy(ramDomain, ramDomain + 4, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2,RamDomain a3) {
RamDomain data[4] = {a0,a1,a2,a3};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_0000(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_0000(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_1110(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_1110(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_1110(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_1111(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_1111(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_1111(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 4 direct b-tree index 0 lex-order [0,1,2,3]\n";
ind_0.printStats(o);
}
};
struct t_btree_iii__0_2_1__100__101__111 {
using t_tuple = Tuple<RamDomain, 3>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2])) ? -1 : (ramBitCast<RamSigned>(a[2]) > ramBitCast<RamSigned>(b[2])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :(0)));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2]))|| (ramBitCast<RamSigned>(a[2]) == ramBitCast<RamSigned>(b[2])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1]))));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamSigned>(a[2]) == ramBitCast<RamSigned>(b[2]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[3];
std::copy(ramDomain, ramDomain + 3, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2) {
RamDomain data[3] = {a0,a1,a2};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_000(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_000(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_100(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_100(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_100(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_101(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_101(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_101(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_111(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_111(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_111(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 3 direct b-tree index 0 lex-order [0,2,1]\n";
ind_0.printStats(o);
}
};
struct t_btree_u__0__1 {
using t_tuple = Tuple<RamDomain, 1>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamUnsigned>(a[0]) < ramBitCast<RamUnsigned>(b[0])) ? -1 : (ramBitCast<RamUnsigned>(a[0]) > ramBitCast<RamUnsigned>(b[0])) ? 1 :(0);
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamUnsigned>(a[0]) < ramBitCast<RamUnsigned>(b[0]));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamUnsigned>(a[0]) == ramBitCast<RamUnsigned>(b[0]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[1];
std::copy(ramDomain, ramDomain + 1, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0) {
RamDomain data[1] = {a0};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_0(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_0(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_1(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 1 direct b-tree index 0 lex-order [0]\n";
ind_0.printStats(o);
}
};
struct t_btree_i__0__1 {
using t_tuple = Tuple<RamDomain, 1>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :(0);
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[1];
std::copy(ramDomain, ramDomain + 1, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0) {
RamDomain data[1] = {a0};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_0(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_0(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_1(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 1 direct b-tree index 0 lex-order [0]\n";
ind_0.printStats(o);
}
};
struct t_btree_iii__0_1_2__111 {
using t_tuple = Tuple<RamDomain, 3>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2])) ? -1 : (ramBitCast<RamSigned>(a[2]) > ramBitCast<RamSigned>(b[2])) ? 1 :(0)));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1]))|| (ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1])) && ((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2]))));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]))&&(ramBitCast<RamSigned>(a[2]) == ramBitCast<RamSigned>(b[2]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[3];
std::copy(ramDomain, ramDomain + 3, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2) {
RamDomain data[3] = {a0,a1,a2};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_000(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_000(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_111(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_111(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_111(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 3 direct b-tree index 0 lex-order [0,1,2]\n";
ind_0.printStats(o);
}
};
struct t_btree_iu__0_1__11__10 {
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamUnsigned>(a[1]) < ramBitCast<RamUnsigned>(b[1])) ? -1 : (ramBitCast<RamUnsigned>(a[1]) > ramBitCast<RamUnsigned>(b[1])) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamUnsigned>(a[1]) < ramBitCast<RamUnsigned>(b[1])));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamUnsigned>(a[1]) == ramBitCast<RamUnsigned>(b[1]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[2];
std::copy(ramDomain, ramDomain + 2, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1) {
RamDomain data[2] = {a0,a1};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_11(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_10(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 2 direct b-tree index 0 lex-order [0,1]\n";
ind_0.printStats(o);
}
};
struct t_btree_ii__0_1__11__12__10 {
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[2];
std::copy(ramDomain, ramDomain + 2, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1) {
RamDomain data[2] = {a0,a1};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_11(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_12(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_12(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_12(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_10(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 2 direct b-tree index 0 lex-order [0,1]\n";
ind_0.printStats(o);
}
};

class Sf_analysis : public SouffleProgram {
private:
static inline bool regex_wrapper(const std::string& pattern, const std::string& text) {
   bool result = false; 
   try { result = std::regex_match(text, std::regex(pattern)); } catch(...) { 
     std::cerr << "warning: wrong pattern provided for match(\"" << pattern << "\",\"" << text << "\").\n";
}
   return result;
}
private:
static inline std::string substr_wrapper(const std::string& str, size_t idx, size_t len) {
   std::string result; 
   try { result = str.substr(idx,len); } catch(...) { 
     std::cerr << "warning: wrong index position provided by substr(\"";
     std::cerr << str << "\"," << (int32_t)idx << "," << (int32_t)len << ") functor.\n";
   } return result;
}
public:
// -- initialize symbol table --
SymbolTable symTable;// -- initialize record table --
RecordTable recordTable;
// -- Table: +disconnected0
Own<t_nullaries> rel_1_disconnected0 = mk<t_nullaries>();
souffle::RelationWrapper<0,t_nullaries,Tuple<RamDomain,0>,0,0> wrapper_rel_1_disconnected0;
// -- Table: +disconnected1
Own<t_nullaries> rel_2_disconnected1 = mk<t_nullaries>();
souffle::RelationWrapper<1,t_nullaries,Tuple<RamDomain,0>,0,0> wrapper_rel_2_disconnected1;
// -- Table: +disconnected2
Own<t_nullaries> rel_3_disconnected2 = mk<t_nullaries>();
souffle::RelationWrapper<2,t_nullaries,Tuple<RamDomain,0>,0,0> wrapper_rel_3_disconnected2;
// -- Table: +disconnected3
Own<t_nullaries> rel_4_disconnected3 = mk<t_nullaries>();
souffle::RelationWrapper<3,t_nullaries,Tuple<RamDomain,0>,0,0> wrapper_rel_4_disconnected3;
// -- Table: +disconnected4
Own<t_nullaries> rel_5_disconnected4 = mk<t_nullaries>();
souffle::RelationWrapper<4,t_nullaries,Tuple<RamDomain,0>,0,0> wrapper_rel_5_disconnected4;
// -- Table: +disconnected5
Own<t_nullaries> rel_6_disconnected5 = mk<t_nullaries>();
souffle::RelationWrapper<5,t_nullaries,Tuple<RamDomain,0>,0,0> wrapper_rel_6_disconnected5;
// -- Table: +disconnected6
Own<t_nullaries> rel_7_disconnected6 = mk<t_nullaries>();
souffle::RelationWrapper<6,t_nullaries,Tuple<RamDomain,0>,0,0> wrapper_rel_7_disconnected6;
// -- Table: +disconnected7
Own<t_nullaries> rel_8_disconnected7 = mk<t_nullaries>();
souffle::RelationWrapper<7,t_nullaries,Tuple<RamDomain,0>,0,0> wrapper_rel_8_disconnected7;
// -- Table: @delta_+disconnected0
Own<t_nullaries> rel_9_delta_disconnected0 = mk<t_nullaries>();
// -- Table: @delta_+disconnected1
Own<t_nullaries> rel_10_delta_disconnected1 = mk<t_nullaries>();
// -- Table: @delta_+disconnected2
Own<t_nullaries> rel_11_delta_disconnected2 = mk<t_nullaries>();
// -- Table: @delta_+disconnected3
Own<t_nullaries> rel_12_delta_disconnected3 = mk<t_nullaries>();
// -- Table: @delta_+disconnected4
Own<t_nullaries> rel_13_delta_disconnected4 = mk<t_nullaries>();
// -- Table: @delta_+disconnected5
Own<t_nullaries> rel_14_delta_disconnected5 = mk<t_nullaries>();
// -- Table: @delta_+disconnected6
Own<t_nullaries> rel_15_delta_disconnected6 = mk<t_nullaries>();
// -- Table: @delta_+disconnected7
Own<t_nullaries> rel_16_delta_disconnected7 = mk<t_nullaries>();
// -- Table: @delta_VBool
Own<t_btree_ui__0_1__11__10> rel_17_delta_VBool = mk<t_btree_ui__0_1__11__10>();
// -- Table: @delta_VNum
Own<t_btree_ii__0_1__11__10> rel_18_delta_VNum = mk<t_btree_ii__0_1__11__10>();
// -- Table: @delta_add
Own<t_btree_iii__0_1_2__110__111> rel_19_delta_add = mk<t_btree_iii__0_1_2__110__111>();
// -- Table: @delta_aeval
Own<t_btree_iiii__0_1_2_3__1110__1111> rel_20_delta_aeval = mk<t_btree_iiii__0_1_2_3__1110__1111>();
// -- Table: @delta_exit_var
Own<t_btree_iiii__0_1_2_3__1110__1111> rel_21_delta_exit_var = mk<t_btree_iiii__0_1_2_3__1110__1111>();
// -- Table: @delta_final
Own<t_btree_ii__0_1__11__10> rel_22_delta_final = mk<t_btree_ii__0_1__11__10>();
// -- Table: @delta_flow
Own<t_btree_iii__0_2_1__100__101__111> rel_23_delta_flow = mk<t_btree_iii__0_2_1__100__101__111>();
// -- Table: @delta_freevars
Own<t_btree_ii__0_1__11__10> rel_24_delta_freevars = mk<t_btree_ii__0_1__11__10>();
// -- Table: @delta_freevarsStm
Own<t_btree_ii__0_1__11__10> rel_25_delta_freevarsStm = mk<t_btree_ii__0_1__11__10>();
// -- Table: @delta_greaterThan
Own<t_btree_iii__0_1_2__110__111> rel_26_delta_greaterThan = mk<t_btree_iii__0_1_2__110__111>();
// -- Table: @delta_init
Own<t_btree_ii__0_1__11__10> rel_27_delta_init = mk<t_btree_ii__0_1__11__10>();
// -- Table: @delta_input__VBool
Own<t_btree_u__0__1> rel_28_delta_input_VBool = mk<t_btree_u__0__1>();
// -- Table: @delta_input__VNum
Own<t_btree_i__0__1> rel_29_delta_input_VNum = mk<t_btree_i__0__1>();
// -- Table: @delta_input__aeval
Own<t_btree_iii__0_1_2__111> rel_30_delta_input_aeval = mk<t_btree_iii__0_1_2__111>();
// -- Table: @delta_input__entry_var
Own<t_btree_iii__0_1_2__111> rel_31_delta_input_entry_var = mk<t_btree_iii__0_1_2__111>();
// -- Table: @delta_input__exit_var
Own<t_btree_iii__0_1_2__111> rel_32_delta_input_exit_var = mk<t_btree_iii__0_1_2__111>();
// -- Table: @delta_input__final
Own<t_btree_i__0__1> rel_33_delta_input_final = mk<t_btree_i__0__1>();
// -- Table: @delta_input__flow
Own<t_btree_i__0__1> rel_34_delta_input_flow = mk<t_btree_i__0__1>();
// -- Table: @delta_input__freevars
Own<t_btree_i__0__1> rel_35_delta_input_freevars = mk<t_btree_i__0__1>();
// -- Table: @delta_input__freevarsStm
Own<t_btree_i__0__1> rel_36_delta_input_freevarsStm = mk<t_btree_i__0__1>();
// -- Table: @delta_input__init
Own<t_btree_i__0__1> rel_37_delta_input_init = mk<t_btree_i__0__1>();
// -- Table: @delta_un___VBool
Own<t_btree_iu__0_1__11__10> rel_38_delta_un_VBool = mk<t_btree_iu__0_1__11__10>();
// -- Table: @delta_un___VNum
Own<t_btree_ii__0_1__11__12__10> rel_39_delta_un_VNum = mk<t_btree_ii__0_1__11__12__10>();
// -- Table: @new_+disconnected0
Own<t_nullaries> rel_40_new_disconnected0 = mk<t_nullaries>();
// -- Table: @new_+disconnected1
Own<t_nullaries> rel_41_new_disconnected1 = mk<t_nullaries>();
// -- Table: @new_+disconnected2
Own<t_nullaries> rel_42_new_disconnected2 = mk<t_nullaries>();
// -- Table: @new_+disconnected3
Own<t_nullaries> rel_43_new_disconnected3 = mk<t_nullaries>();
// -- Table: @new_+disconnected4
Own<t_nullaries> rel_44_new_disconnected4 = mk<t_nullaries>();
// -- Table: @new_+disconnected5
Own<t_nullaries> rel_45_new_disconnected5 = mk<t_nullaries>();
// -- Table: @new_+disconnected6
Own<t_nullaries> rel_46_new_disconnected6 = mk<t_nullaries>();
// -- Table: @new_+disconnected7
Own<t_nullaries> rel_47_new_disconnected7 = mk<t_nullaries>();
// -- Table: @new_VBool
Own<t_btree_ui__0_1__11__10> rel_48_new_VBool = mk<t_btree_ui__0_1__11__10>();
// -- Table: @new_VNum
Own<t_btree_ii__0_1__11__10> rel_49_new_VNum = mk<t_btree_ii__0_1__11__10>();
// -- Table: @new_add
Own<t_btree_iii__0_1_2__110__111> rel_50_new_add = mk<t_btree_iii__0_1_2__110__111>();
// -- Table: @new_aeval
Own<t_btree_iiii__0_1_2_3__1110__1111> rel_51_new_aeval = mk<t_btree_iiii__0_1_2_3__1110__1111>();
// -- Table: @new_exit_var
Own<t_btree_iiii__0_1_2_3__1110__1111> rel_52_new_exit_var = mk<t_btree_iiii__0_1_2_3__1110__1111>();
// -- Table: @new_final
Own<t_btree_ii__0_1__11__10> rel_53_new_final = mk<t_btree_ii__0_1__11__10>();
// -- Table: @new_flow
Own<t_btree_iii__0_2_1__100__101__111> rel_54_new_flow = mk<t_btree_iii__0_2_1__100__101__111>();
// -- Table: @new_freevars
Own<t_btree_ii__0_1__11__10> rel_55_new_freevars = mk<t_btree_ii__0_1__11__10>();
// -- Table: @new_freevarsStm
Own<t_btree_ii__0_1__11__10> rel_56_new_freevarsStm = mk<t_btree_ii__0_1__11__10>();
// -- Table: @new_greaterThan
Own<t_btree_iii__0_1_2__110__111> rel_57_new_greaterThan = mk<t_btree_iii__0_1_2__110__111>();
// -- Table: @new_init
Own<t_btree_ii__0_1__11__10> rel_58_new_init = mk<t_btree_ii__0_1__11__10>();
// -- Table: @new_input__VBool
Own<t_btree_u__0__1> rel_59_new_input_VBool = mk<t_btree_u__0__1>();
// -- Table: @new_input__VNum
Own<t_btree_i__0__1> rel_60_new_input_VNum = mk<t_btree_i__0__1>();
// -- Table: @new_input__aeval
Own<t_btree_iii__0_1_2__111> rel_61_new_input_aeval = mk<t_btree_iii__0_1_2__111>();
// -- Table: @new_input__entry_var
Own<t_btree_iii__0_1_2__111> rel_62_new_input_entry_var = mk<t_btree_iii__0_1_2__111>();
// -- Table: @new_input__exit_var
Own<t_btree_iii__0_1_2__111> rel_63_new_input_exit_var = mk<t_btree_iii__0_1_2__111>();
// -- Table: @new_input__final
Own<t_btree_i__0__1> rel_64_new_input_final = mk<t_btree_i__0__1>();
// -- Table: @new_input__flow
Own<t_btree_i__0__1> rel_65_new_input_flow = mk<t_btree_i__0__1>();
// -- Table: @new_input__freevars
Own<t_btree_i__0__1> rel_66_new_input_freevars = mk<t_btree_i__0__1>();
// -- Table: @new_input__freevarsStm
Own<t_btree_i__0__1> rel_67_new_input_freevarsStm = mk<t_btree_i__0__1>();
// -- Table: @new_input__init
Own<t_btree_i__0__1> rel_68_new_input_init = mk<t_btree_i__0__1>();
// -- Table: @new_un___VBool
Own<t_btree_iu__0_1__11__10> rel_69_new_un_VBool = mk<t_btree_iu__0_1__11__10>();
// -- Table: @new_un___VNum
Own<t_btree_ii__0_1__11__12__10> rel_70_new_un_VNum = mk<t_btree_ii__0_1__11__12__10>();
// -- Table: VBool
Own<t_btree_ui__0_1__11__10> rel_71_VBool = mk<t_btree_ui__0_1__11__10>();
souffle::RelationWrapper<8,t_btree_ui__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_71_VBool;
// -- Table: VNum
Own<t_btree_ii__0_1__11__10> rel_72_VNum = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<9,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_72_VNum;
// -- Table: add
Own<t_btree_iii__0_1_2__110__111> rel_73_add = mk<t_btree_iii__0_1_2__110__111>();
souffle::RelationWrapper<10,t_btree_iii__0_1_2__110__111,Tuple<RamDomain,3>,3,0> wrapper_rel_73_add;
// -- Table: aeval
Own<t_btree_iiii__0_1_2_3__1110__1111> rel_74_aeval = mk<t_btree_iiii__0_1_2_3__1110__1111>();
souffle::RelationWrapper<11,t_btree_iiii__0_1_2_3__1110__1111,Tuple<RamDomain,4>,4,0> wrapper_rel_74_aeval;
// -- Table: exit_var
Own<t_btree_iiii__0_1_2_3__1110__1111> rel_75_exit_var = mk<t_btree_iiii__0_1_2_3__1110__1111>();
souffle::RelationWrapper<12,t_btree_iiii__0_1_2_3__1110__1111,Tuple<RamDomain,4>,4,0> wrapper_rel_75_exit_var;
// -- Table: ext_input__final_var
Own<t_btree_i__0__1> rel_76_ext_input_final_var = mk<t_btree_i__0__1>();
souffle::RelationWrapper<13,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_76_ext_input_final_var;
// -- Table: final
Own<t_btree_ii__0_1__11__10> rel_77_final = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<14,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_77_final;
// -- Table: final_var
Own<t_btree_iii__0_1_2__111> rel_78_final_var = mk<t_btree_iii__0_1_2__111>();
souffle::RelationWrapper<15,t_btree_iii__0_1_2__111,Tuple<RamDomain,3>,3,0> wrapper_rel_78_final_var;
// -- Table: flow
Own<t_btree_iii__0_2_1__100__101__111> rel_79_flow = mk<t_btree_iii__0_2_1__100__101__111>();
souffle::RelationWrapper<16,t_btree_iii__0_2_1__100__101__111,Tuple<RamDomain,3>,3,0> wrapper_rel_79_flow;
// -- Table: freevars
Own<t_btree_ii__0_1__11__10> rel_80_freevars = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<17,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_80_freevars;
// -- Table: freevarsStm
Own<t_btree_ii__0_1__11__10> rel_81_freevarsStm = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<18,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_81_freevarsStm;
// -- Table: greaterThan
Own<t_btree_iii__0_1_2__110__111> rel_82_greaterThan = mk<t_btree_iii__0_1_2__110__111>();
souffle::RelationWrapper<19,t_btree_iii__0_1_2__110__111,Tuple<RamDomain,3>,3,0> wrapper_rel_82_greaterThan;
// -- Table: hasType__Add
Own<t_btree_i__0__1> rel_83_hasType_Add = mk<t_btree_i__0__1>();
souffle::RelationWrapper<20,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_83_hasType_Add;
// -- Table: hasType__Assign
Own<t_btree_i__0__1> rel_84_hasType_Assign = mk<t_btree_i__0__1>();
souffle::RelationWrapper<21,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_84_hasType_Assign;
// -- Table: hasType__GreaterThan
Own<t_btree_i__0__1> rel_85_hasType_GreaterThan = mk<t_btree_i__0__1>();
souffle::RelationWrapper<22,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_85_hasType_GreaterThan;
// -- Table: hasType__If
Own<t_btree_i__0__1> rel_86_hasType_If = mk<t_btree_i__0__1>();
souffle::RelationWrapper<23,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_86_hasType_If;
// -- Table: hasType__Num
Own<t_btree_i__0__1> rel_87_hasType_Num = mk<t_btree_i__0__1>();
souffle::RelationWrapper<24,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_87_hasType_Num;
// -- Table: hasType__Sequence
Own<t_btree_i__0__1> rel_88_hasType_Sequence = mk<t_btree_i__0__1>();
souffle::RelationWrapper<25,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_88_hasType_Sequence;
// -- Table: hasType__Skip
Own<t_btree_i__0__1> rel_89_hasType_Skip = mk<t_btree_i__0__1>();
souffle::RelationWrapper<26,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_89_hasType_Skip;
// -- Table: hasType__VBool
Own<t_btree_i__0__1> rel_90_hasType_VBool = mk<t_btree_i__0__1>();
souffle::RelationWrapper<27,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_90_hasType_VBool;
// -- Table: hasType__VNum
Own<t_btree_i__0__1> rel_91_hasType_VNum = mk<t_btree_i__0__1>();
souffle::RelationWrapper<28,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_91_hasType_VNum;
// -- Table: hasType__Var
Own<t_btree_i__0__1> rel_92_hasType_Var = mk<t_btree_i__0__1>();
souffle::RelationWrapper<29,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_92_hasType_Var;
// -- Table: hasType__While
Own<t_btree_i__0__1> rel_93_hasType_While = mk<t_btree_i__0__1>();
souffle::RelationWrapper<30,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_93_hasType_While;
// -- Table: init
Own<t_btree_ii__0_1__11__10> rel_94_init = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<31,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_94_init;
// -- Table: input__VBool
Own<t_btree_u__0__1> rel_95_input_VBool = mk<t_btree_u__0__1>();
souffle::RelationWrapper<32,t_btree_u__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_95_input_VBool;
// -- Table: input__VNum
Own<t_btree_i__0__1> rel_96_input_VNum = mk<t_btree_i__0__1>();
souffle::RelationWrapper<33,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_96_input_VNum;
// -- Table: input__aeval
Own<t_btree_iii__0_1_2__111> rel_97_input_aeval = mk<t_btree_iii__0_1_2__111>();
souffle::RelationWrapper<34,t_btree_iii__0_1_2__111,Tuple<RamDomain,3>,3,0> wrapper_rel_97_input_aeval;
// -- Table: input__entry_var
Own<t_btree_iii__0_1_2__111> rel_98_input_entry_var = mk<t_btree_iii__0_1_2__111>();
souffle::RelationWrapper<35,t_btree_iii__0_1_2__111,Tuple<RamDomain,3>,3,0> wrapper_rel_98_input_entry_var;
// -- Table: input__exit_var
Own<t_btree_iii__0_1_2__111> rel_99_input_exit_var = mk<t_btree_iii__0_1_2__111>();
souffle::RelationWrapper<36,t_btree_iii__0_1_2__111,Tuple<RamDomain,3>,3,0> wrapper_rel_99_input_exit_var;
// -- Table: input__final
Own<t_btree_i__0__1> rel_100_input_final = mk<t_btree_i__0__1>();
souffle::RelationWrapper<37,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_100_input_final;
// -- Table: input__flow
Own<t_btree_i__0__1> rel_101_input_flow = mk<t_btree_i__0__1>();
souffle::RelationWrapper<38,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_101_input_flow;
// -- Table: input__freevars
Own<t_btree_i__0__1> rel_102_input_freevars = mk<t_btree_i__0__1>();
souffle::RelationWrapper<39,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_102_input_freevars;
// -- Table: input__freevarsStm
Own<t_btree_i__0__1> rel_103_input_freevarsStm = mk<t_btree_i__0__1>();
souffle::RelationWrapper<40,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_103_input_freevarsStm;
// -- Table: input__init
Own<t_btree_i__0__1> rel_104_input_init = mk<t_btree_i__0__1>();
souffle::RelationWrapper<41,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_104_input_init;
// -- Table: path__Add__0
Own<t_btree_ii__0_1__11__10> rel_105_path_Add_0 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<42,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_105_path_Add_0;
// -- Table: path__Add__1
Own<t_btree_ii__0_1__11__10> rel_106_path_Add_1 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<43,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_106_path_Add_1;
// -- Table: path__Assign__0
Own<t_btree_ii__0_1__11__10> rel_107_path_Assign_0 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<44,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_107_path_Assign_0;
// -- Table: path__Assign__1
Own<t_btree_ii__0_1__11__10> rel_108_path_Assign_1 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<45,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_108_path_Assign_1;
// -- Table: path__GreaterThan__0
Own<t_btree_ii__0_1__11__10> rel_109_path_GreaterThan_0 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<46,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_109_path_GreaterThan_0;
// -- Table: path__GreaterThan__1
Own<t_btree_ii__0_1__11__10> rel_110_path_GreaterThan_1 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<47,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_110_path_GreaterThan_1;
// -- Table: path__If__0
Own<t_btree_ii__0_1__11__10> rel_111_path_If_0 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<48,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_111_path_If_0;
// -- Table: path__If__1
Own<t_btree_ii__0_1__11__10> rel_112_path_If_1 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<49,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_112_path_If_1;
// -- Table: path__If__2
Own<t_btree_ii__0_1__11__10> rel_113_path_If_2 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<50,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_113_path_If_2;
// -- Table: path__Num__0
Own<t_btree_ii__0_1__11__10> rel_114_path_Num_0 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<51,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_114_path_Num_0;
// -- Table: path__Sequence__0
Own<t_btree_ii__0_1__11__10> rel_115_path_Sequence_0 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<52,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_115_path_Sequence_0;
// -- Table: path__Sequence__1
Own<t_btree_ii__0_1__11__10> rel_116_path_Sequence_1 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<53,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_116_path_Sequence_1;
// -- Table: path__VBool__0
Own<t_btree_iu__0_1__11__10> rel_117_path_VBool_0 = mk<t_btree_iu__0_1__11__10>();
souffle::RelationWrapper<54,t_btree_iu__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_117_path_VBool_0;
// -- Table: path__VNum__0
Own<t_btree_ii__0_1__11__10> rel_118_path_VNum_0 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<55,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_118_path_VNum_0;
// -- Table: path__Var__0
Own<t_btree_ii__0_1__11__10> rel_119_path_Var_0 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<56,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_119_path_Var_0;
// -- Table: path__While__0
Own<t_btree_ii__0_1__11__10> rel_120_path_While_0 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<57,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_120_path_While_0;
// -- Table: path__While__1
Own<t_btree_ii__0_1__11__10> rel_121_path_While_1 = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<58,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_121_path_While_1;
// -- Table: un___VBool
Own<t_btree_iu__0_1__11__10> rel_122_un_VBool = mk<t_btree_iu__0_1__11__10>();
souffle::RelationWrapper<59,t_btree_iu__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_122_un_VBool;
// -- Table: un___VNum
Own<t_btree_ii__0_1__11__12__10> rel_123_un_VNum = mk<t_btree_ii__0_1__11__12__10>();
souffle::RelationWrapper<60,t_btree_ii__0_1__11__12__10,Tuple<RamDomain,2>,2,0> wrapper_rel_123_un_VNum;
public:
Sf_analysis() : 
wrapper_rel_1_disconnected0(*rel_1_disconnected0,symTable,"+disconnected0",std::array<const char *,0>{{}},std::array<const char *,0>{{}}),

wrapper_rel_2_disconnected1(*rel_2_disconnected1,symTable,"+disconnected1",std::array<const char *,0>{{}},std::array<const char *,0>{{}}),

wrapper_rel_3_disconnected2(*rel_3_disconnected2,symTable,"+disconnected2",std::array<const char *,0>{{}},std::array<const char *,0>{{}}),

wrapper_rel_4_disconnected3(*rel_4_disconnected3,symTable,"+disconnected3",std::array<const char *,0>{{}},std::array<const char *,0>{{}}),

wrapper_rel_5_disconnected4(*rel_5_disconnected4,symTable,"+disconnected4",std::array<const char *,0>{{}},std::array<const char *,0>{{}}),

wrapper_rel_6_disconnected5(*rel_6_disconnected5,symTable,"+disconnected5",std::array<const char *,0>{{}},std::array<const char *,0>{{}}),

wrapper_rel_7_disconnected6(*rel_7_disconnected6,symTable,"+disconnected6",std::array<const char *,0>{{}},std::array<const char *,0>{{}}),

wrapper_rel_8_disconnected7(*rel_8_disconnected7,symTable,"+disconnected7",std::array<const char *,0>{{}},std::array<const char *,0>{{}}),

wrapper_rel_71_VBool(*rel_71_VBool,symTable,"VBool",std::array<const char *,2>{{"u:unsigned","+:Val"}},std::array<const char *,2>{{"_0","out"}}),

wrapper_rel_72_VNum(*rel_72_VNum,symTable,"VNum",std::array<const char *,2>{{"i:number","+:Val"}},std::array<const char *,2>{{"_0","out"}}),

wrapper_rel_73_add(*rel_73_add,symTable,"add",std::array<const char *,3>{{"+:Val","+:Val","+:Val"}},std::array<const char *,3>{{"v1","v2","out__0"}}),

wrapper_rel_74_aeval(*rel_74_aeval,symTable,"aeval",std::array<const char *,4>{{"+:Exp","+:Stm","+:Stm","+:Val"}},std::array<const char *,4>{{"exp","node","prog","out__0"}}),

wrapper_rel_75_exit_var(*rel_75_exit_var,symTable,"exit_var",std::array<const char *,4>{{"+:Stm","+:Stm","s:symbol","+:Val"}},std::array<const char *,4>{{"stm","prog","x","out__0"}}),

wrapper_rel_76_ext_input_final_var(*rel_76_ext_input_final_var,symTable,"ext_input__final_var",std::array<const char *,1>{{"+:Stm"}},std::array<const char *,1>{{"prog"}}),

wrapper_rel_77_final(*rel_77_final,symTable,"final",std::array<const char *,2>{{"+:Stm","+:Stm"}},std::array<const char *,2>{{"stm","out__0"}}),

wrapper_rel_78_final_var(*rel_78_final_var,symTable,"final_var",std::array<const char *,3>{{"+:Stm","s:symbol","+:Val"}},std::array<const char *,3>{{"prog","out_0__0","out_1__0"}}),

wrapper_rel_79_flow(*rel_79_flow,symTable,"flow",std::array<const char *,3>{{"+:Stm","+:Stm","+:Stm"}},std::array<const char *,3>{{"stm","out_0__0","out_1__0"}}),

wrapper_rel_80_freevars(*rel_80_freevars,symTable,"freevars",std::array<const char *,2>{{"+:Exp","s:symbol"}},std::array<const char *,2>{{"exp","out__0"}}),

wrapper_rel_81_freevarsStm(*rel_81_freevarsStm,symTable,"freevarsStm",std::array<const char *,2>{{"+:Stm","s:symbol"}},std::array<const char *,2>{{"stm","out__0"}}),

wrapper_rel_82_greaterThan(*rel_82_greaterThan,symTable,"greaterThan",std::array<const char *,3>{{"+:Val","+:Val","+:Val"}},std::array<const char *,3>{{"v1","v2","out__0"}}),

wrapper_rel_83_hasType_Add(*rel_83_hasType_Add,symTable,"hasType__Add",std::array<const char *,1>{{"+:Exp"}},std::array<const char *,1>{{"out"}}),

wrapper_rel_84_hasType_Assign(*rel_84_hasType_Assign,symTable,"hasType__Assign",std::array<const char *,1>{{"+:Stm"}},std::array<const char *,1>{{"out"}}),

wrapper_rel_85_hasType_GreaterThan(*rel_85_hasType_GreaterThan,symTable,"hasType__GreaterThan",std::array<const char *,1>{{"+:Exp"}},std::array<const char *,1>{{"out"}}),

wrapper_rel_86_hasType_If(*rel_86_hasType_If,symTable,"hasType__If",std::array<const char *,1>{{"+:Stm"}},std::array<const char *,1>{{"out"}}),

wrapper_rel_87_hasType_Num(*rel_87_hasType_Num,symTable,"hasType__Num",std::array<const char *,1>{{"+:Exp"}},std::array<const char *,1>{{"out"}}),

wrapper_rel_88_hasType_Sequence(*rel_88_hasType_Sequence,symTable,"hasType__Sequence",std::array<const char *,1>{{"+:Stm"}},std::array<const char *,1>{{"out"}}),

wrapper_rel_89_hasType_Skip(*rel_89_hasType_Skip,symTable,"hasType__Skip",std::array<const char *,1>{{"+:Stm"}},std::array<const char *,1>{{"out"}}),

wrapper_rel_90_hasType_VBool(*rel_90_hasType_VBool,symTable,"hasType__VBool",std::array<const char *,1>{{"+:Val"}},std::array<const char *,1>{{"out"}}),

wrapper_rel_91_hasType_VNum(*rel_91_hasType_VNum,symTable,"hasType__VNum",std::array<const char *,1>{{"+:Val"}},std::array<const char *,1>{{"out"}}),

wrapper_rel_92_hasType_Var(*rel_92_hasType_Var,symTable,"hasType__Var",std::array<const char *,1>{{"+:Exp"}},std::array<const char *,1>{{"out"}}),

wrapper_rel_93_hasType_While(*rel_93_hasType_While,symTable,"hasType__While",std::array<const char *,1>{{"+:Stm"}},std::array<const char *,1>{{"out"}}),

wrapper_rel_94_init(*rel_94_init,symTable,"init",std::array<const char *,2>{{"+:Stm","+:Stm"}},std::array<const char *,2>{{"stm","out__0"}}),

wrapper_rel_95_input_VBool(*rel_95_input_VBool,symTable,"input__VBool",std::array<const char *,1>{{"u:unsigned"}},std::array<const char *,1>{{"_0__0"}}),

wrapper_rel_96_input_VNum(*rel_96_input_VNum,symTable,"input__VNum",std::array<const char *,1>{{"i:number"}},std::array<const char *,1>{{"_0__0"}}),

wrapper_rel_97_input_aeval(*rel_97_input_aeval,symTable,"input__aeval",std::array<const char *,3>{{"+:Exp","+:Stm","+:Stm"}},std::array<const char *,3>{{"exp__0","node__0","prog__0"}}),

wrapper_rel_98_input_entry_var(*rel_98_input_entry_var,symTable,"input__entry_var",std::array<const char *,3>{{"+:Stm","+:Stm","s:symbol"}},std::array<const char *,3>{{"stm__0","prog__0","x__0"}}),

wrapper_rel_99_input_exit_var(*rel_99_input_exit_var,symTable,"input__exit_var",std::array<const char *,3>{{"+:Stm","+:Stm","s:symbol"}},std::array<const char *,3>{{"stm__0","prog__0","x__0"}}),

wrapper_rel_100_input_final(*rel_100_input_final,symTable,"input__final",std::array<const char *,1>{{"+:Stm"}},std::array<const char *,1>{{"stm__0"}}),

wrapper_rel_101_input_flow(*rel_101_input_flow,symTable,"input__flow",std::array<const char *,1>{{"+:Stm"}},std::array<const char *,1>{{"stm__0"}}),

wrapper_rel_102_input_freevars(*rel_102_input_freevars,symTable,"input__freevars",std::array<const char *,1>{{"+:Exp"}},std::array<const char *,1>{{"exp__0"}}),

wrapper_rel_103_input_freevarsStm(*rel_103_input_freevarsStm,symTable,"input__freevarsStm",std::array<const char *,1>{{"+:Stm"}},std::array<const char *,1>{{"stm__0"}}),

wrapper_rel_104_input_init(*rel_104_input_init,symTable,"input__init",std::array<const char *,1>{{"+:Stm"}},std::array<const char *,1>{{"stm__0"}}),

wrapper_rel_105_path_Add_0(*rel_105_path_Add_0,symTable,"path__Add__0",std::array<const char *,2>{{"+:Exp","+:Exp"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_106_path_Add_1(*rel_106_path_Add_1,symTable,"path__Add__1",std::array<const char *,2>{{"+:Exp","+:Exp"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_107_path_Assign_0(*rel_107_path_Assign_0,symTable,"path__Assign__0",std::array<const char *,2>{{"+:Stm","s:symbol"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_108_path_Assign_1(*rel_108_path_Assign_1,symTable,"path__Assign__1",std::array<const char *,2>{{"+:Stm","+:Exp"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_109_path_GreaterThan_0(*rel_109_path_GreaterThan_0,symTable,"path__GreaterThan__0",std::array<const char *,2>{{"+:Exp","+:Exp"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_110_path_GreaterThan_1(*rel_110_path_GreaterThan_1,symTable,"path__GreaterThan__1",std::array<const char *,2>{{"+:Exp","+:Exp"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_111_path_If_0(*rel_111_path_If_0,symTable,"path__If__0",std::array<const char *,2>{{"+:Stm","+:Exp"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_112_path_If_1(*rel_112_path_If_1,symTable,"path__If__1",std::array<const char *,2>{{"+:Stm","+:Stm"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_113_path_If_2(*rel_113_path_If_2,symTable,"path__If__2",std::array<const char *,2>{{"+:Stm","+:Stm"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_114_path_Num_0(*rel_114_path_Num_0,symTable,"path__Num__0",std::array<const char *,2>{{"+:Exp","i:number"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_115_path_Sequence_0(*rel_115_path_Sequence_0,symTable,"path__Sequence__0",std::array<const char *,2>{{"+:Stm","+:Stm"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_116_path_Sequence_1(*rel_116_path_Sequence_1,symTable,"path__Sequence__1",std::array<const char *,2>{{"+:Stm","+:Stm"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_117_path_VBool_0(*rel_117_path_VBool_0,symTable,"path__VBool__0",std::array<const char *,2>{{"+:Val","u:unsigned"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_118_path_VNum_0(*rel_118_path_VNum_0,symTable,"path__VNum__0",std::array<const char *,2>{{"+:Val","i:number"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_119_path_Var_0(*rel_119_path_Var_0,symTable,"path__Var__0",std::array<const char *,2>{{"+:Exp","s:symbol"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_120_path_While_0(*rel_120_path_While_0,symTable,"path__While__0",std::array<const char *,2>{{"+:Stm","+:Exp"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_121_path_While_1(*rel_121_path_While_1,symTable,"path__While__1",std::array<const char *,2>{{"+:Stm","+:Stm"}},std::array<const char *,2>{{"out","field"}}),

wrapper_rel_122_un_VBool(*rel_122_un_VBool,symTable,"un___VBool",std::array<const char *,2>{{"+:Val","u:unsigned"}},std::array<const char *,2>{{"out","_0"}}),

wrapper_rel_123_un_VNum(*rel_123_un_VNum,symTable,"un___VNum",std::array<const char *,2>{{"+:Val","i:number"}},std::array<const char *,2>{{"out","_0"}}){
addRelation("+disconnected0",&wrapper_rel_1_disconnected0,false,false);
addRelation("+disconnected1",&wrapper_rel_2_disconnected1,false,false);
addRelation("+disconnected2",&wrapper_rel_3_disconnected2,false,false);
addRelation("+disconnected3",&wrapper_rel_4_disconnected3,false,false);
addRelation("+disconnected4",&wrapper_rel_5_disconnected4,false,false);
addRelation("+disconnected5",&wrapper_rel_6_disconnected5,false,false);
addRelation("+disconnected6",&wrapper_rel_7_disconnected6,false,false);
addRelation("+disconnected7",&wrapper_rel_8_disconnected7,false,false);
addRelation("VBool",&wrapper_rel_71_VBool,false,false);
addRelation("VNum",&wrapper_rel_72_VNum,false,false);
addRelation("add",&wrapper_rel_73_add,false,false);
addRelation("aeval",&wrapper_rel_74_aeval,false,false);
addRelation("exit_var",&wrapper_rel_75_exit_var,false,false);
addRelation("ext_input__final_var",&wrapper_rel_76_ext_input_final_var,true,false);
addRelation("final",&wrapper_rel_77_final,false,false);
addRelation("final_var",&wrapper_rel_78_final_var,false,true);
addRelation("flow",&wrapper_rel_79_flow,false,false);
addRelation("freevars",&wrapper_rel_80_freevars,false,false);
addRelation("freevarsStm",&wrapper_rel_81_freevarsStm,false,false);
addRelation("greaterThan",&wrapper_rel_82_greaterThan,false,false);
addRelation("hasType__Add",&wrapper_rel_83_hasType_Add,true,false);
addRelation("hasType__Assign",&wrapper_rel_84_hasType_Assign,true,false);
addRelation("hasType__GreaterThan",&wrapper_rel_85_hasType_GreaterThan,true,false);
addRelation("hasType__If",&wrapper_rel_86_hasType_If,true,false);
addRelation("hasType__Num",&wrapper_rel_87_hasType_Num,true,false);
addRelation("hasType__Sequence",&wrapper_rel_88_hasType_Sequence,true,false);
addRelation("hasType__Skip",&wrapper_rel_89_hasType_Skip,true,false);
addRelation("hasType__VBool",&wrapper_rel_90_hasType_VBool,true,false);
addRelation("hasType__VNum",&wrapper_rel_91_hasType_VNum,true,false);
addRelation("hasType__Var",&wrapper_rel_92_hasType_Var,true,false);
addRelation("hasType__While",&wrapper_rel_93_hasType_While,true,false);
addRelation("init",&wrapper_rel_94_init,false,false);
addRelation("input__VBool",&wrapper_rel_95_input_VBool,false,false);
addRelation("input__VNum",&wrapper_rel_96_input_VNum,false,false);
addRelation("input__aeval",&wrapper_rel_97_input_aeval,false,false);
addRelation("input__entry_var",&wrapper_rel_98_input_entry_var,false,false);
addRelation("input__exit_var",&wrapper_rel_99_input_exit_var,false,false);
addRelation("input__final",&wrapper_rel_100_input_final,false,false);
addRelation("input__flow",&wrapper_rel_101_input_flow,false,false);
addRelation("input__freevars",&wrapper_rel_102_input_freevars,false,false);
addRelation("input__freevarsStm",&wrapper_rel_103_input_freevarsStm,false,false);
addRelation("input__init",&wrapper_rel_104_input_init,false,false);
addRelation("path__Add__0",&wrapper_rel_105_path_Add_0,true,false);
addRelation("path__Add__1",&wrapper_rel_106_path_Add_1,true,false);
addRelation("path__Assign__0",&wrapper_rel_107_path_Assign_0,true,false);
addRelation("path__Assign__1",&wrapper_rel_108_path_Assign_1,true,false);
addRelation("path__GreaterThan__0",&wrapper_rel_109_path_GreaterThan_0,true,false);
addRelation("path__GreaterThan__1",&wrapper_rel_110_path_GreaterThan_1,true,false);
addRelation("path__If__0",&wrapper_rel_111_path_If_0,true,false);
addRelation("path__If__1",&wrapper_rel_112_path_If_1,true,false);
addRelation("path__If__2",&wrapper_rel_113_path_If_2,true,false);
addRelation("path__Num__0",&wrapper_rel_114_path_Num_0,true,false);
addRelation("path__Sequence__0",&wrapper_rel_115_path_Sequence_0,true,false);
addRelation("path__Sequence__1",&wrapper_rel_116_path_Sequence_1,true,false);
addRelation("path__VBool__0",&wrapper_rel_117_path_VBool_0,true,false);
addRelation("path__VNum__0",&wrapper_rel_118_path_VNum_0,true,false);
addRelation("path__Var__0",&wrapper_rel_119_path_Var_0,true,false);
addRelation("path__While__0",&wrapper_rel_120_path_While_0,true,false);
addRelation("path__While__1",&wrapper_rel_121_path_While_1,true,false);
addRelation("un___VBool",&wrapper_rel_122_un_VBool,false,false);
addRelation("un___VNum",&wrapper_rel_123_un_VNum,false,false);
}
~Sf_analysis() {
}
private:
std::string inputDirectory;
std::string outputDirectory;
bool performIO;
std::atomic<RamDomain> ctr{};

std::atomic<size_t> iter{};
void runFunction(std::string inputDirectoryArg = "", std::string outputDirectoryArg = "", bool performIOArg = false) {
this->inputDirectory = inputDirectoryArg;
this->outputDirectory = outputDirectoryArg;
this->performIO = performIOArg;
SignalHandler::instance()->set();
#if defined(_OPENMP)
if (getNumThreads() > 0) {omp_set_num_threads(getNumThreads());}
#endif

// -- query evaluation --
{
 std::vector<RamDomain> args, ret;
subroutine_0(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_1(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_12(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_23(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_25(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_26(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_27(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_28(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_29(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_30(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_2(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_3(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_4(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_5(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_6(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_7(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_8(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_9(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_10(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_11(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_13(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_14(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_15(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_16(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_17(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_18(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_19(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_20(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_21(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_22(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_24(args, ret);
}

// -- relation hint statistics --
SignalHandler::instance()->reset();
}
public:
void run() override { runFunction("", "", false); }
public:
void runAll(std::string inputDirectoryArg = "", std::string outputDirectoryArg = "") override { runFunction(inputDirectoryArg, outputDirectoryArg, true);
}
public:
void printAll(std::string outputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","prog\tout_0__0\tout_1__0"},{"name","final_var"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"prog\", \"out_0__0\", \"out_1__0\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"+:Stm\", \"s:symbol\", \"+:Val\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_78_final_var);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__If"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_86_hasType_If);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Add__1"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"+:Exp\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_106_path_Add_1);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Add__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"+:Exp\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_105_path_Add_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__Assign"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_84_hasType_Assign);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Num__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"i:number\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_114_path_Num_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","prog"},{"fact-dir","generated/dataflow/ex1"},{"name","ext_input__final_var"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"prog\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_76_ext_input_final_var);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__Sequence"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_88_hasType_Sequence);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__Skip"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_89_hasType_Skip);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__While"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_93_hasType_While);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__Var"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Exp\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_92_hasType_Var);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__Add"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Exp\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_83_hasType_Add);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__VNum"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Val\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_91_hasType_VNum);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__Num"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Exp\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_87_hasType_Num);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__GreaterThan__1"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"+:Exp\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_110_path_GreaterThan_1);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__GreaterThan"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Exp\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_85_hasType_GreaterThan);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__VBool"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Val\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_90_hasType_VBool);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__VNum__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Val\", \"i:number\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_118_path_VNum_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__If__2"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_113_path_If_2);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__GreaterThan__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"+:Exp\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_109_path_GreaterThan_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Assign__1"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Exp\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_108_path_Assign_1);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__While__1"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_121_path_While_1);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__If__1"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_112_path_If_1);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Sequence__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_115_path_Sequence_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__VBool__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Val\", \"u:unsigned\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_117_path_VBool_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Sequence__1"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_116_path_Sequence_1);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__If__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Exp\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_111_path_If_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__While__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Exp\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_120_path_While_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Var__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"s:symbol\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_119_path_Var_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Assign__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"s:symbol\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_107_path_Assign_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
public:
void dumpInputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "hasType__If";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_86_hasType_If);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__Add__1";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"+:Exp\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_106_path_Add_1);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__Add__0";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"+:Exp\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_105_path_Add_0);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "hasType__Assign";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_84_hasType_Assign);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__Num__0";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"i:number\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_114_path_Num_0);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "ext_input__final_var";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_76_ext_input_final_var);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "hasType__Sequence";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_88_hasType_Sequence);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "hasType__Skip";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_89_hasType_Skip);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "hasType__While";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_93_hasType_While);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "hasType__Var";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Exp\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_92_hasType_Var);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "hasType__Add";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Exp\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_83_hasType_Add);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "hasType__VNum";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Val\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_91_hasType_VNum);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "hasType__Num";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Exp\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_87_hasType_Num);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__GreaterThan__1";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"+:Exp\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_110_path_GreaterThan_1);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "hasType__GreaterThan";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Exp\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_85_hasType_GreaterThan);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "hasType__VBool";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Val\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_90_hasType_VBool);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__VNum__0";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Val\", \"i:number\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_118_path_VNum_0);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__If__2";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_113_path_If_2);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__GreaterThan__0";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"+:Exp\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_109_path_GreaterThan_0);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__Assign__1";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Exp\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_108_path_Assign_1);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__While__1";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_121_path_While_1);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__If__1";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_112_path_If_1);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__Sequence__0";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_115_path_Sequence_0);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__VBool__0";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Val\", \"u:unsigned\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_117_path_VBool_0);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__Sequence__1";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_116_path_Sequence_1);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__If__0";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Exp\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_111_path_If_0);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__While__0";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Exp\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_120_path_While_0);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__Var__0";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_119_path_Var_0);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "path__Assign__0";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_107_path_Assign_0);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void dumpOutputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "final_var";
rwOperation["types"] = "{\"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"+:Stm\", \"s:symbol\", \"+:Val\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_78_final_var);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
SymbolTable& getSymbolTable() override {
return symTable;
}
void executeSubroutine(std::string name, const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) override {
if (name == "stratum_0") {
subroutine_0(args, ret);
return;}
if (name == "stratum_1") {
subroutine_1(args, ret);
return;}
if (name == "stratum_10") {
subroutine_2(args, ret);
return;}
if (name == "stratum_11") {
subroutine_3(args, ret);
return;}
if (name == "stratum_12") {
subroutine_4(args, ret);
return;}
if (name == "stratum_13") {
subroutine_5(args, ret);
return;}
if (name == "stratum_14") {
subroutine_6(args, ret);
return;}
if (name == "stratum_15") {
subroutine_7(args, ret);
return;}
if (name == "stratum_16") {
subroutine_8(args, ret);
return;}
if (name == "stratum_17") {
subroutine_9(args, ret);
return;}
if (name == "stratum_18") {
subroutine_10(args, ret);
return;}
if (name == "stratum_19") {
subroutine_11(args, ret);
return;}
if (name == "stratum_2") {
subroutine_12(args, ret);
return;}
if (name == "stratum_20") {
subroutine_13(args, ret);
return;}
if (name == "stratum_21") {
subroutine_14(args, ret);
return;}
if (name == "stratum_22") {
subroutine_15(args, ret);
return;}
if (name == "stratum_23") {
subroutine_16(args, ret);
return;}
if (name == "stratum_24") {
subroutine_17(args, ret);
return;}
if (name == "stratum_25") {
subroutine_18(args, ret);
return;}
if (name == "stratum_26") {
subroutine_19(args, ret);
return;}
if (name == "stratum_27") {
subroutine_20(args, ret);
return;}
if (name == "stratum_28") {
subroutine_21(args, ret);
return;}
if (name == "stratum_29") {
subroutine_22(args, ret);
return;}
if (name == "stratum_3") {
subroutine_23(args, ret);
return;}
if (name == "stratum_30") {
subroutine_24(args, ret);
return;}
if (name == "stratum_4") {
subroutine_25(args, ret);
return;}
if (name == "stratum_5") {
subroutine_26(args, ret);
return;}
if (name == "stratum_6") {
subroutine_27(args, ret);
return;}
if (name == "stratum_7") {
subroutine_28(args, ret);
return;}
if (name == "stratum_8") {
subroutine_29(args, ret);
return;}
if (name == "stratum_9") {
subroutine_30(args, ret);
return;}
fatal("unknown subroutine");
}
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_0(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__Assign"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_84_hasType_Assign);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_1(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__If"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_86_hasType_If);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_2(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__VNum"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Val\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_91_hasType_VNum);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_3(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__VNum__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Val\", \"i:number\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_118_path_VNum_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_4(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__Num"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Exp\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_87_hasType_Num);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_5(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Num__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"i:number\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_114_path_Num_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_6(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__GreaterThan"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Exp\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_85_hasType_GreaterThan);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_7(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__GreaterThan__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"+:Exp\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_109_path_GreaterThan_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_8(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__GreaterThan__1"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"+:Exp\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_110_path_GreaterThan_1);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_9(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__VBool"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Val\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_90_hasType_VBool);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_10(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__VBool__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Val\", \"u:unsigned\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_117_path_VBool_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_11(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__If__1"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_112_path_If_1);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_12(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__Sequence"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_88_hasType_Sequence);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_13(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__If__2"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_113_path_If_2);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_14(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Sequence__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_115_path_Sequence_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_15(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Sequence__1"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_116_path_Sequence_1);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_16(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__While__1"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Stm\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_121_path_While_1);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_17(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Assign__1"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Exp\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_108_path_Assign_1);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_18(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__If__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Exp\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_111_path_If_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_19(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__While__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"+:Exp\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_120_path_While_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_20(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Var__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"s:symbol\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_119_path_Var_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_21(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Assign__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Stm\", \"s:symbol\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_107_path_Assign_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_22(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
[&](){
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
for(const auto& env0 : *rel_77_final) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_22_delta_final->insert(tuple,READ_OP_CONTEXT(rel_22_delta_final_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_94_init_op_ctxt,rel_94_init->createContext());
CREATE_OP_CONTEXT(rel_27_delta_init_op_ctxt,rel_27_delta_init->createContext());
for(const auto& env0 : *rel_94_init) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_27_delta_init->insert(tuple,READ_OP_CONTEXT(rel_27_delta_init_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
for(const auto& env0 : *rel_80_freevars) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_24_delta_freevars->insert(tuple,READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
for(const auto& env0 : *rel_79_flow) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_23_delta_flow->insert(tuple,READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt,rel_25_delta_freevarsStm->createContext());
for(const auto& env0 : *rel_81_freevarsStm) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_25_delta_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
for(const auto& env0 : *rel_71_VBool) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_17_delta_VBool->insert(tuple,READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
for(const auto& env0 : *rel_75_exit_var) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env0[3])}};
rel_21_delta_exit_var->insert(tuple,READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
for(const auto& env0 : *rel_72_VNum) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_18_delta_VNum->insert(tuple,READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt));
}
}
();SignalHandler::instance()->setMsg(R"_(un___VBool(out,_0) :- 
   hasType__VBool(out),
   path__VBool__0(out,_0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [55:1-55:69])_");
if(!(rel_90_hasType_VBool->empty()) && !(rel_117_path_VBool_0->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_90_hasType_VBool_op_ctxt,rel_90_hasType_VBool->createContext());
CREATE_OP_CONTEXT(rel_117_path_VBool_0_op_ctxt,rel_117_path_VBool_0->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
for(const auto& env0 : *rel_90_hasType_VBool) {
auto range = rel_117_path_VBool_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_117_path_VBool_0_op_ctxt));
for(const auto& env1 : range) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env1[1])}};
rel_122_un_VBool->insert(tuple,READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
}
}
}
();}
[&](){
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
for(const auto& env0 : *rel_122_un_VBool) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_38_delta_un_VBool->insert(tuple,READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_102_input_freevars_op_ctxt,rel_102_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_35_delta_input_freevars_op_ctxt,rel_35_delta_input_freevars->createContext());
for(const auto& env0 : *rel_102_input_freevars) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_35_delta_input_freevars->insert(tuple,READ_OP_CONTEXT(rel_35_delta_input_freevars_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_26_delta_greaterThan_op_ctxt,rel_26_delta_greaterThan->createContext());
for(const auto& env0 : *rel_82_greaterThan) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_26_delta_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_26_delta_greaterThan_op_ctxt));
}
}
();SignalHandler::instance()->setMsg(R"_(un___VNum(out,_0) :- 
   hasType__VNum(out),
   path__VNum__0(out,_0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [60:1-60:66])_");
if(!(rel_91_hasType_VNum->empty()) && !(rel_118_path_VNum_0->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_91_hasType_VNum_op_ctxt,rel_91_hasType_VNum->createContext());
CREATE_OP_CONTEXT(rel_118_path_VNum_0_op_ctxt,rel_118_path_VNum_0->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
for(const auto& env0 : *rel_91_hasType_VNum) {
auto range = rel_118_path_VNum_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_118_path_VNum_0_op_ctxt));
for(const auto& env1 : range) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env1[1])}};
rel_123_un_VNum->insert(tuple,READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
}
}
}
();}
[&](){
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
for(const auto& env0 : *rel_123_un_VNum) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_39_delta_un_VNum->insert(tuple,READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
for(const auto& env0 : *rel_101_input_flow) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_34_delta_input_flow->insert(tuple,READ_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
for(const auto& env0 : *rel_74_aeval) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env0[3])}};
rel_20_delta_aeval->insert(tuple,READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_95_input_VBool_op_ctxt,rel_95_input_VBool->createContext());
CREATE_OP_CONTEXT(rel_28_delta_input_VBool_op_ctxt,rel_28_delta_input_VBool->createContext());
for(const auto& env0 : *rel_95_input_VBool) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_28_delta_input_VBool->insert(tuple,READ_OP_CONTEXT(rel_28_delta_input_VBool_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_104_input_init_op_ctxt,rel_104_input_init->createContext());
CREATE_OP_CONTEXT(rel_37_delta_input_init_op_ctxt,rel_37_delta_input_init->createContext());
for(const auto& env0 : *rel_104_input_init) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_37_delta_input_init->insert(tuple,READ_OP_CONTEXT(rel_37_delta_input_init_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_19_delta_add_op_ctxt,rel_19_delta_add->createContext());
for(const auto& env0 : *rel_73_add) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_19_delta_add->insert(tuple,READ_OP_CONTEXT(rel_19_delta_add_op_ctxt));
}
}
();SignalHandler::instance()->setMsg(R"_(input__final(stm__0) :- 
   ext_input__final_var(stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [99:1-99:54])_");
if(!(rel_76_ext_input_final_var->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_76_ext_input_final_var_op_ctxt,rel_76_ext_input_final_var->createContext());
CREATE_OP_CONTEXT(rel_100_input_final_op_ctxt,rel_100_input_final->createContext());
for(const auto& env0 : *rel_76_ext_input_final_var) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_100_input_final->insert(tuple,READ_OP_CONTEXT(rel_100_input_final_op_ctxt));
}
}
();}
[&](){
CREATE_OP_CONTEXT(rel_100_input_final_op_ctxt,rel_100_input_final->createContext());
CREATE_OP_CONTEXT(rel_33_delta_input_final_op_ctxt,rel_33_delta_input_final->createContext());
for(const auto& env0 : *rel_100_input_final) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_33_delta_input_final->insert(tuple,READ_OP_CONTEXT(rel_33_delta_input_final_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
for(const auto& env0 : *rel_103_input_freevarsStm) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_36_delta_input_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_96_input_VNum_op_ctxt,rel_96_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_29_delta_input_VNum_op_ctxt,rel_29_delta_input_VNum->createContext());
for(const auto& env0 : *rel_96_input_VNum) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_29_delta_input_VNum->insert(tuple,READ_OP_CONTEXT(rel_29_delta_input_VNum_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_30_delta_input_aeval->insert(tuple,READ_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
for(const auto& env0 : *rel_98_input_entry_var) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_31_delta_input_entry_var->insert(tuple,READ_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt));
}
}
();[&](){
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
CREATE_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt,rel_32_delta_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_32_delta_input_exit_var->insert(tuple,READ_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt));
}
}
();if(!(rel_6_disconnected5->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_14_delta_disconnected5_op_ctxt,rel_14_delta_disconnected5->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_14_delta_disconnected5->insert(tuple,READ_OP_CONTEXT(rel_14_delta_disconnected5_op_ctxt));
}
();}
if(!(rel_1_disconnected0->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_9_delta_disconnected0_op_ctxt,rel_9_delta_disconnected0->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_9_delta_disconnected0->insert(tuple,READ_OP_CONTEXT(rel_9_delta_disconnected0_op_ctxt));
}
();}
if(!(rel_2_disconnected1->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_10_delta_disconnected1_op_ctxt,rel_10_delta_disconnected1->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_10_delta_disconnected1->insert(tuple,READ_OP_CONTEXT(rel_10_delta_disconnected1_op_ctxt));
}
();}
if(!(rel_3_disconnected2->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_11_delta_disconnected2_op_ctxt,rel_11_delta_disconnected2->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_11_delta_disconnected2->insert(tuple,READ_OP_CONTEXT(rel_11_delta_disconnected2_op_ctxt));
}
();}
if(!(rel_4_disconnected3->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_12_delta_disconnected3_op_ctxt,rel_12_delta_disconnected3->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_12_delta_disconnected3->insert(tuple,READ_OP_CONTEXT(rel_12_delta_disconnected3_op_ctxt));
}
();}
if(!(rel_5_disconnected4->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_13_delta_disconnected4_op_ctxt,rel_13_delta_disconnected4->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_13_delta_disconnected4->insert(tuple,READ_OP_CONTEXT(rel_13_delta_disconnected4_op_ctxt));
}
();}
if(!(rel_7_disconnected6->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_15_delta_disconnected6_op_ctxt,rel_15_delta_disconnected6->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_15_delta_disconnected6->insert(tuple,READ_OP_CONTEXT(rel_15_delta_disconnected6_op_ctxt));
}
();}
if(!(rel_8_disconnected7->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_16_delta_disconnected7_op_ctxt,rel_16_delta_disconnected7->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_16_delta_disconnected7->insert(tuple,READ_OP_CONTEXT(rel_16_delta_disconnected7_op_ctxt));
}
();}
iter = 0;
for(;;) {
SECTIONS_START;
SECTION_START;
SignalHandler::instance()->setMsg(R"_(final(stm,out__0) :- 
   input__final(stm),
   hasType__Sequence(stm),
   path__Sequence__1(stm,s2),
   final(s2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [21:1-21:112])_");
if(!(rel_77_final->empty()) && !(rel_116_path_Sequence_1->empty()) && !(rel_33_delta_input_final->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_53_new_final_op_ctxt,rel_53_new_final->createContext());
CREATE_OP_CONTEXT(rel_33_delta_input_final_op_ctxt,rel_33_delta_input_final->createContext());
for(const auto& env0 : *rel_33_delta_input_final) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_77_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_77_final_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_22_delta_final->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt))) && !(rel_77_final->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_77_final_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_53_new_final->insert(tuple,READ_OP_CONTEXT(rel_53_new_final_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(final(stm,out__0) :- 
   input__final(stm),
   hasType__Sequence(stm),
   path__Sequence__1(stm,s2),
   final(s2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [21:1-21:112])_");
if(!(rel_22_delta_final->empty()) && !(rel_116_path_Sequence_1->empty()) && !(rel_100_input_final->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_53_new_final_op_ctxt,rel_53_new_final->createContext());
CREATE_OP_CONTEXT(rel_100_input_final_op_ctxt,rel_100_input_final->createContext());
for(const auto& env0 : *rel_100_input_final) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_22_delta_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_77_final->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_77_final_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_53_new_final->insert(tuple,READ_OP_CONTEXT(rel_53_new_final_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(final(stm,out__0) :- 
   input__final(stm),
   hasType__If(stm),
   path__If__1(stm,s1),
   final(s1,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [22:1-22:100])_");
if(!(rel_77_final->empty()) && !(rel_112_path_If_1->empty()) && !(rel_33_delta_input_final->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_112_path_If_1_op_ctxt,rel_112_path_If_1->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_53_new_final_op_ctxt,rel_53_new_final->createContext());
CREATE_OP_CONTEXT(rel_33_delta_input_final_op_ctxt,rel_33_delta_input_final->createContext());
for(const auto& env0 : *rel_33_delta_input_final) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_112_path_If_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_112_path_If_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_77_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_77_final_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_22_delta_final->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt))) && !(rel_77_final->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_77_final_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_53_new_final->insert(tuple,READ_OP_CONTEXT(rel_53_new_final_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(final(stm,out__0) :- 
   input__final(stm),
   hasType__If(stm),
   path__If__1(stm,s1),
   final(s1,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [22:1-22:100])_");
if(!(rel_22_delta_final->empty()) && !(rel_112_path_If_1->empty()) && !(rel_100_input_final->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_112_path_If_1_op_ctxt,rel_112_path_If_1->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_53_new_final_op_ctxt,rel_53_new_final->createContext());
CREATE_OP_CONTEXT(rel_100_input_final_op_ctxt,rel_100_input_final->createContext());
for(const auto& env0 : *rel_100_input_final) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_112_path_If_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_112_path_If_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_22_delta_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_77_final->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_77_final_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_53_new_final->insert(tuple,READ_OP_CONTEXT(rel_53_new_final_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(final(stm,out__0) :- 
   input__final(stm),
   hasType__If(stm),
   path__If__2(stm,s2),
   final(s2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [23:1-23:100])_");
if(!(rel_77_final->empty()) && !(rel_113_path_If_2->empty()) && !(rel_33_delta_input_final->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_113_path_If_2_op_ctxt,rel_113_path_If_2->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_53_new_final_op_ctxt,rel_53_new_final->createContext());
CREATE_OP_CONTEXT(rel_33_delta_input_final_op_ctxt,rel_33_delta_input_final->createContext());
for(const auto& env0 : *rel_33_delta_input_final) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_113_path_If_2->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_113_path_If_2_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_77_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_77_final_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_22_delta_final->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt))) && !(rel_77_final->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_77_final_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_53_new_final->insert(tuple,READ_OP_CONTEXT(rel_53_new_final_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(final(stm,out__0) :- 
   input__final(stm),
   hasType__If(stm),
   path__If__2(stm,s2),
   final(s2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [23:1-23:100])_");
if(!(rel_22_delta_final->empty()) && !(rel_113_path_If_2->empty()) && !(rel_100_input_final->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_113_path_If_2_op_ctxt,rel_113_path_If_2->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_53_new_final_op_ctxt,rel_53_new_final->createContext());
CREATE_OP_CONTEXT(rel_100_input_final_op_ctxt,rel_100_input_final->createContext());
for(const auto& env0 : *rel_100_input_final) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_113_path_If_2->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_113_path_If_2_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_22_delta_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_77_final->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_77_final_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_53_new_final->insert(tuple,READ_OP_CONTEXT(rel_53_new_final_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(final(stm,stm) :- 
   input__final(stm),
   hasType__Assign(stm).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [19:1-19:77])_");
if(!(rel_33_delta_input_final->empty()) && !(rel_84_hasType_Assign->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt,rel_84_hasType_Assign->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_53_new_final_op_ctxt,rel_53_new_final->createContext());
CREATE_OP_CONTEXT(rel_33_delta_input_final_op_ctxt,rel_33_delta_input_final->createContext());
for(const auto& env0 : *rel_33_delta_input_final) {
if( rel_84_hasType_Assign->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt)) && !(rel_77_final->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_77_final_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[0])}};
rel_53_new_final->insert(tuple,READ_OP_CONTEXT(rel_53_new_final_op_ctxt));
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(final(stm,stm) :- 
   input__final(stm),
   hasType__Skip(stm).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [20:1-20:75])_");
if(!(rel_33_delta_input_final->empty()) && !(rel_89_hasType_Skip->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt,rel_89_hasType_Skip->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_53_new_final_op_ctxt,rel_53_new_final->createContext());
CREATE_OP_CONTEXT(rel_33_delta_input_final_op_ctxt,rel_33_delta_input_final->createContext());
for(const auto& env0 : *rel_33_delta_input_final) {
if( rel_89_hasType_Skip->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt)) && !(rel_77_final->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_77_final_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[0])}};
rel_53_new_final->insert(tuple,READ_OP_CONTEXT(rel_53_new_final_op_ctxt));
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(final(stm,stm) :- 
   input__final(stm),
   hasType__While(stm).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [24:1-24:76])_");
if(!(rel_33_delta_input_final->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_53_new_final_op_ctxt,rel_53_new_final->createContext());
CREATE_OP_CONTEXT(rel_33_delta_input_final_op_ctxt,rel_33_delta_input_final->createContext());
for(const auto& env0 : *rel_33_delta_input_final) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt)) && !(rel_77_final->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_77_final_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[0])}};
rel_53_new_final->insert(tuple,READ_OP_CONTEXT(rel_53_new_final_op_ctxt));
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(init(stm,out__0) :- 
   input__init(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,s1),
   init(s1,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [15:1-15:109])_");
if(!(rel_94_init->empty()) && !(rel_115_path_Sequence_0->empty()) && !(rel_37_delta_input_init->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_94_init_op_ctxt,rel_94_init->createContext());
CREATE_OP_CONTEXT(rel_27_delta_init_op_ctxt,rel_27_delta_init->createContext());
CREATE_OP_CONTEXT(rel_58_new_init_op_ctxt,rel_58_new_init->createContext());
CREATE_OP_CONTEXT(rel_37_delta_input_init_op_ctxt,rel_37_delta_input_init->createContext());
for(const auto& env0 : *rel_37_delta_input_init) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_94_init->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_94_init_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_27_delta_init->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_27_delta_init_op_ctxt))) && !(rel_94_init->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_94_init_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_58_new_init->insert(tuple,READ_OP_CONTEXT(rel_58_new_init_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(init(stm,out__0) :- 
   input__init(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,s1),
   init(s1,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [15:1-15:109])_");
if(!(rel_27_delta_init->empty()) && !(rel_115_path_Sequence_0->empty()) && !(rel_104_input_init->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_94_init_op_ctxt,rel_94_init->createContext());
CREATE_OP_CONTEXT(rel_27_delta_init_op_ctxt,rel_27_delta_init->createContext());
CREATE_OP_CONTEXT(rel_58_new_init_op_ctxt,rel_58_new_init->createContext());
CREATE_OP_CONTEXT(rel_104_input_init_op_ctxt,rel_104_input_init->createContext());
for(const auto& env0 : *rel_104_input_init) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_27_delta_init->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_27_delta_init_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_94_init->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_94_init_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_58_new_init->insert(tuple,READ_OP_CONTEXT(rel_58_new_init_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(init(stm,stm) :- 
   input__init(stm),
   hasType__Assign(stm).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [13:1-13:75])_");
if(!(rel_37_delta_input_init->empty()) && !(rel_84_hasType_Assign->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt,rel_84_hasType_Assign->createContext());
CREATE_OP_CONTEXT(rel_94_init_op_ctxt,rel_94_init->createContext());
CREATE_OP_CONTEXT(rel_58_new_init_op_ctxt,rel_58_new_init->createContext());
CREATE_OP_CONTEXT(rel_37_delta_input_init_op_ctxt,rel_37_delta_input_init->createContext());
for(const auto& env0 : *rel_37_delta_input_init) {
if( rel_84_hasType_Assign->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt)) && !(rel_94_init->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_94_init_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[0])}};
rel_58_new_init->insert(tuple,READ_OP_CONTEXT(rel_58_new_init_op_ctxt));
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(init(stm,stm) :- 
   input__init(stm),
   hasType__Skip(stm).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [14:1-14:73])_");
if(!(rel_37_delta_input_init->empty()) && !(rel_89_hasType_Skip->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt,rel_89_hasType_Skip->createContext());
CREATE_OP_CONTEXT(rel_94_init_op_ctxt,rel_94_init->createContext());
CREATE_OP_CONTEXT(rel_58_new_init_op_ctxt,rel_58_new_init->createContext());
CREATE_OP_CONTEXT(rel_37_delta_input_init_op_ctxt,rel_37_delta_input_init->createContext());
for(const auto& env0 : *rel_37_delta_input_init) {
if( rel_89_hasType_Skip->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt)) && !(rel_94_init->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_94_init_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[0])}};
rel_58_new_init->insert(tuple,READ_OP_CONTEXT(rel_58_new_init_op_ctxt));
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(init(stm,stm) :- 
   input__init(stm),
   hasType__If(stm).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [16:1-16:71])_");
if(!(rel_37_delta_input_init->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_94_init_op_ctxt,rel_94_init->createContext());
CREATE_OP_CONTEXT(rel_58_new_init_op_ctxt,rel_58_new_init->createContext());
CREATE_OP_CONTEXT(rel_37_delta_input_init_op_ctxt,rel_37_delta_input_init->createContext());
for(const auto& env0 : *rel_37_delta_input_init) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt)) && !(rel_94_init->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_94_init_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[0])}};
rel_58_new_init->insert(tuple,READ_OP_CONTEXT(rel_58_new_init_op_ctxt));
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(init(stm,stm) :- 
   input__init(stm),
   hasType__While(stm).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [17:1-17:74])_");
if(!(rel_37_delta_input_init->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_94_init_op_ctxt,rel_94_init->createContext());
CREATE_OP_CONTEXT(rel_58_new_init_op_ctxt,rel_58_new_init->createContext());
CREATE_OP_CONTEXT(rel_37_delta_input_init_op_ctxt,rel_37_delta_input_init->createContext());
for(const auto& env0 : *rel_37_delta_input_init) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt)) && !(rel_94_init->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_94_init_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[0])}};
rel_58_new_init->insert(tuple,READ_OP_CONTEXT(rel_58_new_init_op_ctxt));
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(freevars(exp,out__0) :- 
   input__freevars(exp),
   hasType__Var(exp),
   path__Var__0(exp,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [37:1-37:93])_");
if(!(rel_119_path_Var_0->empty()) && !(rel_35_delta_input_freevars->empty()) && !(rel_92_hasType_Var->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_92_hasType_Var_op_ctxt,rel_92_hasType_Var->createContext());
CREATE_OP_CONTEXT(rel_119_path_Var_0_op_ctxt,rel_119_path_Var_0->createContext());
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_55_new_freevars_op_ctxt,rel_55_new_freevars->createContext());
CREATE_OP_CONTEXT(rel_35_delta_input_freevars_op_ctxt,rel_35_delta_input_freevars->createContext());
for(const auto& env0 : *rel_35_delta_input_freevars) {
if( rel_92_hasType_Var->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_92_hasType_Var_op_ctxt))) {
auto range = rel_119_path_Var_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_119_path_Var_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_80_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env1[1])}};
rel_55_new_freevars->insert(tuple,READ_OP_CONTEXT(rel_55_new_freevars_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevars(exp,out__0) :- 
   input__freevars(exp),
   hasType__GreaterThan(exp),
   path__GreaterThan__0(exp,e1),
   freevars(e1,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [38:1-38:127])_");
if(!(rel_80_freevars->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_35_delta_input_freevars->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_55_new_freevars_op_ctxt,rel_55_new_freevars->createContext());
CREATE_OP_CONTEXT(rel_35_delta_input_freevars_op_ctxt,rel_35_delta_input_freevars->createContext());
for(const auto& env0 : *rel_35_delta_input_freevars) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_80_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_24_delta_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt))) && !(rel_80_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_55_new_freevars->insert(tuple,READ_OP_CONTEXT(rel_55_new_freevars_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevars(exp,out__0) :- 
   input__freevars(exp),
   hasType__GreaterThan(exp),
   path__GreaterThan__0(exp,e1),
   freevars(e1,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [38:1-38:127])_");
if(!(rel_24_delta_freevars->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_102_input_freevars->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_55_new_freevars_op_ctxt,rel_55_new_freevars->createContext());
CREATE_OP_CONTEXT(rel_102_input_freevars_op_ctxt,rel_102_input_freevars->createContext());
for(const auto& env0 : *rel_102_input_freevars) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_24_delta_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_80_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_55_new_freevars->insert(tuple,READ_OP_CONTEXT(rel_55_new_freevars_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevars(exp,out__0) :- 
   input__freevars(exp),
   hasType__GreaterThan(exp),
   path__GreaterThan__1(exp,e2),
   freevars(e2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [39:1-39:127])_");
if(!(rel_80_freevars->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_35_delta_input_freevars->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_55_new_freevars_op_ctxt,rel_55_new_freevars->createContext());
CREATE_OP_CONTEXT(rel_35_delta_input_freevars_op_ctxt,rel_35_delta_input_freevars->createContext());
for(const auto& env0 : *rel_35_delta_input_freevars) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_80_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_24_delta_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt))) && !(rel_80_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_55_new_freevars->insert(tuple,READ_OP_CONTEXT(rel_55_new_freevars_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevars(exp,out__0) :- 
   input__freevars(exp),
   hasType__GreaterThan(exp),
   path__GreaterThan__1(exp,e2),
   freevars(e2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [39:1-39:127])_");
if(!(rel_24_delta_freevars->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_102_input_freevars->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_55_new_freevars_op_ctxt,rel_55_new_freevars->createContext());
CREATE_OP_CONTEXT(rel_102_input_freevars_op_ctxt,rel_102_input_freevars->createContext());
for(const auto& env0 : *rel_102_input_freevars) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_24_delta_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_80_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_55_new_freevars->insert(tuple,READ_OP_CONTEXT(rel_55_new_freevars_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevars(exp,out__0) :- 
   input__freevars(exp),
   hasType__Add(exp),
   path__Add__0(exp,e1),
   freevars(e1,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [40:1-40:111])_");
if(!(rel_80_freevars->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_35_delta_input_freevars->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_55_new_freevars_op_ctxt,rel_55_new_freevars->createContext());
CREATE_OP_CONTEXT(rel_35_delta_input_freevars_op_ctxt,rel_35_delta_input_freevars->createContext());
for(const auto& env0 : *rel_35_delta_input_freevars) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_80_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_24_delta_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt))) && !(rel_80_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_55_new_freevars->insert(tuple,READ_OP_CONTEXT(rel_55_new_freevars_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevars(exp,out__0) :- 
   input__freevars(exp),
   hasType__Add(exp),
   path__Add__0(exp,e1),
   freevars(e1,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [40:1-40:111])_");
if(!(rel_24_delta_freevars->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_102_input_freevars->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_55_new_freevars_op_ctxt,rel_55_new_freevars->createContext());
CREATE_OP_CONTEXT(rel_102_input_freevars_op_ctxt,rel_102_input_freevars->createContext());
for(const auto& env0 : *rel_102_input_freevars) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_24_delta_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_80_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_55_new_freevars->insert(tuple,READ_OP_CONTEXT(rel_55_new_freevars_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevars(exp,out__0) :- 
   input__freevars(exp),
   hasType__Add(exp),
   path__Add__1(exp,e2),
   freevars(e2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [41:1-41:111])_");
if(!(rel_80_freevars->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_35_delta_input_freevars->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_55_new_freevars_op_ctxt,rel_55_new_freevars->createContext());
CREATE_OP_CONTEXT(rel_35_delta_input_freevars_op_ctxt,rel_35_delta_input_freevars->createContext());
for(const auto& env0 : *rel_35_delta_input_freevars) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_80_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_24_delta_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt))) && !(rel_80_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_55_new_freevars->insert(tuple,READ_OP_CONTEXT(rel_55_new_freevars_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevars(exp,out__0) :- 
   input__freevars(exp),
   hasType__Add(exp),
   path__Add__1(exp,e2),
   freevars(e2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [41:1-41:111])_");
if(!(rel_24_delta_freevars->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_102_input_freevars->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_55_new_freevars_op_ctxt,rel_55_new_freevars->createContext());
CREATE_OP_CONTEXT(rel_102_input_freevars_op_ctxt,rel_102_input_freevars->createContext());
for(const auto& env0 : *rel_102_input_freevars) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_24_delta_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_80_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_55_new_freevars->insert(tuple,READ_OP_CONTEXT(rel_55_new_freevars_op_ctxt));
}
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,out_1__0) :- 
   input__flow(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,s1),
   flow(s1,out_0__0,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [26:1-26:133])_");
if(!(rel_79_flow->empty()) && !(rel_115_path_Sequence_0->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_79_flow->lowerUpperRange_100(Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env1[1]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt))) && !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,out_1__0) :- 
   input__flow(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,s1),
   flow(s1,out_0__0,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [26:1-26:133])_");
if(!(rel_23_delta_flow->empty()) && !(rel_115_path_Sequence_0->empty()) && !(rel_101_input_flow->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
for(const auto& env0 : *rel_101_input_flow) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_23_delta_flow->lowerUpperRange_100(Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,out_1__0) :- 
   input__flow(stm),
   hasType__Sequence(stm),
   path__Sequence__1(stm,s2),
   flow(s2,out_0__0,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [27:1-27:133])_");
if(!(rel_79_flow->empty()) && !(rel_116_path_Sequence_1->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_79_flow->lowerUpperRange_100(Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env1[1]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt))) && !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,out_1__0) :- 
   input__flow(stm),
   hasType__Sequence(stm),
   path__Sequence__1(stm,s2),
   flow(s2,out_0__0,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [27:1-27:133])_");
if(!(rel_23_delta_flow->empty()) && !(rel_116_path_Sequence_1->empty()) && !(rel_101_input_flow->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
for(const auto& env0 : *rel_101_input_flow) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_23_delta_flow->lowerUpperRange_100(Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,out_1__0) :- 
   input__flow(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,s1),
   path__Sequence__1(stm,s2),
   final(s1,out_0__0),
   init(s2,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [28:1-28:172])_");
if(!(rel_94_init->empty()) && !(rel_77_final->empty()) && !(rel_116_path_Sequence_1->empty()) && !(rel_115_path_Sequence_0->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_94_init_op_ctxt,rel_94_init->createContext());
CREATE_OP_CONTEXT(rel_27_delta_init_op_ctxt,rel_27_delta_init->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_77_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_77_final_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_22_delta_final->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env3[1])}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt)))) {
auto range = rel_94_init->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env2[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env2[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_94_init_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_27_delta_init->contains(Tuple<RamDomain,2>{{ramBitCast(env2[1]),ramBitCast(env4[1])}},READ_OP_CONTEXT(rel_27_delta_init_op_ctxt))) && !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env3[1]),ramBitCast(env4[1])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env3[1]),ramBitCast(env4[1])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,out_1__0) :- 
   input__flow(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,s1),
   path__Sequence__1(stm,s2),
   final(s1,out_0__0),
   init(s2,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [28:1-28:172])_");
if(!(rel_94_init->empty()) && !(rel_22_delta_final->empty()) && !(rel_116_path_Sequence_1->empty()) && !(rel_115_path_Sequence_0->empty()) && !(rel_101_input_flow->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_94_init_op_ctxt,rel_94_init->createContext());
CREATE_OP_CONTEXT(rel_27_delta_init_op_ctxt,rel_27_delta_init->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
for(const auto& env0 : *rel_101_input_flow) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_22_delta_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_94_init->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env2[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env2[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_94_init_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_27_delta_init->contains(Tuple<RamDomain,2>{{ramBitCast(env2[1]),ramBitCast(env4[1])}},READ_OP_CONTEXT(rel_27_delta_init_op_ctxt))) && !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env3[1]),ramBitCast(env4[1])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env3[1]),ramBitCast(env4[1])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,out_1__0) :- 
   input__flow(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,s1),
   path__Sequence__1(stm,s2),
   final(s1,out_0__0),
   init(s2,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [28:1-28:172])_");
if(!(rel_27_delta_init->empty()) && !(rel_77_final->empty()) && !(rel_116_path_Sequence_1->empty()) && !(rel_115_path_Sequence_0->empty()) && !(rel_101_input_flow->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_27_delta_init_op_ctxt,rel_27_delta_init->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
for(const auto& env0 : *rel_101_input_flow) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_77_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_77_final_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_27_delta_init->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env2[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env2[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_27_delta_init_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env3[1]),ramBitCast(env4[1])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env3[1]),ramBitCast(env4[1])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,out_1__0) :- 
   input__flow(stm),
   hasType__If(stm),
   path__If__1(stm,s1),
   flow(s1,out_0__0,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [29:1-29:121])_");
if(!(rel_79_flow->empty()) && !(rel_112_path_If_1->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_112_path_If_1_op_ctxt,rel_112_path_If_1->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_112_path_If_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_112_path_If_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_79_flow->lowerUpperRange_100(Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env1[1]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt))) && !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,out_1__0) :- 
   input__flow(stm),
   hasType__If(stm),
   path__If__1(stm,s1),
   flow(s1,out_0__0,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [29:1-29:121])_");
if(!(rel_23_delta_flow->empty()) && !(rel_112_path_If_1->empty()) && !(rel_101_input_flow->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_112_path_If_1_op_ctxt,rel_112_path_If_1->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
for(const auto& env0 : *rel_101_input_flow) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_112_path_If_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_112_path_If_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_23_delta_flow->lowerUpperRange_100(Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,out_1__0) :- 
   input__flow(stm),
   hasType__If(stm),
   path__If__2(stm,s2),
   flow(s2,out_0__0,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [30:1-30:121])_");
if(!(rel_79_flow->empty()) && !(rel_113_path_If_2->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_113_path_If_2_op_ctxt,rel_113_path_If_2->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_113_path_If_2->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_113_path_If_2_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_79_flow->lowerUpperRange_100(Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env1[1]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt))) && !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,out_1__0) :- 
   input__flow(stm),
   hasType__If(stm),
   path__If__2(stm,s2),
   flow(s2,out_0__0,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [30:1-30:121])_");
if(!(rel_23_delta_flow->empty()) && !(rel_113_path_If_2->empty()) && !(rel_101_input_flow->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_113_path_If_2_op_ctxt,rel_113_path_If_2->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
for(const auto& env0 : *rel_101_input_flow) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_113_path_If_2->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_113_path_If_2_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_23_delta_flow->lowerUpperRange_100(Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,out_1__0) :- 
   input__flow(stm),
   hasType__While(stm),
   path__While__1(stm,s),
   flow(s,out_0__0,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [33:1-33:125])_");
if(!(rel_79_flow->empty()) && !(rel_121_path_While_1->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_121_path_While_1_op_ctxt,rel_121_path_While_1->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_121_path_While_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_121_path_While_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_79_flow->lowerUpperRange_100(Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env1[1]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt))) && !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,out_1__0) :- 
   input__flow(stm),
   hasType__While(stm),
   path__While__1(stm,s),
   flow(s,out_0__0,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [33:1-33:125])_");
if(!(rel_23_delta_flow->empty()) && !(rel_121_path_While_1->empty()) && !(rel_101_input_flow->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_121_path_While_1_op_ctxt,rel_121_path_While_1->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
for(const auto& env0 : *rel_101_input_flow) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_121_path_While_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_121_path_While_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_23_delta_flow->lowerUpperRange_100(Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env2[2])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,stm,out_1__0) :- 
   input__flow(stm),
   hasType__If(stm),
   path__If__1(stm,s1),
   init(s1,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [31:1-31:127])_");
if(!(rel_94_init->empty()) && !(rel_112_path_If_1->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_112_path_If_1_op_ctxt,rel_112_path_If_1->createContext());
CREATE_OP_CONTEXT(rel_94_init_op_ctxt,rel_94_init->createContext());
CREATE_OP_CONTEXT(rel_27_delta_init_op_ctxt,rel_27_delta_init->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_112_path_If_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_112_path_If_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_94_init->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_94_init_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_27_delta_init->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_27_delta_init_op_ctxt))) && !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,stm,out_1__0) :- 
   input__flow(stm),
   hasType__If(stm),
   path__If__1(stm,s1),
   init(s1,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [31:1-31:127])_");
if(!(rel_27_delta_init->empty()) && !(rel_112_path_If_1->empty()) && !(rel_101_input_flow->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_112_path_If_1_op_ctxt,rel_112_path_If_1->createContext());
CREATE_OP_CONTEXT(rel_27_delta_init_op_ctxt,rel_27_delta_init->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
for(const auto& env0 : *rel_101_input_flow) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_112_path_If_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_112_path_If_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_27_delta_init->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_27_delta_init_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,stm,out_1__0) :- 
   input__flow(stm),
   hasType__If(stm),
   path__If__2(stm,s2),
   init(s2,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [32:1-32:127])_");
if(!(rel_94_init->empty()) && !(rel_113_path_If_2->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_113_path_If_2_op_ctxt,rel_113_path_If_2->createContext());
CREATE_OP_CONTEXT(rel_94_init_op_ctxt,rel_94_init->createContext());
CREATE_OP_CONTEXT(rel_27_delta_init_op_ctxt,rel_27_delta_init->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_113_path_If_2->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_113_path_If_2_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_94_init->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_94_init_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_27_delta_init->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_27_delta_init_op_ctxt))) && !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,stm,out_1__0) :- 
   input__flow(stm),
   hasType__If(stm),
   path__If__2(stm,s2),
   init(s2,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [32:1-32:127])_");
if(!(rel_27_delta_init->empty()) && !(rel_113_path_If_2->empty()) && !(rel_101_input_flow->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_113_path_If_2_op_ctxt,rel_113_path_If_2->createContext());
CREATE_OP_CONTEXT(rel_27_delta_init_op_ctxt,rel_27_delta_init->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
for(const auto& env0 : *rel_101_input_flow) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_113_path_If_2->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_113_path_If_2_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_27_delta_init->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_27_delta_init_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,stm,out_1__0) :- 
   input__flow(stm),
   hasType__While(stm),
   path__While__1(stm,s),
   init(s,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [34:1-34:131])_");
if(!(rel_94_init->empty()) && !(rel_121_path_While_1->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_121_path_While_1_op_ctxt,rel_121_path_While_1->createContext());
CREATE_OP_CONTEXT(rel_94_init_op_ctxt,rel_94_init->createContext());
CREATE_OP_CONTEXT(rel_27_delta_init_op_ctxt,rel_27_delta_init->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_121_path_While_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_121_path_While_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_94_init->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_94_init_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_27_delta_init->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_27_delta_init_op_ctxt))) && !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,stm,out_1__0) :- 
   input__flow(stm),
   hasType__While(stm),
   path__While__1(stm,s),
   init(s,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [34:1-34:131])_");
if(!(rel_27_delta_init->empty()) && !(rel_121_path_While_1->empty()) && !(rel_101_input_flow->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_121_path_While_1_op_ctxt,rel_121_path_While_1->createContext());
CREATE_OP_CONTEXT(rel_27_delta_init_op_ctxt,rel_27_delta_init->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
for(const auto& env0 : *rel_101_input_flow) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_121_path_While_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_121_path_While_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_27_delta_init->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_27_delta_init_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,stm) :- 
   input__flow(stm),
   hasType__While(stm),
   path__While__1(stm,s),
   final(s,out_0__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [35:1-35:132])_");
if(!(rel_77_final->empty()) && !(rel_121_path_While_1->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_121_path_While_1_op_ctxt,rel_121_path_While_1->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_121_path_While_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_121_path_While_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_77_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_77_final_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_22_delta_final->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt))) && !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env0[0])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(flow(stm,out_0__0,stm) :- 
   input__flow(stm),
   hasType__While(stm),
   path__While__1(stm,s),
   final(s,out_0__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [35:1-35:132])_");
if(!(rel_22_delta_final->empty()) && !(rel_121_path_While_1->empty()) && !(rel_101_input_flow->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_121_path_While_1_op_ctxt,rel_121_path_While_1->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
for(const auto& env0 : *rel_101_input_flow) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_121_path_While_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_121_path_While_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_22_delta_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_79_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env0[0])}};
rel_54_new_flow->insert(tuple,READ_OP_CONTEXT(rel_54_new_flow_op_ctxt));
}
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__Assign(stm),
   path__Assign__1(stm,a),
   freevars(a,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [43:1-43:121])_");
if(!(rel_80_freevars->empty()) && !(rel_108_path_Assign_1->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_84_hasType_Assign->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt,rel_84_hasType_Assign->createContext());
CREATE_OP_CONTEXT(rel_108_path_Assign_1_op_ctxt,rel_108_path_Assign_1->createContext());
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_84_hasType_Assign->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt))) {
auto range = rel_108_path_Assign_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_108_path_Assign_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_80_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_24_delta_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt))) && !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__Assign(stm),
   path__Assign__1(stm,a),
   freevars(a,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [43:1-43:121])_");
if(!(rel_24_delta_freevars->empty()) && !(rel_108_path_Assign_1->empty()) && !(rel_103_input_freevarsStm->empty()) && !(rel_84_hasType_Assign->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt,rel_84_hasType_Assign->createContext());
CREATE_OP_CONTEXT(rel_108_path_Assign_1_op_ctxt,rel_108_path_Assign_1->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
for(const auto& env0 : *rel_103_input_freevarsStm) {
if( rel_84_hasType_Assign->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt))) {
auto range = rel_108_path_Assign_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_108_path_Assign_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_24_delta_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,s1),
   freevarsStm(s1,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [44:1-44:130])_");
if(!(rel_81_freevarsStm->empty()) && !(rel_115_path_Sequence_0->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt,rel_25_delta_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_81_freevarsStm->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_25_delta_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt))) && !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,s1),
   freevarsStm(s1,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [44:1-44:130])_");
if(!(rel_25_delta_freevarsStm->empty()) && !(rel_115_path_Sequence_0->empty()) && !(rel_103_input_freevarsStm->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt,rel_25_delta_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
for(const auto& env0 : *rel_103_input_freevarsStm) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_25_delta_freevarsStm->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__Sequence(stm),
   path__Sequence__1(stm,s2),
   freevarsStm(s2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [45:1-45:130])_");
if(!(rel_81_freevarsStm->empty()) && !(rel_116_path_Sequence_1->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt,rel_25_delta_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_81_freevarsStm->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_25_delta_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt))) && !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__Sequence(stm),
   path__Sequence__1(stm,s2),
   freevarsStm(s2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [45:1-45:130])_");
if(!(rel_25_delta_freevarsStm->empty()) && !(rel_116_path_Sequence_1->empty()) && !(rel_103_input_freevarsStm->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt,rel_25_delta_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
for(const auto& env0 : *rel_103_input_freevarsStm) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_25_delta_freevarsStm->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__If(stm),
   path__If__0(stm,c),
   freevars(c,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [46:1-46:113])_");
if(!(rel_80_freevars->empty()) && !(rel_111_path_If_0->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_111_path_If_0_op_ctxt,rel_111_path_If_0->createContext());
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_111_path_If_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_111_path_If_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_80_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_24_delta_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt))) && !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__If(stm),
   path__If__0(stm,c),
   freevars(c,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [46:1-46:113])_");
if(!(rel_24_delta_freevars->empty()) && !(rel_111_path_If_0->empty()) && !(rel_103_input_freevarsStm->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_111_path_If_0_op_ctxt,rel_111_path_If_0->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
for(const auto& env0 : *rel_103_input_freevarsStm) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_111_path_If_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_111_path_If_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_24_delta_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__If(stm),
   path__If__1(stm,s1),
   freevarsStm(s1,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [47:1-47:118])_");
if(!(rel_81_freevarsStm->empty()) && !(rel_112_path_If_1->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_112_path_If_1_op_ctxt,rel_112_path_If_1->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt,rel_25_delta_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_112_path_If_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_112_path_If_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_81_freevarsStm->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_25_delta_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt))) && !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__If(stm),
   path__If__1(stm,s1),
   freevarsStm(s1,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [47:1-47:118])_");
if(!(rel_25_delta_freevarsStm->empty()) && !(rel_112_path_If_1->empty()) && !(rel_103_input_freevarsStm->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_112_path_If_1_op_ctxt,rel_112_path_If_1->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt,rel_25_delta_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
for(const auto& env0 : *rel_103_input_freevarsStm) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_112_path_If_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_112_path_If_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_25_delta_freevarsStm->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__If(stm),
   path__If__2(stm,s2),
   freevarsStm(s2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [48:1-48:118])_");
if(!(rel_81_freevarsStm->empty()) && !(rel_113_path_If_2->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_113_path_If_2_op_ctxt,rel_113_path_If_2->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt,rel_25_delta_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_113_path_If_2->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_113_path_If_2_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_81_freevarsStm->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_25_delta_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt))) && !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__If(stm),
   path__If__2(stm,s2),
   freevarsStm(s2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [48:1-48:118])_");
if(!(rel_25_delta_freevarsStm->empty()) && !(rel_113_path_If_2->empty()) && !(rel_103_input_freevarsStm->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_113_path_If_2_op_ctxt,rel_113_path_If_2->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt,rel_25_delta_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
for(const auto& env0 : *rel_103_input_freevarsStm) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_113_path_If_2->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_113_path_If_2_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_25_delta_freevarsStm->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__While(stm),
   path__While__0(stm,c),
   freevars(c,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [49:1-49:119])_");
if(!(rel_80_freevars->empty()) && !(rel_120_path_While_0->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_120_path_While_0_op_ctxt,rel_120_path_While_0->createContext());
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_120_path_While_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_120_path_While_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_80_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_80_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_24_delta_freevars->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt))) && !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__While(stm),
   path__While__0(stm,c),
   freevars(c,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [49:1-49:119])_");
if(!(rel_24_delta_freevars->empty()) && !(rel_120_path_While_0->empty()) && !(rel_103_input_freevarsStm->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_120_path_While_0_op_ctxt,rel_120_path_While_0->createContext());
CREATE_OP_CONTEXT(rel_24_delta_freevars_op_ctxt,rel_24_delta_freevars->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
for(const auto& env0 : *rel_103_input_freevarsStm) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_120_path_While_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_120_path_While_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_24_delta_freevars->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_24_delta_freevars_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__While(stm),
   path__While__1(stm,s),
   freevarsStm(s,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [50:1-50:122])_");
if(!(rel_81_freevarsStm->empty()) && !(rel_121_path_While_1->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_121_path_While_1_op_ctxt,rel_121_path_While_1->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt,rel_25_delta_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_121_path_While_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_121_path_While_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_81_freevarsStm->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_25_delta_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt))) && !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(freevarsStm(stm,out__0) :- 
   input__freevarsStm(stm),
   hasType__While(stm),
   path__While__1(stm,s),
   freevarsStm(s,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [50:1-50:122])_");
if(!(rel_25_delta_freevarsStm->empty()) && !(rel_121_path_While_1->empty()) && !(rel_103_input_freevarsStm->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_121_path_While_1_op_ctxt,rel_121_path_While_1->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt,rel_25_delta_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
for(const auto& env0 : *rel_103_input_freevarsStm) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_121_path_While_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_121_path_While_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_25_delta_freevarsStm->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_81_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_56_new_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt));
}
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(VBool(_0,[0,_0]) :- 
   input__VBool(_0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [52:1-52:54])_");
if(!(rel_28_delta_input_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_48_new_VBool_op_ctxt,rel_48_new_VBool->createContext());
CREATE_OP_CONTEXT(rel_28_delta_input_VBool_op_ctxt,rel_28_delta_input_VBool->createContext());
for(const auto& env0 : *rel_28_delta_input_VBool) {
if( !(rel_71_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(pack(recordTable,Tuple<RamDomain,2>{{ramBitCast(ramBitCast(RamSigned(0))),ramBitCast(ramBitCast(env0[0]))}}
))}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(pack(recordTable,Tuple<RamDomain,2>{{ramBitCast(ramBitCast(RamSigned(0))),ramBitCast(ramBitCast(env0[0]))}}
))}};
rel_48_new_VBool->insert(tuple,READ_OP_CONTEXT(rel_48_new_VBool_op_ctxt));
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__Skip(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__1,stm),
   exit_var(pred__1,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [64:1-64:179])_");
if(!(rel_75_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_32_delta_input_exit_var->empty()) && !(rel_89_hasType_Skip->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt,rel_89_hasType_Skip->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt,rel_32_delta_input_exit_var->createContext());
for(const auto& env0 : *rel_32_delta_input_exit_var) {
if( rel_89_hasType_Skip->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt)) && !(rel_31_delta_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt))) && rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env1[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt)))) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__Skip(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__1,stm),
   exit_var(pred__1,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [64:1-64:179])_");
if(!(rel_75_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_31_delta_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_89_hasType_Skip->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt,rel_89_hasType_Skip->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_31_delta_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt)) && rel_89_hasType_Skip->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env1[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt)))) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__Skip(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__1,stm),
   exit_var(pred__1,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [64:1-64:179])_");
if(!(rel_75_exit_var->empty()) && !(rel_23_delta_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_89_hasType_Skip->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt,rel_89_hasType_Skip->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)) && rel_89_hasType_Skip->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt))) {
auto range = rel_23_delta_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__Skip(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__1,stm),
   exit_var(pred__1,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [64:1-64:179])_");
if(!(rel_21_delta_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_89_hasType_Skip->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt,rel_89_hasType_Skip->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)) && rel_89_hasType_Skip->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_21_delta_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__Sequence(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__2,stm),
   exit_var(pred__2,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [65:1-65:183])_");
if(!(rel_75_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_32_delta_input_exit_var->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt,rel_32_delta_input_exit_var->createContext());
for(const auto& env0 : *rel_32_delta_input_exit_var) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt)) && !(rel_31_delta_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt))) && rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env1[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt)))) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__Sequence(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__2,stm),
   exit_var(pred__2,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [65:1-65:183])_");
if(!(rel_75_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_31_delta_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_31_delta_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt)) && rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env1[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt)))) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__Sequence(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__2,stm),
   exit_var(pred__2,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [65:1-65:183])_");
if(!(rel_75_exit_var->empty()) && !(rel_23_delta_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)) && rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_23_delta_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__Sequence(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__2,stm),
   exit_var(pred__2,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [65:1-65:183])_");
if(!(rel_21_delta_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)) && rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_21_delta_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__If(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__3,stm),
   exit_var(pred__3,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [66:1-66:177])_");
if(!(rel_75_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_32_delta_input_exit_var->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt,rel_32_delta_input_exit_var->createContext());
for(const auto& env0 : *rel_32_delta_input_exit_var) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt)) && !(rel_31_delta_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt))) && rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env1[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt)))) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__If(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__3,stm),
   exit_var(pred__3,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [66:1-66:177])_");
if(!(rel_75_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_31_delta_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_31_delta_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt)) && rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env1[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt)))) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__If(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__3,stm),
   exit_var(pred__3,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [66:1-66:177])_");
if(!(rel_75_exit_var->empty()) && !(rel_23_delta_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)) && rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_23_delta_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__If(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__3,stm),
   exit_var(pred__3,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [66:1-66:177])_");
if(!(rel_21_delta_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)) && rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_21_delta_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__While(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__4,stm),
   exit_var(pred__4,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [67:1-67:180])_");
if(!(rel_75_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_32_delta_input_exit_var->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt,rel_32_delta_input_exit_var->createContext());
for(const auto& env0 : *rel_32_delta_input_exit_var) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt)) && !(rel_31_delta_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt))) && rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env1[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt)))) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__While(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__4,stm),
   exit_var(pred__4,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [67:1-67:180])_");
if(!(rel_75_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_31_delta_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_31_delta_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt)) && rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env1[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt)))) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__While(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__4,stm),
   exit_var(pred__4,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [67:1-67:180])_");
if(!(rel_75_exit_var->empty()) && !(rel_23_delta_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)) && rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_23_delta_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__While(stm),
   input__entry_var(stm,prog,x),
   flow(prog,pred__4,stm),
   exit_var(pred__4,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [67:1-67:180])_");
if(!(rel_21_delta_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)) && rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_21_delta_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,y,out__0) :- 
   input__exit_var(stm,prog,y),
   hasType__Assign(stm),
   path__Assign__0(stm,y),
   path__Assign__1(stm,exp),
   aeval(exp,stm,prog,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [62:1-62:177])_");
if(!(rel_74_aeval->empty()) && !(rel_108_path_Assign_1->empty()) && !(rel_107_path_Assign_0->empty()) && !(rel_32_delta_input_exit_var->empty()) && !(rel_84_hasType_Assign->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt,rel_84_hasType_Assign->createContext());
CREATE_OP_CONTEXT(rel_108_path_Assign_1_op_ctxt,rel_108_path_Assign_1->createContext());
CREATE_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt,rel_107_path_Assign_0->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt,rel_32_delta_input_exit_var->createContext());
for(const auto& env0 : *rel_32_delta_input_exit_var) {
if( rel_107_path_Assign_0->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt)) && rel_84_hasType_Assign->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt))) {
auto range = rel_108_path_Assign_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_108_path_Assign_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[0]), ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[0]), ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,y,out__0) :- 
   input__exit_var(stm,prog,y),
   hasType__Assign(stm),
   path__Assign__0(stm,y),
   path__Assign__1(stm,exp),
   aeval(exp,stm,prog,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [62:1-62:177])_");
if(!(rel_20_delta_aeval->empty()) && !(rel_108_path_Assign_1->empty()) && !(rel_107_path_Assign_0->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_84_hasType_Assign->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt,rel_84_hasType_Assign->createContext());
CREATE_OP_CONTEXT(rel_108_path_Assign_1_op_ctxt,rel_108_path_Assign_1->createContext());
CREATE_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt,rel_107_path_Assign_0->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_107_path_Assign_0->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt)) && rel_84_hasType_Assign->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt))) {
auto range = rel_108_path_Assign_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_108_path_Assign_1_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[0]), ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[0]), ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__Assign(stm),
   input__entry_var(stm,prog,x),
   x != y,
   path__Assign__0(stm,y),
   flow(prog,pred__0,stm),
   exit_var(pred__0,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [63:1-63:214])_");
if(!(rel_75_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_107_path_Assign_0->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_32_delta_input_exit_var->empty()) && !(rel_84_hasType_Assign->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt,rel_84_hasType_Assign->createContext());
CREATE_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt,rel_107_path_Assign_0->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt,rel_32_delta_input_exit_var->createContext());
for(const auto& env0 : *rel_32_delta_input_exit_var) {
if( rel_84_hasType_Assign->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt)) && !(rel_31_delta_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt))) && rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt))) {
auto range = rel_107_path_Assign_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt));
for(const auto& env1 : range) {
if( (ramBitCast<RamDomain>(env0[2]) != ramBitCast<RamDomain>(env1[1]))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env2[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt)))) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
break;
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__Assign(stm),
   input__entry_var(stm,prog,x),
   x != y,
   path__Assign__0(stm,y),
   flow(prog,pred__0,stm),
   exit_var(pred__0,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [63:1-63:214])_");
if(!(rel_75_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_107_path_Assign_0->empty()) && !(rel_31_delta_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_84_hasType_Assign->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt,rel_84_hasType_Assign->createContext());
CREATE_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt,rel_107_path_Assign_0->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_31_delta_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt)) && rel_84_hasType_Assign->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt))) {
auto range = rel_107_path_Assign_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt));
for(const auto& env1 : range) {
if( (ramBitCast<RamDomain>(env0[2]) != ramBitCast<RamDomain>(env1[1]))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env2[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt)))) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
}
break;
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__Assign(stm),
   input__entry_var(stm,prog,x),
   x != y,
   path__Assign__0(stm,y),
   flow(prog,pred__0,stm),
   exit_var(pred__0,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [63:1-63:214])_");
if(!(rel_75_exit_var->empty()) && !(rel_23_delta_flow->empty()) && !(rel_107_path_Assign_0->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_84_hasType_Assign->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt,rel_84_hasType_Assign->createContext());
CREATE_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt,rel_107_path_Assign_0->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)) && rel_84_hasType_Assign->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt))) {
auto range = rel_107_path_Assign_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt));
for(const auto& env1 : range) {
if( (ramBitCast<RamDomain>(env0[2]) != ramBitCast<RamDomain>(env1[1]))) {
auto range = rel_23_delta_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
break;
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(exit_var(stm,prog,x,out__0) :- 
   input__exit_var(stm,prog,x),
   hasType__Assign(stm),
   input__entry_var(stm,prog,x),
   x != y,
   path__Assign__0(stm,y),
   flow(prog,pred__0,stm),
   exit_var(pred__0,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [63:1-63:214])_");
if(!(rel_21_delta_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_107_path_Assign_0->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_99_input_exit_var->empty()) && !(rel_84_hasType_Assign->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt,rel_84_hasType_Assign->createContext());
CREATE_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt,rel_107_path_Assign_0->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
for(const auto& env0 : *rel_99_input_exit_var) {
if( rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)) && rel_84_hasType_Assign->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt))) {
auto range = rel_107_path_Assign_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt));
for(const auto& env1 : range) {
if( (ramBitCast<RamDomain>(env0[2]) != ramBitCast<RamDomain>(env1[1]))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_21_delta_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_75_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}};
rel_52_new_exit_var->insert(tuple,READ_OP_CONTEXT(rel_52_new_exit_var_op_ctxt));
}
}
}
break;
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(VNum(_0,[1,_0]) :- 
   input__VNum(_0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [57:1-57:51])_");
if(!(rel_29_delta_input_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_49_new_VNum_op_ctxt,rel_49_new_VNum->createContext());
CREATE_OP_CONTEXT(rel_29_delta_input_VNum_op_ctxt,rel_29_delta_input_VNum->createContext());
for(const auto& env0 : *rel_29_delta_input_VNum) {
if( !(rel_72_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(pack(recordTable,Tuple<RamDomain,2>{{ramBitCast(ramBitCast(RamSigned(1))),ramBitCast(ramBitCast(env0[0]))}}
))}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(pack(recordTable,Tuple<RamDomain,2>{{ramBitCast(ramBitCast(RamSigned(1))),ramBitCast(ramBitCast(env0[0]))}}
))}};
rel_49_new_VNum->insert(tuple,READ_OP_CONTEXT(rel_49_new_VNum_op_ctxt));
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(un___VBool(out,_0) :- 
   VBool(_0,out).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [54:1-54:39])_");
if(!(rel_17_delta_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_69_new_un_VBool_op_ctxt,rel_69_new_un_VBool->createContext());
for(const auto& env0 : *rel_17_delta_VBool) {
if( !(rel_122_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env0[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[1]),ramBitCast(env0[0])}};
rel_69_new_un_VBool->insert(tuple,READ_OP_CONTEXT(rel_69_new_un_VBool_op_ctxt));
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(input__freevars(exp__0) :- 
   input__freevars(exp),
   hasType__GreaterThan(exp),
   path__GreaterThan__0(exp,exp__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [108:1-108:111])_");
if(!(rel_109_path_GreaterThan_0->empty()) && !(rel_35_delta_input_freevars->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_102_input_freevars_op_ctxt,rel_102_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_35_delta_input_freevars_op_ctxt,rel_35_delta_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt,rel_66_new_input_freevars->createContext());
for(const auto& env0 : *rel_35_delta_input_freevars) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_102_input_freevars->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_102_input_freevars_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_66_new_input_freevars->insert(tuple,READ_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__freevars(exp__0) :- 
   input__freevars(exp),
   hasType__GreaterThan(exp),
   path__GreaterThan__1(exp,exp__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [109:1-109:111])_");
if(!(rel_110_path_GreaterThan_1->empty()) && !(rel_35_delta_input_freevars->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_102_input_freevars_op_ctxt,rel_102_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_35_delta_input_freevars_op_ctxt,rel_35_delta_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt,rel_66_new_input_freevars->createContext());
for(const auto& env0 : *rel_35_delta_input_freevars) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_102_input_freevars->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_102_input_freevars_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_66_new_input_freevars->insert(tuple,READ_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__freevars(exp__0) :- 
   input__freevars(exp),
   hasType__Add(exp),
   path__Add__0(exp,exp__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [110:1-110:95])_");
if(!(rel_105_path_Add_0->empty()) && !(rel_35_delta_input_freevars->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_102_input_freevars_op_ctxt,rel_102_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_35_delta_input_freevars_op_ctxt,rel_35_delta_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt,rel_66_new_input_freevars->createContext());
for(const auto& env0 : *rel_35_delta_input_freevars) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_102_input_freevars->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_102_input_freevars_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_66_new_input_freevars->insert(tuple,READ_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__freevars(exp__0) :- 
   input__freevars(exp),
   hasType__Add(exp),
   path__Add__1(exp,exp__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [111:1-111:95])_");
if(!(rel_106_path_Add_1->empty()) && !(rel_35_delta_input_freevars->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_102_input_freevars_op_ctxt,rel_102_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_35_delta_input_freevars_op_ctxt,rel_35_delta_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt,rel_66_new_input_freevars->createContext());
for(const auto& env0 : *rel_35_delta_input_freevars) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_102_input_freevars->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_102_input_freevars_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_66_new_input_freevars->insert(tuple,READ_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__freevars(exp__0) :- 
   input__freevarsStm(stm),
   hasType__Assign(stm),
   path__Assign__1(stm,exp__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [112:1-112:104])_");
if(!(rel_108_path_Assign_1->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_84_hasType_Assign->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt,rel_84_hasType_Assign->createContext());
CREATE_OP_CONTEXT(rel_108_path_Assign_1_op_ctxt,rel_108_path_Assign_1->createContext());
CREATE_OP_CONTEXT(rel_102_input_freevars_op_ctxt,rel_102_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt,rel_66_new_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_84_hasType_Assign->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt))) {
auto range = rel_108_path_Assign_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_108_path_Assign_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_102_input_freevars->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_102_input_freevars_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_66_new_input_freevars->insert(tuple,READ_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__freevars(exp__0) :- 
   input__freevarsStm(stm),
   hasType__If(stm),
   path__If__0(stm,exp__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [113:1-113:96])_");
if(!(rel_111_path_If_0->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_111_path_If_0_op_ctxt,rel_111_path_If_0->createContext());
CREATE_OP_CONTEXT(rel_102_input_freevars_op_ctxt,rel_102_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt,rel_66_new_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_111_path_If_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_111_path_If_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_102_input_freevars->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_102_input_freevars_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_66_new_input_freevars->insert(tuple,READ_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__freevars(exp__0) :- 
   input__freevarsStm(stm),
   hasType__While(stm),
   path__While__0(stm,exp__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [114:1-114:102])_");
if(!(rel_120_path_While_0->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_120_path_While_0_op_ctxt,rel_120_path_While_0->createContext());
CREATE_OP_CONTEXT(rel_102_input_freevars_op_ctxt,rel_102_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt,rel_66_new_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_120_path_While_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_120_path_While_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_102_input_freevars->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_102_input_freevars_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_66_new_input_freevars->insert(tuple,READ_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt));
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   n1 > n2,
   eval__0 = 1,
   VBool(eval__0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [77:1-77:331])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_71_VBool->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamDomain>(env5[1]) != ramBitCast<RamDomain>(env6[1])) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(1)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(1)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt))) && !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   n1 > n2,
   eval__0 = 1,
   VBool(eval__0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [77:1-77:331])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_71_VBool->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamDomain>(env5[1]) != ramBitCast<RamDomain>(env6[1])) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(1)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(1)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt))) && !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   n1 > n2,
   eval__0 = 1,
   VBool(eval__0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [77:1-77:331])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_71_VBool->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamDomain>(env5[1]) != ramBitCast<RamDomain>(env6[1])) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(1)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(1)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt))) && !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   n1 > n2,
   eval__0 = 1,
   VBool(eval__0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [77:1-77:331])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_71_VBool->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamDomain>(env5[1]) != ramBitCast<RamDomain>(env6[1])) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(1)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(1)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt))) && !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   n1 > n2,
   eval__0 = 1,
   VBool(eval__0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [77:1-77:331])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_71_VBool->empty()) && !(rel_39_delta_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_39_delta_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamDomain>(env5[1]) != ramBitCast<RamDomain>(env6[1]))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(1)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(1)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt))) && !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   n1 > n2,
   eval__0 = 1,
   VBool(eval__0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [77:1-77:331])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_17_delta_VBool->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamDomain>(env5[1]) != ramBitCast<RamDomain>(env6[1]))) {
auto range = rel_17_delta_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(1)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(1)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   n1 <= n2,
   eval__0 = 0,
   VBool(eval__0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [78:1-78:332])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_71_VBool->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt))) && !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   n1 <= n2,
   eval__0 = 0,
   VBool(eval__0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [78:1-78:332])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_71_VBool->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt))) && !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   n1 <= n2,
   eval__0 = 0,
   VBool(eval__0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [78:1-78:332])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_71_VBool->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt))) && !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   n1 <= n2,
   eval__0 = 0,
   VBool(eval__0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [78:1-78:332])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_71_VBool->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt))) && !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   n1 <= n2,
   eval__0 = 0,
   VBool(eval__0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [78:1-78:332])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_71_VBool->empty()) && !(rel_39_delta_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_39_delta_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env6 : range) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt))) && !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   n1 <= n2,
   eval__0 = 0,
   VBool(eval__0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [78:1-78:332])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_17_delta_VBool->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
auto range = rel_17_delta_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__GreaterThan(exp__1),
   path__GreaterThan__0(exp__1,e1__1),
   path__GreaterThan__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1),
   aeval(e2__1,node__1,prog__1,v2),
   un___VNum(v1,_),
   un___VBool(v2,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [79:1-79:304])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__GreaterThan(exp__1),
   path__GreaterThan__0(exp__1,e1__1),
   path__GreaterThan__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1),
   aeval(e2__1,node__1,prog__1,v2),
   un___VNum(v1,_),
   un___VBool(v2,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [79:1-79:304])_");
if(!(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty()) && !(rel_123_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__GreaterThan(exp__1),
   path__GreaterThan__0(exp__1,e1__1),
   path__GreaterThan__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1),
   aeval(e2__1,node__1,prog__1,v2),
   un___VNum(v1,_),
   un___VBool(v2,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [79:1-79:304])_");
if(!(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty()) && !(rel_123_un_VNum->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
break;
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__GreaterThan(exp__1),
   path__GreaterThan__0(exp__1,e1__1),
   path__GreaterThan__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1),
   aeval(e2__1,node__1,prog__1,v2),
   un___VNum(v1,_),
   un___VBool(v2,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [79:1-79:304])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__GreaterThan(exp__1),
   path__GreaterThan__0(exp__1,e1__1),
   path__GreaterThan__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1),
   aeval(e2__1,node__1,prog__1,v2),
   un___VNum(v1,_),
   un___VBool(v2,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [79:1-79:304])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_71_VBool->empty()) && !(rel_38_delta_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !rel_38_delta_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)).empty()) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__GreaterThan(exp__1),
   path__GreaterThan__0(exp__1,e1__1),
   path__GreaterThan__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1),
   aeval(e2__1,node__1,prog__1,v2),
   un___VNum(v1,_),
   un___VBool(v2,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [79:1-79:304])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_17_delta_VBool->empty()) && !(rel_122_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt)).empty()) {
auto range = rel_17_delta_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__GreaterThan(exp__2),
   path__GreaterThan__0(exp__2,e1__2),
   path__GreaterThan__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1),
   aeval(e2__2,node__2,prog__2,v2),
   un___VBool(v1,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [80:1-80:285])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__GreaterThan(exp__2),
   path__GreaterThan__0(exp__2,e1__2),
   path__GreaterThan__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1),
   aeval(e2__2,node__2,prog__2,v2),
   un___VBool(v1,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [80:1-80:285])_");
if(!(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__GreaterThan(exp__2),
   path__GreaterThan__0(exp__2,e1__2),
   path__GreaterThan__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1),
   aeval(e2__2,node__2,prog__2,v2),
   un___VBool(v1,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [80:1-80:285])_");
if(!(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__GreaterThan(exp__2),
   path__GreaterThan__0(exp__2,e1__2),
   path__GreaterThan__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1),
   aeval(e2__2,node__2,prog__2,v2),
   un___VBool(v1,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [80:1-80:285])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_71_VBool->empty()) && !(rel_38_delta_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !rel_38_delta_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(greaterThan(v1,v2,out__0) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__GreaterThan(exp__2),
   path__GreaterThan__0(exp__2,e1__2),
   path__GreaterThan__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1),
   aeval(e2__2,node__2,prog__2,v2),
   un___VBool(v1,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [80:1-80:285])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_17_delta_VBool->empty()) && !(rel_122_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_17_delta_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_82_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}};
rel_57_new_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt));
}
}
}
}
}
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(un___VNum(out,_0) :- 
   VNum(_0,out).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [59:1-59:37])_");
if(!(rel_18_delta_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_70_new_un_VNum_op_ctxt,rel_70_new_un_VNum->createContext());
for(const auto& env0 : *rel_18_delta_VNum) {
if( !(rel_123_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env0[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt)))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[1]),ramBitCast(env0[0])}};
rel_70_new_un_VNum->insert(tuple,READ_OP_CONTEXT(rel_70_new_un_VNum_op_ctxt));
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(input__flow(stm__0) :- 
   input__flow(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [101:1-101:97])_");
if(!(rel_115_path_Sequence_0->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
CREATE_OP_CONTEXT(rel_65_new_input_flow_op_ctxt,rel_65_new_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_101_input_flow->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_101_input_flow_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_65_new_input_flow->insert(tuple,READ_OP_CONTEXT(rel_65_new_input_flow_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__flow(stm__0) :- 
   input__flow(stm),
   hasType__Sequence(stm),
   path__Sequence__1(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [102:1-102:97])_");
if(!(rel_116_path_Sequence_1->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
CREATE_OP_CONTEXT(rel_65_new_input_flow_op_ctxt,rel_65_new_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_101_input_flow->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_101_input_flow_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_65_new_input_flow->insert(tuple,READ_OP_CONTEXT(rel_65_new_input_flow_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__flow(stm__0) :- 
   input__flow(stm),
   hasType__If(stm),
   path__If__1(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [103:1-103:85])_");
if(!(rel_112_path_If_1->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_112_path_If_1_op_ctxt,rel_112_path_If_1->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
CREATE_OP_CONTEXT(rel_65_new_input_flow_op_ctxt,rel_65_new_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_112_path_If_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_112_path_If_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_101_input_flow->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_101_input_flow_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_65_new_input_flow->insert(tuple,READ_OP_CONTEXT(rel_65_new_input_flow_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__flow(stm__0) :- 
   input__flow(stm),
   hasType__If(stm),
   path__If__2(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [104:1-104:85])_");
if(!(rel_113_path_If_2->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_113_path_If_2_op_ctxt,rel_113_path_If_2->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
CREATE_OP_CONTEXT(rel_65_new_input_flow_op_ctxt,rel_65_new_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_113_path_If_2->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_113_path_If_2_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_101_input_flow->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_101_input_flow_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_65_new_input_flow->insert(tuple,READ_OP_CONTEXT(rel_65_new_input_flow_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__flow(stm__0) :- 
   input__flow(stm),
   hasType__While(stm),
   path__While__1(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [105:1-105:91])_");
if(!(rel_121_path_While_1->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_121_path_While_1_op_ctxt,rel_121_path_While_1->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
CREATE_OP_CONTEXT(rel_65_new_input_flow_op_ctxt,rel_65_new_input_flow->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_121_path_While_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_121_path_While_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_101_input_flow->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_101_input_flow_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_65_new_input_flow->insert(tuple,READ_OP_CONTEXT(rel_65_new_input_flow_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__flow(stm__0) :- 
   input__entry_var(_,stm__0,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [106:1-106:57])_");
if(!(rel_31_delta_input_entry_var->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
CREATE_OP_CONTEXT(rel_65_new_input_flow_op_ctxt,rel_65_new_input_flow->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
for(const auto& env0 : *rel_31_delta_input_entry_var) {
if( !(rel_101_input_flow->contains(Tuple<RamDomain,1>{{ramBitCast(env0[1])}},READ_OP_CONTEXT(rel_101_input_flow_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[1])}};
rel_65_new_input_flow->insert(tuple,READ_OP_CONTEXT(rel_65_new_input_flow_op_ctxt));
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__Num(exp),
   path__Num__0(exp,i),
   VNum(i,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [72:1-72:123])_");
if(!(rel_72_VNum->empty()) && !(rel_114_path_Num_0->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_87_hasType_Num->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_87_hasType_Num_op_ctxt,rel_87_hasType_Num->createContext());
CREATE_OP_CONTEXT(rel_114_path_Num_0_op_ctxt,rel_114_path_Num_0->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_87_hasType_Num->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_87_hasType_Num_op_ctxt))) {
auto range = rel_114_path_Num_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_114_path_Num_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt))) && !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[1])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__Num(exp),
   path__Num__0(exp,i),
   VNum(i,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [72:1-72:123])_");
if(!(rel_18_delta_VNum->empty()) && !(rel_114_path_Num_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_87_hasType_Num->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_87_hasType_Num_op_ctxt,rel_87_hasType_Num->createContext());
CREATE_OP_CONTEXT(rel_114_path_Num_0_op_ctxt,rel_114_path_Num_0->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_87_hasType_Num->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_87_hasType_Num_op_ctxt))) {
auto range = rel_114_path_Num_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_114_path_Num_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_18_delta_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env2[1])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__Var(exp),
   path__Var__0(exp,x),
   input__entry_var(node,prog,x),
   flow(prog,pred__0,node),
   exit_var(pred__0,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [73:1-73:202])_");
if(!(rel_75_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_119_path_Var_0->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_92_hasType_Var->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_92_hasType_Var_op_ctxt,rel_92_hasType_Var->createContext());
CREATE_OP_CONTEXT(rel_119_path_Var_0_op_ctxt,rel_119_path_Var_0->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_92_hasType_Var->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_92_hasType_Var_op_ctxt))) {
auto range = rel_119_path_Var_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_119_path_Var_0_op_ctxt));
for(const auto& env1 : range) {
if( rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)) && !(rel_31_delta_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt)))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[1])}},Tuple<RamDomain,3>{{ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[1])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[2]),ramBitCast(env2[1]),ramBitCast(env0[1])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt)))) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[2]), ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[2]), ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[2]),ramBitCast(env1[1]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__Var(exp),
   path__Var__0(exp,x),
   input__entry_var(node,prog,x),
   flow(prog,pred__0,node),
   exit_var(pred__0,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [73:1-73:202])_");
if(!(rel_75_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_31_delta_input_entry_var->empty()) && !(rel_119_path_Var_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_92_hasType_Var->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_92_hasType_Var_op_ctxt,rel_92_hasType_Var->createContext());
CREATE_OP_CONTEXT(rel_119_path_Var_0_op_ctxt,rel_119_path_Var_0->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_92_hasType_Var->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_92_hasType_Var_op_ctxt))) {
auto range = rel_119_path_Var_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_119_path_Var_0_op_ctxt));
for(const auto& env1 : range) {
if( rel_31_delta_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[1])}},Tuple<RamDomain,3>{{ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[1])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[2]),ramBitCast(env2[1]),ramBitCast(env0[1])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt)))) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[2]), ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[2]), ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[2]),ramBitCast(env1[1]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__Var(exp),
   path__Var__0(exp,x),
   input__entry_var(node,prog,x),
   flow(prog,pred__0,node),
   exit_var(pred__0,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [73:1-73:202])_");
if(!(rel_75_exit_var->empty()) && !(rel_23_delta_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_119_path_Var_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_92_hasType_Var->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_92_hasType_Var_op_ctxt,rel_92_hasType_Var->createContext());
CREATE_OP_CONTEXT(rel_119_path_Var_0_op_ctxt,rel_119_path_Var_0->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_92_hasType_Var->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_92_hasType_Var_op_ctxt))) {
auto range = rel_119_path_Var_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_119_path_Var_0_op_ctxt));
for(const auto& env1 : range) {
if( rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt))) {
auto range = rel_23_delta_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[1])}},Tuple<RamDomain,3>{{ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[1])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[2]), ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[2]), ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_21_delta_exit_var->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[2]),ramBitCast(env1[1]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt))) && !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__Var(exp),
   path__Var__0(exp,x),
   input__entry_var(node,prog,x),
   flow(prog,pred__0,node),
   exit_var(pred__0,prog,x,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [73:1-73:202])_");
if(!(rel_21_delta_exit_var->empty()) && !(rel_79_flow->empty()) && !(rel_98_input_entry_var->empty()) && !(rel_119_path_Var_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_92_hasType_Var->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_92_hasType_Var_op_ctxt,rel_92_hasType_Var->createContext());
CREATE_OP_CONTEXT(rel_119_path_Var_0_op_ctxt,rel_119_path_Var_0->createContext());
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt,rel_21_delta_exit_var->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_92_hasType_Var->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_92_hasType_Var_op_ctxt))) {
auto range = rel_119_path_Var_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_119_path_Var_0_op_ctxt));
for(const auto& env1 : range) {
if( rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt))) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[1])}},Tuple<RamDomain,3>{{ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[1])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_21_delta_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[2]), ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[2]), ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_21_delta_exit_var_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__GreaterThan(exp),
   path__GreaterThan__0(exp,e1),
   path__GreaterThan__1(exp,e2),
   aeval(e1,node,prog,v1),
   aeval(e2,node,prog,v2),
   greaterThan(v1,v2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [74:1-74:237])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_82_greaterThan->empty()) && !(rel_74_aeval->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_26_delta_greaterThan_op_ctxt,rel_26_delta_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_82_greaterThan->lowerUpperRange_110(Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_26_delta_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_26_delta_greaterThan_op_ctxt))) && !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__GreaterThan(exp),
   path__GreaterThan__0(exp,e1),
   path__GreaterThan__1(exp,e2),
   aeval(e1,node,prog,v1),
   aeval(e2,node,prog,v2),
   greaterThan(v1,v2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [74:1-74:237])_");
if(!(rel_82_greaterThan->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_26_delta_greaterThan_op_ctxt,rel_26_delta_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_82_greaterThan->lowerUpperRange_110(Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_26_delta_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_26_delta_greaterThan_op_ctxt))) && !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__GreaterThan(exp),
   path__GreaterThan__0(exp,e1),
   path__GreaterThan__1(exp,e2),
   aeval(e1,node,prog,v1),
   aeval(e2,node,prog,v2),
   greaterThan(v1,v2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [74:1-74:237])_");
if(!(rel_82_greaterThan->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_26_delta_greaterThan_op_ctxt,rel_26_delta_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_82_greaterThan->lowerUpperRange_110(Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_26_delta_greaterThan->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_26_delta_greaterThan_op_ctxt))) && !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__GreaterThan(exp),
   path__GreaterThan__0(exp,e1),
   path__GreaterThan__1(exp,e2),
   aeval(e1,node,prog,v1),
   aeval(e2,node,prog,v2),
   greaterThan(v1,v2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [74:1-74:237])_");
if(!(rel_85_hasType_GreaterThan->empty()) && !(rel_97_input_aeval->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_26_delta_greaterThan->empty()) && !(rel_74_aeval->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_26_delta_greaterThan_op_ctxt,rel_26_delta_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_26_delta_greaterThan->lowerUpperRange_110(Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_26_delta_greaterThan_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__Add(exp),
   path__Add__0(exp,e1),
   path__Add__1(exp,e2),
   aeval(e1,node,prog,v1),
   aeval(e2,node,prog,v2),
   add(v1,v2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [75:1-75:205])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_73_add->empty()) && !(rel_74_aeval->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_19_delta_add_op_ctxt,rel_19_delta_add->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_73_add->lowerUpperRange_110(Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_73_add_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_19_delta_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_19_delta_add_op_ctxt))) && !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__Add(exp),
   path__Add__0(exp,e1),
   path__Add__1(exp,e2),
   aeval(e1,node,prog,v1),
   aeval(e2,node,prog,v2),
   add(v1,v2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [75:1-75:205])_");
if(!(rel_73_add->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_19_delta_add_op_ctxt,rel_19_delta_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_73_add->lowerUpperRange_110(Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_73_add_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_19_delta_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_19_delta_add_op_ctxt))) && !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__Add(exp),
   path__Add__0(exp,e1),
   path__Add__1(exp,e2),
   aeval(e1,node,prog,v1),
   aeval(e2,node,prog,v2),
   add(v1,v2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [75:1-75:205])_");
if(!(rel_73_add->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_19_delta_add_op_ctxt,rel_19_delta_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_73_add->lowerUpperRange_110(Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_73_add_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_19_delta_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_19_delta_add_op_ctxt))) && !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(aeval(exp,node,prog,out__0) :- 
   input__aeval(exp,node,prog),
   hasType__Add(exp),
   path__Add__0(exp,e1),
   path__Add__1(exp,e2),
   aeval(e1,node,prog,v1),
   aeval(e2,node,prog,v2),
   add(v1,v2,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [75:1-75:205])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_19_delta_add->empty()) && !(rel_74_aeval->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
CREATE_OP_CONTEXT(rel_19_delta_add_op_ctxt,rel_19_delta_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_19_delta_add->lowerUpperRange_110(Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,3>{{ramBitCast(env3[3]), ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_19_delta_add_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_74_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)))) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env5[2])}};
rel_51_new_aeval->insert(tuple,READ_OP_CONTEXT(rel_51_new_aeval_op_ctxt));
}
}
}
}
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(input__VBool(1) :- 
   +disconnected0().
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [123:1-123:309])_");
if(!(rel_9_delta_disconnected0->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_95_input_VBool_op_ctxt,rel_95_input_VBool->createContext());
CREATE_OP_CONTEXT(rel_59_new_input_VBool_op_ctxt,rel_59_new_input_VBool->createContext());
if(!(rel_95_input_VBool->contains(Tuple<RamDomain,1>{{ramBitCast(RamUnsigned(1))}},READ_OP_CONTEXT(rel_95_input_VBool_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(RamUnsigned(1))}};
rel_59_new_input_VBool->insert(tuple,READ_OP_CONTEXT(rel_59_new_input_VBool_op_ctxt));
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__VBool(0) :- 
   +disconnected1().
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [124:1-124:310])_");
if(!(rel_10_delta_disconnected1->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_95_input_VBool_op_ctxt,rel_95_input_VBool->createContext());
CREATE_OP_CONTEXT(rel_59_new_input_VBool_op_ctxt,rel_59_new_input_VBool->createContext());
if(!(rel_95_input_VBool->contains(Tuple<RamDomain,1>{{ramBitCast(RamUnsigned(0))}},READ_OP_CONTEXT(rel_95_input_VBool_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(RamUnsigned(0))}};
rel_59_new_input_VBool->insert(tuple,READ_OP_CONTEXT(rel_59_new_input_VBool_op_ctxt));
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__VBool(0) :- 
   +disconnected2().
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [125:1-125:301])_");
if(!(rel_11_delta_disconnected2->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_95_input_VBool_op_ctxt,rel_95_input_VBool->createContext());
CREATE_OP_CONTEXT(rel_59_new_input_VBool_op_ctxt,rel_59_new_input_VBool->createContext());
if(!(rel_95_input_VBool->contains(Tuple<RamDomain,1>{{ramBitCast(RamUnsigned(0))}},READ_OP_CONTEXT(rel_95_input_VBool_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(RamUnsigned(0))}};
rel_59_new_input_VBool->insert(tuple,READ_OP_CONTEXT(rel_59_new_input_VBool_op_ctxt));
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__VBool(0) :- 
   +disconnected3().
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [126:1-126:279])_");
if(!(rel_12_delta_disconnected3->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_95_input_VBool_op_ctxt,rel_95_input_VBool->createContext());
CREATE_OP_CONTEXT(rel_59_new_input_VBool_op_ctxt,rel_59_new_input_VBool->createContext());
if(!(rel_95_input_VBool->contains(Tuple<RamDomain,1>{{ramBitCast(RamUnsigned(0))}},READ_OP_CONTEXT(rel_95_input_VBool_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(RamUnsigned(0))}};
rel_59_new_input_VBool->insert(tuple,READ_OP_CONTEXT(rel_59_new_input_VBool_op_ctxt));
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__VBool(0) :- 
   +disconnected4().
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [127:1-127:277])_");
if(!(rel_13_delta_disconnected4->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_95_input_VBool_op_ctxt,rel_95_input_VBool->createContext());
CREATE_OP_CONTEXT(rel_59_new_input_VBool_op_ctxt,rel_59_new_input_VBool->createContext());
if(!(rel_95_input_VBool->contains(Tuple<RamDomain,1>{{ramBitCast(RamUnsigned(0))}},READ_OP_CONTEXT(rel_95_input_VBool_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(RamUnsigned(0))}};
rel_59_new_input_VBool->insert(tuple,READ_OP_CONTEXT(rel_59_new_input_VBool_op_ctxt));
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__VBool(0) :- 
   +disconnected5().
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [128:1-128:255])_");
if(!(rel_14_delta_disconnected5->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_95_input_VBool_op_ctxt,rel_95_input_VBool->createContext());
CREATE_OP_CONTEXT(rel_59_new_input_VBool_op_ctxt,rel_59_new_input_VBool->createContext());
if(!(rel_95_input_VBool->contains(Tuple<RamDomain,1>{{ramBitCast(RamUnsigned(0))}},READ_OP_CONTEXT(rel_95_input_VBool_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(RamUnsigned(0))}};
rel_59_new_input_VBool->insert(tuple,READ_OP_CONTEXT(rel_59_new_input_VBool_op_ctxt));
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(input__init(stm__0) :- 
   input__init(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [88:1-88:97])_");
if(!(rel_115_path_Sequence_0->empty()) && !(rel_37_delta_input_init->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_104_input_init_op_ctxt,rel_104_input_init->createContext());
CREATE_OP_CONTEXT(rel_37_delta_input_init_op_ctxt,rel_37_delta_input_init->createContext());
CREATE_OP_CONTEXT(rel_68_new_input_init_op_ctxt,rel_68_new_input_init->createContext());
for(const auto& env0 : *rel_37_delta_input_init) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_104_input_init->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_104_input_init_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_68_new_input_init->insert(tuple,READ_OP_CONTEXT(rel_68_new_input_init_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__init(stm__0) :- 
   input__flow(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,s1),
   path__Sequence__1(stm,stm__0),
   final(s1,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [89:1-89:140])_");
if(!(rel_77_final->empty()) && !(rel_116_path_Sequence_1->empty()) && !(rel_115_path_Sequence_0->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
CREATE_OP_CONTEXT(rel_104_input_init_op_ctxt,rel_104_input_init->createContext());
CREATE_OP_CONTEXT(rel_68_new_input_init_op_ctxt,rel_68_new_input_init->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_104_input_init->contains(Tuple<RamDomain,1>{{ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_104_input_init_op_ctxt)))) {
auto range = rel_77_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_77_final_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_22_delta_final->contains(Tuple<RamDomain,2>{{ramBitCast(env1[1]),ramBitCast(env3[1])}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env2[1])}};
rel_68_new_input_init->insert(tuple,READ_OP_CONTEXT(rel_68_new_input_init_op_ctxt));
break;
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__init(stm__0) :- 
   input__flow(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,s1),
   path__Sequence__1(stm,stm__0),
   final(s1,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [89:1-89:140])_");
if(!(rel_22_delta_final->empty()) && !(rel_116_path_Sequence_1->empty()) && !(rel_115_path_Sequence_0->empty()) && !(rel_101_input_flow->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
CREATE_OP_CONTEXT(rel_104_input_init_op_ctxt,rel_104_input_init->createContext());
CREATE_OP_CONTEXT(rel_68_new_input_init_op_ctxt,rel_68_new_input_init->createContext());
for(const auto& env0 : *rel_101_input_flow) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
if( !rel_22_delta_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env1[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt)).empty()) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_104_input_init->contains(Tuple<RamDomain,1>{{ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_104_input_init_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env2[1])}};
rel_68_new_input_init->insert(tuple,READ_OP_CONTEXT(rel_68_new_input_init_op_ctxt));
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__init(stm__0) :- 
   input__flow(stm),
   hasType__If(stm),
   path__If__1(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [90:1-90:85])_");
if(!(rel_112_path_If_1->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_112_path_If_1_op_ctxt,rel_112_path_If_1->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
CREATE_OP_CONTEXT(rel_104_input_init_op_ctxt,rel_104_input_init->createContext());
CREATE_OP_CONTEXT(rel_68_new_input_init_op_ctxt,rel_68_new_input_init->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_112_path_If_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_112_path_If_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_104_input_init->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_104_input_init_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_68_new_input_init->insert(tuple,READ_OP_CONTEXT(rel_68_new_input_init_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__init(stm__0) :- 
   input__flow(stm),
   hasType__If(stm),
   path__If__2(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [91:1-91:85])_");
if(!(rel_113_path_If_2->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_113_path_If_2_op_ctxt,rel_113_path_If_2->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
CREATE_OP_CONTEXT(rel_104_input_init_op_ctxt,rel_104_input_init->createContext());
CREATE_OP_CONTEXT(rel_68_new_input_init_op_ctxt,rel_68_new_input_init->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_113_path_If_2->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_113_path_If_2_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_104_input_init->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_104_input_init_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_68_new_input_init->insert(tuple,READ_OP_CONTEXT(rel_68_new_input_init_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__init(stm__0) :- 
   input__flow(stm),
   hasType__While(stm),
   path__While__1(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [92:1-92:91])_");
if(!(rel_121_path_While_1->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_121_path_While_1_op_ctxt,rel_121_path_While_1->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
CREATE_OP_CONTEXT(rel_104_input_init_op_ctxt,rel_104_input_init->createContext());
CREATE_OP_CONTEXT(rel_68_new_input_init_op_ctxt,rel_68_new_input_init->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_121_path_While_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_121_path_While_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_104_input_init->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_104_input_init_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_68_new_input_init->insert(tuple,READ_OP_CONTEXT(rel_68_new_input_init_op_ctxt));
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__Add(exp__0),
   path__Add__0(exp__0,e1__0),
   path__Add__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) <= -100,
   VNum(-1000,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [82:1-82:291])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_72_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) <= ramBitCast<RamSigned>(RamSigned(-100))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__Add(exp__0),
   path__Add__0(exp__0,e1__0),
   path__Add__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) <= -100,
   VNum(-1000,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [82:1-82:291])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_72_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) <= ramBitCast<RamSigned>(RamSigned(-100))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__Add(exp__0),
   path__Add__0(exp__0,e1__0),
   path__Add__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) <= -100,
   VNum(-1000,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [82:1-82:291])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_72_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) <= ramBitCast<RamSigned>(RamSigned(-100))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__Add(exp__0),
   path__Add__0(exp__0,e1__0),
   path__Add__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) <= -100,
   VNum(-1000,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [82:1-82:291])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_72_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) <= ramBitCast<RamSigned>(RamSigned(-100))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__Add(exp__0),
   path__Add__0(exp__0,e1__0),
   path__Add__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) <= -100,
   VNum(-1000,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [82:1-82:291])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_72_VNum->empty()) && !(rel_39_delta_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) <= ramBitCast<RamSigned>(RamSigned(-100)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__Add(exp__0),
   path__Add__0(exp__0,e1__0),
   path__Add__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1),
   aeval(e2__0,node__0,prog__0,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) <= -100,
   VNum(-1000,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [82:1-82:291])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_18_delta_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) <= ramBitCast<RamSigned>(RamSigned(-100)))) {
auto range = rel_18_delta_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamSigned(-1000)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__Add(exp__1),
   path__Add__0(exp__1,e1__1),
   path__Add__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1),
   aeval(e2__1,node__1,prog__1,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) > -100,
   (n1+n2) >= 100,
   VNum(1000,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [83:1-83:305])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_72_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) >= ramBitCast<RamSigned>(RamSigned(100)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__Add(exp__1),
   path__Add__0(exp__1,e1__1),
   path__Add__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1),
   aeval(e2__1,node__1,prog__1,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) > -100,
   (n1+n2) >= 100,
   VNum(1000,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [83:1-83:305])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_72_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) >= ramBitCast<RamSigned>(RamSigned(100)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__Add(exp__1),
   path__Add__0(exp__1,e1__1),
   path__Add__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1),
   aeval(e2__1,node__1,prog__1,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) > -100,
   (n1+n2) >= 100,
   VNum(1000,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [83:1-83:305])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_72_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) >= ramBitCast<RamSigned>(RamSigned(100)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__Add(exp__1),
   path__Add__0(exp__1,e1__1),
   path__Add__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1),
   aeval(e2__1,node__1,prog__1,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) > -100,
   (n1+n2) >= 100,
   VNum(1000,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [83:1-83:305])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_72_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) >= ramBitCast<RamSigned>(RamSigned(100)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__Add(exp__1),
   path__Add__0(exp__1,e1__1),
   path__Add__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1),
   aeval(e2__1,node__1,prog__1,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) > -100,
   (n1+n2) >= 100,
   VNum(1000,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [83:1-83:305])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_72_VNum->empty()) && !(rel_39_delta_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) >= ramBitCast<RamSigned>(RamSigned(100)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__Add(exp__1),
   path__Add__0(exp__1,e1__1),
   path__Add__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1),
   aeval(e2__1,node__1,prog__1,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) > -100,
   (n1+n2) >= 100,
   VNum(1000,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [83:1-83:305])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_18_delta_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) >= ramBitCast<RamSigned>(RamSigned(100)))) {
auto range = rel_18_delta_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamSigned(1000)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__Add(exp__2),
   path__Add__0(exp__2,e1__2),
   path__Add__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1),
   aeval(e2__2,node__2,prog__2,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) > -100,
   (n1+n2) < 100,
   eval__4 = (n1+n2),
   VNum(eval__4,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [84:1-84:326])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_72_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) < ramBitCast<RamSigned>(RamSigned(100)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt))) && !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__Add(exp__2),
   path__Add__0(exp__2,e1__2),
   path__Add__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1),
   aeval(e2__2,node__2,prog__2,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) > -100,
   (n1+n2) < 100,
   eval__4 = (n1+n2),
   VNum(eval__4,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [84:1-84:326])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_72_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) < ramBitCast<RamSigned>(RamSigned(100)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt))) && !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__Add(exp__2),
   path__Add__0(exp__2,e1__2),
   path__Add__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1),
   aeval(e2__2,node__2,prog__2,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) > -100,
   (n1+n2) < 100,
   eval__4 = (n1+n2),
   VNum(eval__4,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [84:1-84:326])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_72_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) < ramBitCast<RamSigned>(RamSigned(100)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt))) && !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__Add(exp__2),
   path__Add__0(exp__2,e1__2),
   path__Add__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1),
   aeval(e2__2,node__2,prog__2,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) > -100,
   (n1+n2) < 100,
   eval__4 = (n1+n2),
   VNum(eval__4,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [84:1-84:326])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_72_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) < ramBitCast<RamSigned>(RamSigned(100)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt))) && !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__Add(exp__2),
   path__Add__0(exp__2,e1__2),
   path__Add__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1),
   aeval(e2__2,node__2,prog__2,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) > -100,
   (n1+n2) < 100,
   eval__4 = (n1+n2),
   VNum(eval__4,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [84:1-84:326])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_72_VNum->empty()) && !(rel_39_delta_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) < ramBitCast<RamSigned>(RamSigned(100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100)))) {
auto range = rel_72_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_18_delta_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env7[0]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt))) && !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__Add(exp__2),
   path__Add__0(exp__2,e1__2),
   path__Add__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1),
   aeval(e2__2,node__2,prog__2,v2),
   un___VNum(v1,n1),
   un___VNum(v2,n2),
   (n1+n2) > -100,
   (n1+n2) < 100,
   eval__4 = (n1+n2),
   VNum(eval__4,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [84:1-84:326])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_18_delta_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_18_delta_VNum_op_ctxt,rel_18_delta_VNum->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) < ramBitCast<RamSigned>(RamSigned(100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100)))) {
auto range = rel_18_delta_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_18_delta_VNum_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__3,node__3,prog__3),
   hasType__Add(exp__3),
   path__Add__0(exp__3,e1__3),
   path__Add__1(exp__3,e2__3),
   aeval(e1__3,node__3,prog__3,v1),
   aeval(e2__3,node__3,prog__3,v2),
   un___VNum(v1,_),
   un___VBool(v2,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [85:1-85:272])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__3,node__3,prog__3),
   hasType__Add(exp__3),
   path__Add__0(exp__3,e1__3),
   path__Add__1(exp__3,e2__3),
   aeval(e1__3,node__3,prog__3,v1),
   aeval(e2__3,node__3,prog__3,v2),
   un___VNum(v1,_),
   un___VBool(v2,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [85:1-85:272])_");
if(!(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty()) && !(rel_123_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__3,node__3,prog__3),
   hasType__Add(exp__3),
   path__Add__0(exp__3,e1__3),
   path__Add__1(exp__3,e2__3),
   aeval(e1__3,node__3,prog__3,v1),
   aeval(e2__3,node__3,prog__3,v2),
   un___VNum(v1,_),
   un___VBool(v2,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [85:1-85:272])_");
if(!(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty()) && !(rel_123_un_VNum->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env7 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env7[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env7[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
break;
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__3,node__3,prog__3),
   hasType__Add(exp__3),
   path__Add__0(exp__3,e1__3),
   path__Add__1(exp__3,e2__3),
   aeval(e1__3,node__3,prog__3,v1),
   aeval(e2__3,node__3,prog__3,v2),
   un___VNum(v1,_),
   un___VBool(v2,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [85:1-85:272])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__3,node__3,prog__3),
   hasType__Add(exp__3),
   path__Add__0(exp__3,e1__3),
   path__Add__1(exp__3,e2__3),
   aeval(e1__3,node__3,prog__3,v1),
   aeval(e2__3,node__3,prog__3,v2),
   un___VNum(v1,_),
   un___VBool(v2,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [85:1-85:272])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_71_VBool->empty()) && !(rel_38_delta_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !rel_38_delta_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)).empty()) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__3,node__3,prog__3),
   hasType__Add(exp__3),
   path__Add__0(exp__3,e1__3),
   path__Add__1(exp__3,e2__3),
   aeval(e1__3,node__3,prog__3,v1),
   aeval(e2__3,node__3,prog__3,v2),
   un___VNum(v1,_),
   un___VBool(v2,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [85:1-85:272])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_17_delta_VBool->empty()) && !(rel_122_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt)).empty()) {
auto range = rel_17_delta_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__4,node__4,prog__4),
   hasType__Add(exp__4),
   path__Add__0(exp__4,e1__4),
   path__Add__1(exp__4,e2__4),
   aeval(e1__4,node__4,prog__4,v1),
   aeval(e2__4,node__4,prog__4,v2),
   un___VBool(v1,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [86:1-86:253])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__4,node__4,prog__4),
   hasType__Add(exp__4),
   path__Add__0(exp__4,e1__4),
   path__Add__1(exp__4,e2__4),
   aeval(e1__4,node__4,prog__4,v1),
   aeval(e2__4,node__4,prog__4,v2),
   un___VBool(v1,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [86:1-86:253])_");
if(!(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__4,node__4,prog__4),
   hasType__Add(exp__4),
   path__Add__0(exp__4,e1__4),
   path__Add__1(exp__4,e2__4),
   aeval(e1__4,node__4,prog__4,v1),
   aeval(e2__4,node__4,prog__4,v2),
   un___VBool(v1,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [86:1-86:253])_");
if(!(rel_71_VBool->empty()) && !(rel_122_un_VBool->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env6[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__4,node__4,prog__4),
   hasType__Add(exp__4),
   path__Add__0(exp__4,e1__4),
   path__Add__1(exp__4,e2__4),
   aeval(e1__4,node__4,prog__4,v1),
   aeval(e2__4,node__4,prog__4,v2),
   un___VBool(v1,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [86:1-86:253])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_71_VBool->empty()) && !(rel_38_delta_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !rel_38_delta_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_71_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt))) && !(rel_17_delta_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(add(v1,v2,out__0) :- 
   input__aeval(exp__4,node__4,prog__4),
   hasType__Add(exp__4),
   path__Add__0(exp__4,e1__4),
   path__Add__1(exp__4,e2__4),
   aeval(e1__4,node__4,prog__4,v1),
   aeval(e2__4,node__4,prog__4,v2),
   un___VBool(v1,_),
   VBool(0,out__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [86:1-86:253])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_17_delta_VBool->empty()) && !(rel_122_un_VBool->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_17_delta_VBool_op_ctxt,rel_17_delta_VBool->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_17_delta_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(RamUnsigned(0)), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_17_delta_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_73_add->contains(Tuple<RamDomain,3>{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_73_add_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env3[3]),ramBitCast(env4[3]),ramBitCast(env5[1])}};
rel_50_new_add->insert(tuple,READ_OP_CONTEXT(rel_50_new_add_op_ctxt));
}
}
}
}
}
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(input__final(stm__0) :- 
   input__final(stm),
   hasType__Sequence(stm),
   path__Sequence__1(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [94:1-94:99])_");
if(!(rel_116_path_Sequence_1->empty()) && !(rel_33_delta_input_final->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_100_input_final_op_ctxt,rel_100_input_final->createContext());
CREATE_OP_CONTEXT(rel_33_delta_input_final_op_ctxt,rel_33_delta_input_final->createContext());
CREATE_OP_CONTEXT(rel_64_new_input_final_op_ctxt,rel_64_new_input_final->createContext());
for(const auto& env0 : *rel_33_delta_input_final) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_100_input_final->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_100_input_final_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_64_new_input_final->insert(tuple,READ_OP_CONTEXT(rel_64_new_input_final_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__final(stm__0) :- 
   input__final(stm),
   hasType__If(stm),
   path__If__1(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [95:1-95:87])_");
if(!(rel_112_path_If_1->empty()) && !(rel_33_delta_input_final->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_112_path_If_1_op_ctxt,rel_112_path_If_1->createContext());
CREATE_OP_CONTEXT(rel_100_input_final_op_ctxt,rel_100_input_final->createContext());
CREATE_OP_CONTEXT(rel_33_delta_input_final_op_ctxt,rel_33_delta_input_final->createContext());
CREATE_OP_CONTEXT(rel_64_new_input_final_op_ctxt,rel_64_new_input_final->createContext());
for(const auto& env0 : *rel_33_delta_input_final) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_112_path_If_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_112_path_If_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_100_input_final->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_100_input_final_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_64_new_input_final->insert(tuple,READ_OP_CONTEXT(rel_64_new_input_final_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__final(stm__0) :- 
   input__final(stm),
   hasType__If(stm),
   path__If__2(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [96:1-96:87])_");
if(!(rel_113_path_If_2->empty()) && !(rel_33_delta_input_final->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_113_path_If_2_op_ctxt,rel_113_path_If_2->createContext());
CREATE_OP_CONTEXT(rel_100_input_final_op_ctxt,rel_100_input_final->createContext());
CREATE_OP_CONTEXT(rel_33_delta_input_final_op_ctxt,rel_33_delta_input_final->createContext());
CREATE_OP_CONTEXT(rel_64_new_input_final_op_ctxt,rel_64_new_input_final->createContext());
for(const auto& env0 : *rel_33_delta_input_final) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_113_path_If_2->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_113_path_If_2_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_100_input_final->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_100_input_final_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_64_new_input_final->insert(tuple,READ_OP_CONTEXT(rel_64_new_input_final_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__final(stm__0) :- 
   input__flow(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [97:1-97:98])_");
if(!(rel_115_path_Sequence_0->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
CREATE_OP_CONTEXT(rel_100_input_final_op_ctxt,rel_100_input_final->createContext());
CREATE_OP_CONTEXT(rel_64_new_input_final_op_ctxt,rel_64_new_input_final->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_100_input_final->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_100_input_final_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_64_new_input_final->insert(tuple,READ_OP_CONTEXT(rel_64_new_input_final_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__final(stm__0) :- 
   input__flow(stm),
   hasType__While(stm),
   path__While__1(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [98:1-98:92])_");
if(!(rel_121_path_While_1->empty()) && !(rel_34_delta_input_flow->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_121_path_While_1_op_ctxt,rel_121_path_While_1->createContext());
CREATE_OP_CONTEXT(rel_34_delta_input_flow_op_ctxt,rel_34_delta_input_flow->createContext());
CREATE_OP_CONTEXT(rel_100_input_final_op_ctxt,rel_100_input_final->createContext());
CREATE_OP_CONTEXT(rel_64_new_input_final_op_ctxt,rel_64_new_input_final->createContext());
for(const auto& env0 : *rel_34_delta_input_flow) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_121_path_While_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_121_path_While_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_100_input_final->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_100_input_final_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_64_new_input_final->insert(tuple,READ_OP_CONTEXT(rel_64_new_input_final_op_ctxt));
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(input__freevarsStm(stm__0) :- 
   input__freevarsStm(stm),
   hasType__Sequence(stm),
   path__Sequence__0(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [116:1-116:111])_");
if(!(rel_115_path_Sequence_0->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt,rel_115_path_Sequence_0->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_67_new_input_freevarsStm_op_ctxt,rel_67_new_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_115_path_Sequence_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_115_path_Sequence_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_103_input_freevarsStm->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_67_new_input_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_67_new_input_freevarsStm_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__freevarsStm(stm__0) :- 
   input__freevarsStm(stm),
   hasType__Sequence(stm),
   path__Sequence__1(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [117:1-117:111])_");
if(!(rel_116_path_Sequence_1->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt,rel_116_path_Sequence_1->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_67_new_input_freevarsStm_op_ctxt,rel_67_new_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt))) {
auto range = rel_116_path_Sequence_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_116_path_Sequence_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_103_input_freevarsStm->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_67_new_input_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_67_new_input_freevarsStm_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__freevarsStm(stm__0) :- 
   input__freevarsStm(stm),
   hasType__If(stm),
   path__If__1(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [118:1-118:99])_");
if(!(rel_112_path_If_1->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_112_path_If_1_op_ctxt,rel_112_path_If_1->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_67_new_input_freevarsStm_op_ctxt,rel_67_new_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_112_path_If_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_112_path_If_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_103_input_freevarsStm->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_67_new_input_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_67_new_input_freevarsStm_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__freevarsStm(stm__0) :- 
   input__freevarsStm(stm),
   hasType__If(stm),
   path__If__2(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [119:1-119:99])_");
if(!(rel_113_path_If_2->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_113_path_If_2_op_ctxt,rel_113_path_If_2->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_67_new_input_freevarsStm_op_ctxt,rel_67_new_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt))) {
auto range = rel_113_path_If_2->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_113_path_If_2_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_103_input_freevarsStm->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_67_new_input_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_67_new_input_freevarsStm_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__freevarsStm(stm__0) :- 
   input__freevarsStm(stm),
   hasType__While(stm),
   path__While__1(stm,stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [120:1-120:105])_");
if(!(rel_121_path_While_1->empty()) && !(rel_36_delta_input_freevarsStm->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_121_path_While_1_op_ctxt,rel_121_path_While_1->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_36_delta_input_freevarsStm_op_ctxt,rel_36_delta_input_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_67_new_input_freevarsStm_op_ctxt,rel_67_new_input_freevarsStm->createContext());
for(const auto& env0 : *rel_36_delta_input_freevarsStm) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt))) {
auto range = rel_121_path_While_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_121_path_While_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_103_input_freevarsStm->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_67_new_input_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_67_new_input_freevarsStm_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__freevarsStm(stm__0) :- 
   ext_input__final_var(stm__0),
   final(stm__0,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [121:1-121:78])_");
if(!(rel_76_ext_input_final_var->empty()) && !(rel_22_delta_final->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_76_ext_input_final_var_op_ctxt,rel_76_ext_input_final_var->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_67_new_input_freevarsStm_op_ctxt,rel_67_new_input_freevarsStm->createContext());
for(const auto& env0 : *rel_76_ext_input_final_var) {
if( !rel_22_delta_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt)).empty() && !(rel_103_input_freevarsStm->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_67_new_input_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_67_new_input_freevarsStm_op_ctxt));
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(input__VNum(_0__0) :- 
   input__aeval(exp,_,_),
   hasType__Num(exp),
   path__Num__0(exp,_0__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [130:1-130:98])_");
if(!(rel_114_path_Num_0->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_87_hasType_Num->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_87_hasType_Num_op_ctxt,rel_87_hasType_Num->createContext());
CREATE_OP_CONTEXT(rel_114_path_Num_0_op_ctxt,rel_114_path_Num_0->createContext());
CREATE_OP_CONTEXT(rel_96_input_VNum_op_ctxt,rel_96_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt,rel_60_new_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_87_hasType_Num->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_87_hasType_Num_op_ctxt))) {
auto range = rel_114_path_Num_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_114_path_Num_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_96_input_VNum->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_96_input_VNum_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_60_new_input_VNum->insert(tuple,READ_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__VNum((n1+n2)) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__Add(exp__2),
   path__Add__0(exp__2,e1__2),
   path__Add__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1__3),
   aeval(e2__2,node__2,prog__2,v2__3),
   un___VNum(v1__3,n1),
   un___VNum(v2__3,n2),
   (n1+n2) > -100,
   (n1+n2) < 100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [133:1-133:312])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_123_un_VNum->empty()) && !(rel_74_aeval->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_96_input_VNum_op_ctxt,rel_96_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt,rel_60_new_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) < ramBitCast<RamSigned>(RamSigned(100))) && !(rel_96_input_VNum->contains(Tuple<RamDomain,1>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1])))}},READ_OP_CONTEXT(rel_96_input_VNum_op_ctxt))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1])))}};
rel_60_new_input_VNum->insert(tuple,READ_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__VNum((n1+n2)) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__Add(exp__2),
   path__Add__0(exp__2,e1__2),
   path__Add__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1__3),
   aeval(e2__2,node__2,prog__2,v2__3),
   un___VNum(v1__3,n1),
   un___VNum(v2__3,n2),
   (n1+n2) > -100,
   (n1+n2) < 100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [133:1-133:312])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_74_aeval->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_96_input_VNum_op_ctxt,rel_96_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt,rel_60_new_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) < ramBitCast<RamSigned>(RamSigned(100))) && !(rel_96_input_VNum->contains(Tuple<RamDomain,1>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1])))}},READ_OP_CONTEXT(rel_96_input_VNum_op_ctxt))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1])))}};
rel_60_new_input_VNum->insert(tuple,READ_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__VNum((n1+n2)) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__Add(exp__2),
   path__Add__0(exp__2,e1__2),
   path__Add__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1__3),
   aeval(e2__2,node__2,prog__2,v2__3),
   un___VNum(v1__3,n1),
   un___VNum(v2__3,n2),
   (n1+n2) > -100,
   (n1+n2) < 100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [133:1-133:312])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_20_delta_aeval->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_96_input_VNum_op_ctxt,rel_96_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt,rel_60_new_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) < ramBitCast<RamSigned>(RamSigned(100))) && !(rel_96_input_VNum->contains(Tuple<RamDomain,1>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1])))}},READ_OP_CONTEXT(rel_96_input_VNum_op_ctxt))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1])))}};
rel_60_new_input_VNum->insert(tuple,READ_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt));
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__VNum((n1+n2)) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__Add(exp__2),
   path__Add__0(exp__2,e1__2),
   path__Add__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1__3),
   aeval(e2__2,node__2,prog__2,v2__3),
   un___VNum(v1__3,n1),
   un___VNum(v2__3,n2),
   (n1+n2) > -100,
   (n1+n2) < 100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [133:1-133:312])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_39_delta_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_96_input_VNum_op_ctxt,rel_96_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt,rel_60_new_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) < ramBitCast<RamSigned>(RamSigned(100))) && !(rel_96_input_VNum->contains(Tuple<RamDomain,1>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1])))}},READ_OP_CONTEXT(rel_96_input_VNum_op_ctxt))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1])))}};
rel_60_new_input_VNum->insert(tuple,READ_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt));
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__VNum((n1+n2)) :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__Add(exp__2),
   path__Add__0(exp__2,e1__2),
   path__Add__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1__3),
   aeval(e2__2,node__2,prog__2,v2__3),
   un___VNum(v1__3,n1),
   un___VNum(v2__3,n2),
   (n1+n2) > -100,
   (n1+n2) < 100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [133:1-133:312])_");
if(!(rel_83_hasType_Add->empty()) && !(rel_97_input_aeval->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_123_un_VNum->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_96_input_VNum_op_ctxt,rel_96_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt,rel_60_new_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && !(rel_96_input_VNum->contains(Tuple<RamDomain,1>{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1])))}},READ_OP_CONTEXT(rel_96_input_VNum_op_ctxt))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) < ramBitCast<RamSigned>(RamSigned(100)))) {
Tuple<RamDomain,1> tuple{{ramBitCast((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1])))}};
rel_60_new_input_VNum->insert(tuple,READ_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt));
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__VNum(-1000) :- 
   +disconnected6().
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [131:1-131:296])_");
if(!(rel_15_delta_disconnected6->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_96_input_VNum_op_ctxt,rel_96_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt,rel_60_new_input_VNum->createContext());
if(!(rel_96_input_VNum->contains(Tuple<RamDomain,1>{{ramBitCast(RamSigned(-1000))}},READ_OP_CONTEXT(rel_96_input_VNum_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(RamSigned(-1000))}};
rel_60_new_input_VNum->insert(tuple,READ_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt));
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__VNum(1000) :- 
   +disconnected7().
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [132:1-132:310])_");
if(!(rel_16_delta_disconnected7->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_96_input_VNum_op_ctxt,rel_96_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt,rel_60_new_input_VNum->createContext());
if(!(rel_96_input_VNum->contains(Tuple<RamDomain,1>{{ramBitCast(RamSigned(1000))}},READ_OP_CONTEXT(rel_96_input_VNum_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(RamSigned(1000))}};
rel_60_new_input_VNum->insert(tuple,READ_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt));
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(input__aeval(exp__0,node__0,prog__0) :- 
   input__aeval(exp,node__0,prog__0),
   hasType__GreaterThan(exp),
   path__GreaterThan__0(exp,exp__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [146:1-146:141])_");
if(!(rel_109_path_GreaterThan_0->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt,rel_61_new_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_97_input_aeval->contains(Tuple<RamDomain,3>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_97_input_aeval_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_61_new_input_aeval->insert(tuple,READ_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__aeval(exp__0,node__0,prog__0) :- 
   input__aeval(exp,node__0,prog__0),
   hasType__GreaterThan(exp),
   path__GreaterThan__0(exp,e1),
   path__GreaterThan__1(exp,exp__0),
   aeval(e1,node__0,prog__0,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [147:1-147:205])_");
if(!(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt,rel_61_new_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_97_input_aeval->contains(Tuple<RamDomain,3>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_97_input_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_61_new_input_aeval->insert(tuple,READ_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt));
break;
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__aeval(exp__0,node__0,prog__0) :- 
   input__aeval(exp,node__0,prog__0),
   hasType__GreaterThan(exp),
   path__GreaterThan__0(exp,e1),
   path__GreaterThan__1(exp,exp__0),
   aeval(e1,node__0,prog__0,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [147:1-147:205])_");
if(!(rel_20_delta_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_85_hasType_GreaterThan->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt,rel_61_new_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)).empty()) {
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_97_input_aeval->contains(Tuple<RamDomain,3>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_97_input_aeval_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_61_new_input_aeval->insert(tuple,READ_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt));
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__aeval(exp__0,node__0,prog__0) :- 
   input__aeval(exp,node__0,prog__0),
   hasType__Add(exp),
   path__Add__0(exp,exp__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [148:1-148:125])_");
if(!(rel_105_path_Add_0->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt,rel_61_new_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_97_input_aeval->contains(Tuple<RamDomain,3>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_97_input_aeval_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_61_new_input_aeval->insert(tuple,READ_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__aeval(exp__0,node__0,prog__0) :- 
   input__aeval(exp,node__0,prog__0),
   hasType__Add(exp),
   path__Add__0(exp,e1),
   path__Add__1(exp,exp__0),
   aeval(e1,node__0,prog__0,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [149:1-149:181])_");
if(!(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt,rel_61_new_input_aeval->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_97_input_aeval->contains(Tuple<RamDomain,3>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_97_input_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_61_new_input_aeval->insert(tuple,READ_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt));
break;
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__aeval(exp__0,node__0,prog__0) :- 
   input__aeval(exp,node__0,prog__0),
   hasType__Add(exp),
   path__Add__0(exp,e1),
   path__Add__1(exp,exp__0),
   aeval(e1,node__0,prog__0,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [149:1-149:181])_");
if(!(rel_20_delta_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_97_input_aeval->empty()) && !(rel_83_hasType_Add->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt,rel_61_new_input_aeval->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)).empty()) {
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_97_input_aeval->contains(Tuple<RamDomain,3>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_97_input_aeval_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_61_new_input_aeval->insert(tuple,READ_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt));
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__aeval(exp__0,node__0,prog__0) :- 
   input__exit_var(node__0,prog__0,y),
   hasType__Assign(node__0),
   path__Assign__0(node__0,y),
   path__Assign__1(node__0,exp__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [145:1-145:176])_");
if(!(rel_108_path_Assign_1->empty()) && !(rel_107_path_Assign_0->empty()) && !(rel_32_delta_input_exit_var->empty()) && !(rel_84_hasType_Assign->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt,rel_84_hasType_Assign->createContext());
CREATE_OP_CONTEXT(rel_108_path_Assign_1_op_ctxt,rel_108_path_Assign_1->createContext());
CREATE_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt,rel_107_path_Assign_0->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt,rel_61_new_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt,rel_32_delta_input_exit_var->createContext());
for(const auto& env0 : *rel_32_delta_input_exit_var) {
if( rel_107_path_Assign_0->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt)) && rel_84_hasType_Assign->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt))) {
auto range = rel_108_path_Assign_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_108_path_Assign_1_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_97_input_aeval->contains(Tuple<RamDomain,3>{{ramBitCast(env1[1]),ramBitCast(env0[0]),ramBitCast(env0[1])}},READ_OP_CONTEXT(rel_97_input_aeval_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env1[1]),ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_61_new_input_aeval->insert(tuple,READ_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt));
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(input__entry_var(stm__0,prog__0,x__0) :- 
   input__exit_var(stm__0,prog__0,x__0),
   hasType__Assign(stm__0),
   path__Assign__0(stm__0,y),
   x__0 != y.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [135:1-135:147])_");
if(!(rel_107_path_Assign_0->empty()) && !(rel_32_delta_input_exit_var->empty()) && !(rel_84_hasType_Assign->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt,rel_84_hasType_Assign->createContext());
CREATE_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt,rel_107_path_Assign_0->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_62_new_input_entry_var_op_ctxt,rel_62_new_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt,rel_32_delta_input_exit_var->createContext());
for(const auto& env0 : *rel_32_delta_input_exit_var) {
if( rel_84_hasType_Assign->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_84_hasType_Assign_op_ctxt)) && !(rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)))) {
auto range = rel_107_path_Assign_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_107_path_Assign_0_op_ctxt));
for(const auto& env1 : range) {
if( (ramBitCast<RamDomain>(env0[2]) != ramBitCast<RamDomain>(env1[1]))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_62_new_input_entry_var->insert(tuple,READ_OP_CONTEXT(rel_62_new_input_entry_var_op_ctxt));
break;
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__entry_var(stm__0,prog__0,x__0) :- 
   input__exit_var(stm__0,prog__0,x__0),
   hasType__Skip(stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [136:1-136:106])_");
if(!(rel_32_delta_input_exit_var->empty()) && !(rel_89_hasType_Skip->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt,rel_89_hasType_Skip->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_62_new_input_entry_var_op_ctxt,rel_62_new_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt,rel_32_delta_input_exit_var->createContext());
for(const auto& env0 : *rel_32_delta_input_exit_var) {
if( rel_89_hasType_Skip->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_89_hasType_Skip_op_ctxt)) && !(rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_62_new_input_entry_var->insert(tuple,READ_OP_CONTEXT(rel_62_new_input_entry_var_op_ctxt));
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__entry_var(stm__0,prog__0,x__0) :- 
   input__exit_var(stm__0,prog__0,x__0),
   hasType__Sequence(stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [137:1-137:110])_");
if(!(rel_32_delta_input_exit_var->empty()) && !(rel_88_hasType_Sequence->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt,rel_88_hasType_Sequence->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_62_new_input_entry_var_op_ctxt,rel_62_new_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt,rel_32_delta_input_exit_var->createContext());
for(const auto& env0 : *rel_32_delta_input_exit_var) {
if( rel_88_hasType_Sequence->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_88_hasType_Sequence_op_ctxt)) && !(rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_62_new_input_entry_var->insert(tuple,READ_OP_CONTEXT(rel_62_new_input_entry_var_op_ctxt));
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__entry_var(stm__0,prog__0,x__0) :- 
   input__exit_var(stm__0,prog__0,x__0),
   hasType__If(stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [138:1-138:104])_");
if(!(rel_32_delta_input_exit_var->empty()) && !(rel_86_hasType_If->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_86_hasType_If_op_ctxt,rel_86_hasType_If->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_62_new_input_entry_var_op_ctxt,rel_62_new_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt,rel_32_delta_input_exit_var->createContext());
for(const auto& env0 : *rel_32_delta_input_exit_var) {
if( rel_86_hasType_If->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_86_hasType_If_op_ctxt)) && !(rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_62_new_input_entry_var->insert(tuple,READ_OP_CONTEXT(rel_62_new_input_entry_var_op_ctxt));
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__entry_var(stm__0,prog__0,x__0) :- 
   input__exit_var(stm__0,prog__0,x__0),
   hasType__While(stm__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [139:1-139:107])_");
if(!(rel_32_delta_input_exit_var->empty()) && !(rel_93_hasType_While->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_93_hasType_While_op_ctxt,rel_93_hasType_While->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_62_new_input_entry_var_op_ctxt,rel_62_new_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_32_delta_input_exit_var_op_ctxt,rel_32_delta_input_exit_var->createContext());
for(const auto& env0 : *rel_32_delta_input_exit_var) {
if( rel_93_hasType_While->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_93_hasType_While_op_ctxt)) && !(rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_62_new_input_entry_var->insert(tuple,READ_OP_CONTEXT(rel_62_new_input_entry_var_op_ctxt));
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__entry_var(stm__0,prog__0,x__0) :- 
   input__aeval(exp,stm__0,prog__0),
   hasType__Var(exp),
   path__Var__0(exp,x__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [140:1-140:123])_");
if(!(rel_119_path_Var_0->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_92_hasType_Var->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_92_hasType_Var_op_ctxt,rel_92_hasType_Var->createContext());
CREATE_OP_CONTEXT(rel_119_path_Var_0_op_ctxt,rel_119_path_Var_0->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_62_new_input_entry_var_op_ctxt,rel_62_new_input_entry_var->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( rel_92_hasType_Var->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_92_hasType_Var_op_ctxt))) {
auto range = rel_119_path_Var_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_119_path_Var_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_98_input_entry_var->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env1[1])}};
rel_62_new_input_entry_var->insert(tuple,READ_OP_CONTEXT(rel_62_new_input_entry_var_op_ctxt));
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(input__exit_var(stm__0,prog__0,x__0) :- 
   input__entry_var(stm,prog__0,x__0),
   flow(prog__0,stm__0,stm).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [142:1-142:108])_");
if(!(rel_31_delta_input_entry_var->empty()) && !(rel_79_flow->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_31_delta_input_entry_var_op_ctxt,rel_31_delta_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
CREATE_OP_CONTEXT(rel_63_new_input_exit_var_op_ctxt,rel_63_new_input_exit_var->createContext());
for(const auto& env0 : *rel_31_delta_input_entry_var) {
auto range = rel_79_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_79_flow_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_23_delta_flow->contains(Tuple<RamDomain,3>{{ramBitCast(env0[1]),ramBitCast(env1[1]),ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt))) && !(rel_99_input_exit_var->contains(Tuple<RamDomain,3>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_99_input_exit_var_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_63_new_input_exit_var->insert(tuple,READ_OP_CONTEXT(rel_63_new_input_exit_var_op_ctxt));
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__exit_var(stm__0,prog__0,x__0) :- 
   input__entry_var(stm,prog__0,x__0),
   flow(prog__0,stm__0,stm).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [142:1-142:108])_");
if(!(rel_98_input_entry_var->empty()) && !(rel_23_delta_flow->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_23_delta_flow_op_ctxt,rel_23_delta_flow->createContext());
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
CREATE_OP_CONTEXT(rel_63_new_input_exit_var_op_ctxt,rel_63_new_input_exit_var->createContext());
for(const auto& env0 : *rel_98_input_entry_var) {
auto range = rel_23_delta_flow->lowerUpperRange_101(Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,3>{{ramBitCast(env0[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_23_delta_flow_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_99_input_exit_var->contains(Tuple<RamDomain,3>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}},READ_OP_CONTEXT(rel_99_input_exit_var_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_63_new_input_exit_var->insert(tuple,READ_OP_CONTEXT(rel_63_new_input_exit_var_op_ctxt));
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__exit_var(stm__0,prog__0,x__0) :- 
   ext_input__final_var(prog__0),
   final(prog__0,stm__0),
   freevarsStm(prog__0,x__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [143:1-143:125])_");
if(!(rel_81_freevarsStm->empty()) && !(rel_76_ext_input_final_var->empty()) && !(rel_22_delta_final->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_76_ext_input_final_var_op_ctxt,rel_76_ext_input_final_var->createContext());
CREATE_OP_CONTEXT(rel_22_delta_final_op_ctxt,rel_22_delta_final->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt,rel_25_delta_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
CREATE_OP_CONTEXT(rel_63_new_input_exit_var_op_ctxt,rel_63_new_input_exit_var->createContext());
for(const auto& env0 : *rel_76_ext_input_final_var) {
auto range = rel_22_delta_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_22_delta_final_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_81_freevarsStm->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_25_delta_freevarsStm->contains(Tuple<RamDomain,2>{{ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt))) && !(rel_99_input_exit_var->contains(Tuple<RamDomain,3>{{ramBitCast(env1[1]),ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_99_input_exit_var_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env1[1]),ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_63_new_input_exit_var->insert(tuple,READ_OP_CONTEXT(rel_63_new_input_exit_var_op_ctxt));
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(input__exit_var(stm__0,prog__0,x__0) :- 
   ext_input__final_var(prog__0),
   final(prog__0,stm__0),
   freevarsStm(prog__0,x__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [143:1-143:125])_");
if(!(rel_25_delta_freevarsStm->empty()) && !(rel_76_ext_input_final_var->empty()) && !(rel_77_final->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_76_ext_input_final_var_op_ctxt,rel_76_ext_input_final_var->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt,rel_25_delta_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
CREATE_OP_CONTEXT(rel_63_new_input_exit_var_op_ctxt,rel_63_new_input_exit_var->createContext());
for(const auto& env0 : *rel_76_ext_input_final_var) {
auto range = rel_77_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_77_final_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_25_delta_freevarsStm->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_25_delta_freevarsStm_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_99_input_exit_var->contains(Tuple<RamDomain,3>{{ramBitCast(env1[1]),ramBitCast(env0[0]),ramBitCast(env2[1])}},READ_OP_CONTEXT(rel_99_input_exit_var_op_ctxt)))) {
Tuple<RamDomain,3> tuple{{ramBitCast(env1[1]),ramBitCast(env0[0]),ramBitCast(env2[1])}};
rel_63_new_input_exit_var->insert(tuple,READ_OP_CONTEXT(rel_63_new_input_exit_var_op_ctxt));
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(+disconnected5() :- 
   input__aeval(exp__4,node__4,prog__4),
   hasType__Add(exp__4),
   path__Add__0(exp__4,e1__4),
   path__Add__1(exp__4,e2__4),
   aeval(e1__4,node__4,prog__4,v1__5),
   aeval(e2__4,node__4,prog__4,_),
   un___VBool(v1__5,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [128:1-128:255])_");
if(!(rel_122_un_VBool->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_45_new_disconnected5->empty() && rel_6_disconnected5->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_45_new_disconnected5_op_ctxt,rel_45_new_disconnected5->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( !(rel_45_new_disconnected5->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_45_new_disconnected5->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_45_new_disconnected5->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_45_new_disconnected5->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
if( !(rel_45_new_disconnected5->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_45_new_disconnected5->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_45_new_disconnected5->insert(tuple,READ_OP_CONTEXT(rel_45_new_disconnected5_op_ctxt));
break;
}
}
break;
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected5() :- 
   input__aeval(exp__4,node__4,prog__4),
   hasType__Add(exp__4),
   path__Add__0(exp__4,e1__4),
   path__Add__1(exp__4,e2__4),
   aeval(e1__4,node__4,prog__4,v1__5),
   aeval(e2__4,node__4,prog__4,_),
   un___VBool(v1__5,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [128:1-128:255])_");
if(!(rel_97_input_aeval->empty()) && !(rel_122_un_VBool->empty()) && !(rel_83_hasType_Add->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && rel_45_new_disconnected5->empty() && rel_6_disconnected5->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_45_new_disconnected5_op_ctxt,rel_45_new_disconnected5->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_45_new_disconnected5->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_45_new_disconnected5->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_45_new_disconnected5->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_45_new_disconnected5->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
if( !(rel_45_new_disconnected5->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_45_new_disconnected5->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_45_new_disconnected5->insert(tuple,READ_OP_CONTEXT(rel_45_new_disconnected5_op_ctxt));
break;
}
}
break;
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected5() :- 
   input__aeval(exp__4,node__4,prog__4),
   hasType__Add(exp__4),
   path__Add__0(exp__4,e1__4),
   path__Add__1(exp__4,e2__4),
   aeval(e1__4,node__4,prog__4,v1__5),
   aeval(e2__4,node__4,prog__4,_),
   un___VBool(v1__5,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [128:1-128:255])_");
if(!(rel_97_input_aeval->empty()) && !(rel_122_un_VBool->empty()) && !(rel_83_hasType_Add->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && rel_45_new_disconnected5->empty() && rel_6_disconnected5->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_45_new_disconnected5_op_ctxt,rel_45_new_disconnected5->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_45_new_disconnected5->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_45_new_disconnected5->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_45_new_disconnected5->empty())) break;
if( !rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_45_new_disconnected5->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env4[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_45_new_disconnected5->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_45_new_disconnected5->insert(tuple,READ_OP_CONTEXT(rel_45_new_disconnected5_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected5() :- 
   input__aeval(exp__4,node__4,prog__4),
   hasType__Add(exp__4),
   path__Add__0(exp__4,e1__4),
   path__Add__1(exp__4,e2__4),
   aeval(e1__4,node__4,prog__4,v1__5),
   aeval(e2__4,node__4,prog__4,_),
   un___VBool(v1__5,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [128:1-128:255])_");
if(!(rel_38_delta_un_VBool->empty()) && !(rel_97_input_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_45_new_disconnected5->empty() && rel_6_disconnected5->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_45_new_disconnected5_op_ctxt,rel_45_new_disconnected5->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_45_new_disconnected5->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_45_new_disconnected5->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_45_new_disconnected5->empty())) break;
if( !rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_45_new_disconnected5->empty())) break;
if( !rel_38_delta_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)).empty()) {
Tuple<RamDomain,0> tuple{{}};
rel_45_new_disconnected5->insert(tuple,READ_OP_CONTEXT(rel_45_new_disconnected5_op_ctxt));
}
}
}
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(+disconnected0() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   n1 > n2.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [123:1-123:309])_");
if(!(rel_123_un_VNum->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_40_new_disconnected0->empty() && rel_1_disconnected0->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_40_new_disconnected0_op_ctxt,rel_40_new_disconnected0->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( !(rel_40_new_disconnected0->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamDomain>(env5[1]) != ramBitCast<RamDomain>(env6[1])) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_40_new_disconnected0->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_40_new_disconnected0->insert(tuple,READ_OP_CONTEXT(rel_40_new_disconnected0_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected0() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   n1 > n2.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [123:1-123:309])_");
if(!(rel_123_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_40_new_disconnected0->empty() && rel_1_disconnected0->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_40_new_disconnected0_op_ctxt,rel_40_new_disconnected0->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_40_new_disconnected0->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamDomain>(env5[1]) != ramBitCast<RamDomain>(env6[1])) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_40_new_disconnected0->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_40_new_disconnected0->insert(tuple,READ_OP_CONTEXT(rel_40_new_disconnected0_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected0() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   n1 > n2.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [123:1-123:309])_");
if(!(rel_123_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_40_new_disconnected0->empty() && rel_1_disconnected0->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_40_new_disconnected0_op_ctxt,rel_40_new_disconnected0->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_40_new_disconnected0->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamDomain>(env5[1]) != ramBitCast<RamDomain>(env6[1])) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_40_new_disconnected0->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_40_new_disconnected0->insert(tuple,READ_OP_CONTEXT(rel_40_new_disconnected0_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected0() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   n1 > n2.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [123:1-123:309])_");
if(!(rel_123_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_40_new_disconnected0->empty() && rel_1_disconnected0->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_40_new_disconnected0_op_ctxt,rel_40_new_disconnected0->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_40_new_disconnected0->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamDomain>(env5[1]) != ramBitCast<RamDomain>(env6[1])) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_40_new_disconnected0->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_40_new_disconnected0->insert(tuple,READ_OP_CONTEXT(rel_40_new_disconnected0_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected0() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   n1 > n2.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [123:1-123:309])_");
if(!(rel_39_delta_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_40_new_disconnected0->empty() && rel_1_disconnected0->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_40_new_disconnected0_op_ctxt,rel_40_new_disconnected0->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_40_new_disconnected0->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_40_new_disconnected0->empty())) break;
auto range = rel_39_delta_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamDomain>(env5[1]) != ramBitCast<RamDomain>(env6[1]))) {
if( !(rel_40_new_disconnected0->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_40_new_disconnected0->insert(tuple,READ_OP_CONTEXT(rel_40_new_disconnected0_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(+disconnected1() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   n1 <= n2.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [124:1-124:310])_");
if(!(rel_123_un_VNum->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_41_new_disconnected1->empty() && rel_2_disconnected1->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_41_new_disconnected1_op_ctxt,rel_41_new_disconnected1->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( !(rel_41_new_disconnected1->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_41_new_disconnected1->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_41_new_disconnected1->insert(tuple,READ_OP_CONTEXT(rel_41_new_disconnected1_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected1() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   n1 <= n2.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [124:1-124:310])_");
if(!(rel_123_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_41_new_disconnected1->empty() && rel_2_disconnected1->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_41_new_disconnected1_op_ctxt,rel_41_new_disconnected1->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_41_new_disconnected1->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_41_new_disconnected1->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_41_new_disconnected1->insert(tuple,READ_OP_CONTEXT(rel_41_new_disconnected1_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected1() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   n1 <= n2.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [124:1-124:310])_");
if(!(rel_123_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_41_new_disconnected1->empty() && rel_2_disconnected1->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_41_new_disconnected1_op_ctxt,rel_41_new_disconnected1->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_41_new_disconnected1->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_41_new_disconnected1->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_41_new_disconnected1->insert(tuple,READ_OP_CONTEXT(rel_41_new_disconnected1_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected1() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   n1 <= n2.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [124:1-124:310])_");
if(!(rel_123_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_41_new_disconnected1->empty() && rel_2_disconnected1->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_41_new_disconnected1_op_ctxt,rel_41_new_disconnected1->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_41_new_disconnected1->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_41_new_disconnected1->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_41_new_disconnected1->insert(tuple,READ_OP_CONTEXT(rel_41_new_disconnected1_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected1() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__GreaterThan(exp__0),
   path__GreaterThan__0(exp__0,e1__0),
   path__GreaterThan__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   n1 <= n2.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [124:1-124:310])_");
if(!(rel_39_delta_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_41_new_disconnected1->empty() && rel_2_disconnected1->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_41_new_disconnected1_op_ctxt,rel_41_new_disconnected1->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_41_new_disconnected1->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
auto range = rel_39_delta_un_VNum->lowerUpperRange_12(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast(env5[1])}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_41_new_disconnected1->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_41_new_disconnected1->insert(tuple,READ_OP_CONTEXT(rel_41_new_disconnected1_op_ctxt));
}
}
}
}
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(+disconnected2() :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__GreaterThan(exp__1),
   path__GreaterThan__0(exp__1,e1__1),
   path__GreaterThan__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1__2),
   aeval(e2__1,node__1,prog__1,v2__2),
   un___VNum(v1__2,_),
   un___VBool(v2__2,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [125:1-125:301])_");
if(!(rel_122_un_VBool->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_42_new_disconnected2->empty() && rel_3_disconnected2->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_42_new_disconnected2_op_ctxt,rel_42_new_disconnected2->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( !(rel_42_new_disconnected2->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_42_new_disconnected2->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_42_new_disconnected2->insert(tuple,READ_OP_CONTEXT(rel_42_new_disconnected2_op_ctxt));
break;
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected2() :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__GreaterThan(exp__1),
   path__GreaterThan__0(exp__1,e1__1),
   path__GreaterThan__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1__2),
   aeval(e2__1,node__1,prog__1,v2__2),
   un___VNum(v1__2,_),
   un___VBool(v2__2,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [125:1-125:301])_");
if(!(rel_97_input_aeval->empty()) && !(rel_122_un_VBool->empty()) && !(rel_85_hasType_GreaterThan->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && rel_42_new_disconnected2->empty() && rel_3_disconnected2->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_42_new_disconnected2_op_ctxt,rel_42_new_disconnected2->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_42_new_disconnected2->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_42_new_disconnected2->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_42_new_disconnected2->insert(tuple,READ_OP_CONTEXT(rel_42_new_disconnected2_op_ctxt));
break;
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected2() :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__GreaterThan(exp__1),
   path__GreaterThan__0(exp__1,e1__1),
   path__GreaterThan__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1__2),
   aeval(e2__1,node__1,prog__1,v2__2),
   un___VNum(v1__2,_),
   un___VBool(v2__2,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [125:1-125:301])_");
if(!(rel_97_input_aeval->empty()) && !(rel_122_un_VBool->empty()) && !(rel_85_hasType_GreaterThan->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_123_un_VNum->empty()) && rel_42_new_disconnected2->empty() && rel_3_disconnected2->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_42_new_disconnected2_op_ctxt,rel_42_new_disconnected2->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_42_new_disconnected2->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_42_new_disconnected2->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_42_new_disconnected2->insert(tuple,READ_OP_CONTEXT(rel_42_new_disconnected2_op_ctxt));
break;
}
}
break;
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected2() :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__GreaterThan(exp__1),
   path__GreaterThan__0(exp__1,e1__1),
   path__GreaterThan__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1__2),
   aeval(e2__1,node__1,prog__1,v2__2),
   un___VNum(v1__2,_),
   un___VBool(v2__2,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [125:1-125:301])_");
if(!(rel_122_un_VBool->empty()) && !(rel_97_input_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_42_new_disconnected2->empty() && rel_3_disconnected2->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_42_new_disconnected2_op_ctxt,rel_42_new_disconnected2->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_42_new_disconnected2->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
if( !rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_42_new_disconnected2->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_42_new_disconnected2->insert(tuple,READ_OP_CONTEXT(rel_42_new_disconnected2_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected2() :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__GreaterThan(exp__1),
   path__GreaterThan__0(exp__1,e1__1),
   path__GreaterThan__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1__2),
   aeval(e2__1,node__1,prog__1,v2__2),
   un___VNum(v1__2,_),
   un___VBool(v2__2,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [125:1-125:301])_");
if(!(rel_38_delta_un_VBool->empty()) && !(rel_97_input_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_42_new_disconnected2->empty() && rel_3_disconnected2->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_42_new_disconnected2_op_ctxt,rel_42_new_disconnected2->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_42_new_disconnected2->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
if( !rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_42_new_disconnected2->empty())) break;
if( !rel_38_delta_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)).empty()) {
Tuple<RamDomain,0> tuple{{}};
rel_42_new_disconnected2->insert(tuple,READ_OP_CONTEXT(rel_42_new_disconnected2_op_ctxt));
}
}
}
}
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(+disconnected3() :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__GreaterThan(exp__2),
   path__GreaterThan__0(exp__2,e1__2),
   path__GreaterThan__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1__3),
   aeval(e2__2,node__2,prog__2,_),
   un___VBool(v1__3,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [126:1-126:279])_");
if(!(rel_122_un_VBool->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_43_new_disconnected3->empty() && rel_4_disconnected3->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_43_new_disconnected3_op_ctxt,rel_43_new_disconnected3->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( !(rel_43_new_disconnected3->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_43_new_disconnected3->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_43_new_disconnected3->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_43_new_disconnected3->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
if( !(rel_43_new_disconnected3->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_43_new_disconnected3->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_43_new_disconnected3->insert(tuple,READ_OP_CONTEXT(rel_43_new_disconnected3_op_ctxt));
break;
}
}
break;
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected3() :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__GreaterThan(exp__2),
   path__GreaterThan__0(exp__2,e1__2),
   path__GreaterThan__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1__3),
   aeval(e2__2,node__2,prog__2,_),
   un___VBool(v1__3,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [126:1-126:279])_");
if(!(rel_97_input_aeval->empty()) && !(rel_122_un_VBool->empty()) && !(rel_85_hasType_GreaterThan->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && rel_43_new_disconnected3->empty() && rel_4_disconnected3->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_43_new_disconnected3_op_ctxt,rel_43_new_disconnected3->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_43_new_disconnected3->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_43_new_disconnected3->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_43_new_disconnected3->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_43_new_disconnected3->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
if( !(rel_43_new_disconnected3->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_43_new_disconnected3->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_43_new_disconnected3->insert(tuple,READ_OP_CONTEXT(rel_43_new_disconnected3_op_ctxt));
break;
}
}
break;
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected3() :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__GreaterThan(exp__2),
   path__GreaterThan__0(exp__2,e1__2),
   path__GreaterThan__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1__3),
   aeval(e2__2,node__2,prog__2,_),
   un___VBool(v1__3,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [126:1-126:279])_");
if(!(rel_97_input_aeval->empty()) && !(rel_122_un_VBool->empty()) && !(rel_85_hasType_GreaterThan->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && rel_43_new_disconnected3->empty() && rel_4_disconnected3->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_43_new_disconnected3_op_ctxt,rel_43_new_disconnected3->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_43_new_disconnected3->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_43_new_disconnected3->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_43_new_disconnected3->empty())) break;
if( !rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_43_new_disconnected3->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env4[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_43_new_disconnected3->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_43_new_disconnected3->insert(tuple,READ_OP_CONTEXT(rel_43_new_disconnected3_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected3() :- 
   input__aeval(exp__2,node__2,prog__2),
   hasType__GreaterThan(exp__2),
   path__GreaterThan__0(exp__2,e1__2),
   path__GreaterThan__1(exp__2,e2__2),
   aeval(e1__2,node__2,prog__2,v1__3),
   aeval(e2__2,node__2,prog__2,_),
   un___VBool(v1__3,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [126:1-126:279])_");
if(!(rel_38_delta_un_VBool->empty()) && !(rel_97_input_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_110_path_GreaterThan_1->empty()) && !(rel_109_path_GreaterThan_0->empty()) && !(rel_85_hasType_GreaterThan->empty()) && rel_43_new_disconnected3->empty() && rel_4_disconnected3->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt,rel_85_hasType_GreaterThan->createContext());
CREATE_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt,rel_109_path_GreaterThan_0->createContext());
CREATE_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt,rel_110_path_GreaterThan_1->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_43_new_disconnected3_op_ctxt,rel_43_new_disconnected3->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_43_new_disconnected3->empty())) break;
if( rel_85_hasType_GreaterThan->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_85_hasType_GreaterThan_op_ctxt))) {
auto range = rel_109_path_GreaterThan_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_109_path_GreaterThan_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_43_new_disconnected3->empty())) break;
auto range = rel_110_path_GreaterThan_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_110_path_GreaterThan_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_43_new_disconnected3->empty())) break;
if( !rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_43_new_disconnected3->empty())) break;
if( !rel_38_delta_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)).empty()) {
Tuple<RamDomain,0> tuple{{}};
rel_43_new_disconnected3->insert(tuple,READ_OP_CONTEXT(rel_43_new_disconnected3_op_ctxt));
}
}
}
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(+disconnected4() :- 
   input__aeval(exp__3,node__3,prog__3),
   hasType__Add(exp__3),
   path__Add__0(exp__3,e1__3),
   path__Add__1(exp__3,e2__3),
   aeval(e1__3,node__3,prog__3,v1__4),
   aeval(e2__3,node__3,prog__3,v2__4),
   un___VNum(v1__4,_),
   un___VBool(v2__4,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [127:1-127:277])_");
if(!(rel_122_un_VBool->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_44_new_disconnected4->empty() && rel_5_disconnected4->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_44_new_disconnected4_op_ctxt,rel_44_new_disconnected4->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( !(rel_44_new_disconnected4->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_44_new_disconnected4->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_44_new_disconnected4->insert(tuple,READ_OP_CONTEXT(rel_44_new_disconnected4_op_ctxt));
break;
}
}
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected4() :- 
   input__aeval(exp__3,node__3,prog__3),
   hasType__Add(exp__3),
   path__Add__0(exp__3,e1__3),
   path__Add__1(exp__3,e2__3),
   aeval(e1__3,node__3,prog__3,v1__4),
   aeval(e2__3,node__3,prog__3,v2__4),
   un___VNum(v1__4,_),
   un___VBool(v2__4,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [127:1-127:277])_");
if(!(rel_97_input_aeval->empty()) && !(rel_122_un_VBool->empty()) && !(rel_83_hasType_Add->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_123_un_VNum->empty()) && rel_44_new_disconnected4->empty() && rel_5_disconnected4->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_44_new_disconnected4_op_ctxt,rel_44_new_disconnected4->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_44_new_disconnected4->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_44_new_disconnected4->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_44_new_disconnected4->insert(tuple,READ_OP_CONTEXT(rel_44_new_disconnected4_op_ctxt));
break;
}
}
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected4() :- 
   input__aeval(exp__3,node__3,prog__3),
   hasType__Add(exp__3),
   path__Add__0(exp__3,e1__3),
   path__Add__1(exp__3,e2__3),
   aeval(e1__3,node__3,prog__3,v1__4),
   aeval(e2__3,node__3,prog__3,v2__4),
   un___VNum(v1__4,_),
   un___VBool(v2__4,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [127:1-127:277])_");
if(!(rel_97_input_aeval->empty()) && !(rel_122_un_VBool->empty()) && !(rel_83_hasType_Add->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_123_un_VNum->empty()) && rel_44_new_disconnected4->empty() && rel_5_disconnected4->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_44_new_disconnected4_op_ctxt,rel_44_new_disconnected4->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_44_new_disconnected4->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_44_new_disconnected4->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_44_new_disconnected4->insert(tuple,READ_OP_CONTEXT(rel_44_new_disconnected4_op_ctxt));
break;
}
}
break;
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected4() :- 
   input__aeval(exp__3,node__3,prog__3),
   hasType__Add(exp__3),
   path__Add__0(exp__3,e1__3),
   path__Add__1(exp__3,e2__3),
   aeval(e1__3,node__3,prog__3,v1__4),
   aeval(e2__3,node__3,prog__3,v2__4),
   un___VNum(v1__4,_),
   un___VBool(v2__4,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [127:1-127:277])_");
if(!(rel_122_un_VBool->empty()) && !(rel_97_input_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_44_new_disconnected4->empty() && rel_5_disconnected4->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_44_new_disconnected4_op_ctxt,rel_44_new_disconnected4->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_44_new_disconnected4->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
if( !rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_122_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_38_delta_un_VBool->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)))) {
if( !(rel_44_new_disconnected4->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_44_new_disconnected4->insert(tuple,READ_OP_CONTEXT(rel_44_new_disconnected4_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected4() :- 
   input__aeval(exp__3,node__3,prog__3),
   hasType__Add(exp__3),
   path__Add__0(exp__3,e1__3),
   path__Add__1(exp__3,e2__3),
   aeval(e1__3,node__3,prog__3,v1__4),
   aeval(e2__3,node__3,prog__3,v2__4),
   un___VNum(v1__4,_),
   un___VBool(v2__4,_).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [127:1-127:277])_");
if(!(rel_38_delta_un_VBool->empty()) && !(rel_97_input_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_44_new_disconnected4->empty() && rel_5_disconnected4->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt,rel_38_delta_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_44_new_disconnected4_op_ctxt,rel_44_new_disconnected4->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_44_new_disconnected4->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
if( !rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt)).empty()) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_44_new_disconnected4->empty())) break;
if( !rel_38_delta_un_VBool->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_UNSIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_UNSIGNED)}},READ_OP_CONTEXT(rel_38_delta_un_VBool_op_ctxt)).empty()) {
Tuple<RamDomain,0> tuple{{}};
rel_44_new_disconnected4->insert(tuple,READ_OP_CONTEXT(rel_44_new_disconnected4_op_ctxt));
}
}
}
}
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(+disconnected6() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__Add(exp__0),
   path__Add__0(exp__0,e1__0),
   path__Add__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   (n1+n2) <= -100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [131:1-131:296])_");
if(!(rel_123_un_VNum->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_46_new_disconnected6->empty() && rel_7_disconnected6->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_46_new_disconnected6_op_ctxt,rel_46_new_disconnected6->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( !(rel_46_new_disconnected6->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) <= ramBitCast<RamSigned>(RamSigned(-100))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_46_new_disconnected6->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_46_new_disconnected6->insert(tuple,READ_OP_CONTEXT(rel_46_new_disconnected6_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected6() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__Add(exp__0),
   path__Add__0(exp__0,e1__0),
   path__Add__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   (n1+n2) <= -100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [131:1-131:296])_");
if(!(rel_123_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_46_new_disconnected6->empty() && rel_7_disconnected6->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_46_new_disconnected6_op_ctxt,rel_46_new_disconnected6->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_46_new_disconnected6->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) <= ramBitCast<RamSigned>(RamSigned(-100))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_46_new_disconnected6->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_46_new_disconnected6->insert(tuple,READ_OP_CONTEXT(rel_46_new_disconnected6_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected6() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__Add(exp__0),
   path__Add__0(exp__0,e1__0),
   path__Add__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   (n1+n2) <= -100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [131:1-131:296])_");
if(!(rel_123_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_46_new_disconnected6->empty() && rel_7_disconnected6->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_46_new_disconnected6_op_ctxt,rel_46_new_disconnected6->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_46_new_disconnected6->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) <= ramBitCast<RamSigned>(RamSigned(-100))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_46_new_disconnected6->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_46_new_disconnected6->insert(tuple,READ_OP_CONTEXT(rel_46_new_disconnected6_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected6() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__Add(exp__0),
   path__Add__0(exp__0,e1__0),
   path__Add__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   (n1+n2) <= -100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [131:1-131:296])_");
if(!(rel_123_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_46_new_disconnected6->empty() && rel_7_disconnected6->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_46_new_disconnected6_op_ctxt,rel_46_new_disconnected6->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_46_new_disconnected6->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) <= ramBitCast<RamSigned>(RamSigned(-100))) && !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
if( !(rel_46_new_disconnected6->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_46_new_disconnected6->insert(tuple,READ_OP_CONTEXT(rel_46_new_disconnected6_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected6() :- 
   input__aeval(exp__0,node__0,prog__0),
   hasType__Add(exp__0),
   path__Add__0(exp__0,e1__0),
   path__Add__1(exp__0,e2__0),
   aeval(e1__0,node__0,prog__0,v1__1),
   aeval(e2__0,node__0,prog__0,v2__1),
   un___VNum(v1__1,n1),
   un___VNum(v2__1,n2),
   (n1+n2) <= -100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [131:1-131:296])_");
if(!(rel_39_delta_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_46_new_disconnected6->empty() && rel_7_disconnected6->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_46_new_disconnected6_op_ctxt,rel_46_new_disconnected6->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_46_new_disconnected6->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_46_new_disconnected6->empty())) break;
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) <= ramBitCast<RamSigned>(RamSigned(-100)))) {
if( !(rel_46_new_disconnected6->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_46_new_disconnected6->insert(tuple,READ_OP_CONTEXT(rel_46_new_disconnected6_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
();}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(+disconnected7() :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__Add(exp__1),
   path__Add__0(exp__1,e1__1),
   path__Add__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1__2),
   aeval(e2__1,node__1,prog__1,v2__2),
   un___VNum(v1__2,n1),
   un___VNum(v2__2,n2),
   (n1+n2) > -100,
   (n1+n2) >= 100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [132:1-132:310])_");
if(!(rel_123_un_VNum->empty()) && !(rel_30_delta_input_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_47_new_disconnected7->empty() && rel_8_disconnected7->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_30_delta_input_aeval_op_ctxt,rel_30_delta_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_47_new_disconnected7_op_ctxt,rel_47_new_disconnected7->createContext());
for(const auto& env0 : *rel_30_delta_input_aeval) {
if( !(rel_47_new_disconnected7->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env1[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env3[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) >= ramBitCast<RamSigned>(RamSigned(100)))) {
if( !(rel_47_new_disconnected7->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_47_new_disconnected7->insert(tuple,READ_OP_CONTEXT(rel_47_new_disconnected7_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected7() :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__Add(exp__1),
   path__Add__0(exp__1,e1__1),
   path__Add__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1__2),
   aeval(e2__1,node__1,prog__1,v2__2),
   un___VNum(v1__2,n1),
   un___VNum(v2__2,n2),
   (n1+n2) > -100,
   (n1+n2) >= 100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [132:1-132:310])_");
if(!(rel_123_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_47_new_disconnected7->empty() && rel_8_disconnected7->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_47_new_disconnected7_op_ctxt,rel_47_new_disconnected7->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_47_new_disconnected7->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
if( !(rel_20_delta_aeval->contains(Tuple<RamDomain,4>{{ramBitCast(env2[1]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env4[3])}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) >= ramBitCast<RamSigned>(RamSigned(100)))) {
if( !(rel_47_new_disconnected7->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_47_new_disconnected7->insert(tuple,READ_OP_CONTEXT(rel_47_new_disconnected7_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected7() :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__Add(exp__1),
   path__Add__0(exp__1,e1__1),
   path__Add__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1__2),
   aeval(e2__1,node__1,prog__1,v2__2),
   un___VNum(v1__2,n1),
   un___VNum(v2__2,n2),
   (n1+n2) > -100,
   (n1+n2) >= 100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [132:1-132:310])_");
if(!(rel_123_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_20_delta_aeval->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_47_new_disconnected7->empty() && rel_8_disconnected7->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_20_delta_aeval_op_ctxt,rel_20_delta_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_47_new_disconnected7_op_ctxt,rel_47_new_disconnected7->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_47_new_disconnected7->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_20_delta_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_20_delta_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env3[3]),ramBitCast(env5[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt)))) {
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) >= ramBitCast<RamSigned>(RamSigned(100)))) {
if( !(rel_47_new_disconnected7->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_47_new_disconnected7->insert(tuple,READ_OP_CONTEXT(rel_47_new_disconnected7_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected7() :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__Add(exp__1),
   path__Add__0(exp__1,e1__1),
   path__Add__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1__2),
   aeval(e2__1,node__1,prog__1,v2__2),
   un___VNum(v1__2,n1),
   un___VNum(v2__2,n2),
   (n1+n2) > -100,
   (n1+n2) >= 100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [132:1-132:310])_");
if(!(rel_123_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_39_delta_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_47_new_disconnected7->empty() && rel_8_disconnected7->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_47_new_disconnected7_op_ctxt,rel_47_new_disconnected7->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_47_new_disconnected7->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( !(rel_39_delta_un_VNum->contains(Tuple<RamDomain,2>{{ramBitCast(env4[3]),ramBitCast(env6[1])}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) >= ramBitCast<RamSigned>(RamSigned(100)))) {
if( !(rel_47_new_disconnected7->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_47_new_disconnected7->insert(tuple,READ_OP_CONTEXT(rel_47_new_disconnected7_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
();}
SignalHandler::instance()->setMsg(R"_(+disconnected7() :- 
   input__aeval(exp__1,node__1,prog__1),
   hasType__Add(exp__1),
   path__Add__0(exp__1,e1__1),
   path__Add__1(exp__1,e2__1),
   aeval(e1__1,node__1,prog__1,v1__2),
   aeval(e2__1,node__1,prog__1,v2__2),
   un___VNum(v1__2,n1),
   un___VNum(v2__2,n2),
   (n1+n2) > -100,
   (n1+n2) >= 100.
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [132:1-132:310])_");
if(!(rel_39_delta_un_VNum->empty()) && !(rel_97_input_aeval->empty()) && !(rel_123_un_VNum->empty()) && !(rel_74_aeval->empty()) && !(rel_106_path_Add_1->empty()) && !(rel_105_path_Add_0->empty()) && !(rel_83_hasType_Add->empty()) && rel_47_new_disconnected7->empty() && rel_8_disconnected7->empty()) {
[&](){
CREATE_OP_CONTEXT(rel_83_hasType_Add_op_ctxt,rel_83_hasType_Add->createContext());
CREATE_OP_CONTEXT(rel_105_path_Add_0_op_ctxt,rel_105_path_Add_0->createContext());
CREATE_OP_CONTEXT(rel_106_path_Add_1_op_ctxt,rel_106_path_Add_1->createContext());
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt,rel_39_delta_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_47_new_disconnected7_op_ctxt,rel_47_new_disconnected7->createContext());
for(const auto& env0 : *rel_97_input_aeval) {
if( !(rel_47_new_disconnected7->empty())) break;
if( rel_83_hasType_Add->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_83_hasType_Add_op_ctxt))) {
auto range = rel_105_path_Add_0->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_105_path_Add_0_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_106_path_Add_1->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_106_path_Add_1_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_74_aeval->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env2[1]), ramBitCast(env0[1]), ramBitCast(env0[2]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_123_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env3[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
for(const auto& env5 : range) {
if( !(rel_47_new_disconnected7->empty())) break;
auto range = rel_39_delta_un_VNum->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env4[3]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_39_delta_un_VNum_op_ctxt));
for(const auto& env6 : range) {
if( (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) > ramBitCast<RamSigned>(RamSigned(-100))) && (ramBitCast<RamSigned>((ramBitCast<RamSigned>(env5[1]) + ramBitCast<RamSigned>(env6[1]))) >= ramBitCast<RamSigned>(RamSigned(100)))) {
if( !(rel_47_new_disconnected7->empty())) break;
Tuple<RamDomain,0> tuple{{}};
rel_47_new_disconnected7->insert(tuple,READ_OP_CONTEXT(rel_47_new_disconnected7_op_ctxt));
break;
}
}
}
}
}
}
}
}
}
}
();}
SECTION_END
SECTIONS_END;
if(rel_53_new_final->empty() && rel_58_new_init->empty() && rel_55_new_freevars->empty() && rel_54_new_flow->empty() && rel_56_new_freevarsStm->empty() && rel_48_new_VBool->empty() && rel_52_new_exit_var->empty() && rel_49_new_VNum->empty() && rel_69_new_un_VBool->empty() && rel_66_new_input_freevars->empty() && rel_57_new_greaterThan->empty() && rel_70_new_un_VNum->empty() && rel_65_new_input_flow->empty() && rel_51_new_aeval->empty() && rel_59_new_input_VBool->empty() && rel_68_new_input_init->empty() && rel_50_new_add->empty() && rel_64_new_input_final->empty() && rel_67_new_input_freevarsStm->empty() && rel_60_new_input_VNum->empty() && rel_61_new_input_aeval->empty() && rel_62_new_input_entry_var->empty() && rel_63_new_input_exit_var->empty() && rel_45_new_disconnected5->empty() && rel_40_new_disconnected0->empty() && rel_41_new_disconnected1->empty() && rel_42_new_disconnected2->empty() && rel_43_new_disconnected3->empty() && rel_44_new_disconnected4->empty() && rel_46_new_disconnected6->empty() && rel_47_new_disconnected7->empty()) break;
[&](){
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_53_new_final_op_ctxt,rel_53_new_final->createContext());
for(const auto& env0 : *rel_53_new_final) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_77_final->insert(tuple,READ_OP_CONTEXT(rel_77_final_op_ctxt));
}
}
();std::swap(rel_22_delta_final, rel_53_new_final);
rel_53_new_final->purge();
[&](){
CREATE_OP_CONTEXT(rel_94_init_op_ctxt,rel_94_init->createContext());
CREATE_OP_CONTEXT(rel_58_new_init_op_ctxt,rel_58_new_init->createContext());
for(const auto& env0 : *rel_58_new_init) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_94_init->insert(tuple,READ_OP_CONTEXT(rel_94_init_op_ctxt));
}
}
();std::swap(rel_27_delta_init, rel_58_new_init);
rel_58_new_init->purge();
[&](){
CREATE_OP_CONTEXT(rel_80_freevars_op_ctxt,rel_80_freevars->createContext());
CREATE_OP_CONTEXT(rel_55_new_freevars_op_ctxt,rel_55_new_freevars->createContext());
for(const auto& env0 : *rel_55_new_freevars) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_80_freevars->insert(tuple,READ_OP_CONTEXT(rel_80_freevars_op_ctxt));
}
}
();std::swap(rel_24_delta_freevars, rel_55_new_freevars);
rel_55_new_freevars->purge();
[&](){
CREATE_OP_CONTEXT(rel_79_flow_op_ctxt,rel_79_flow->createContext());
CREATE_OP_CONTEXT(rel_54_new_flow_op_ctxt,rel_54_new_flow->createContext());
for(const auto& env0 : *rel_54_new_flow) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_79_flow->insert(tuple,READ_OP_CONTEXT(rel_79_flow_op_ctxt));
}
}
();std::swap(rel_23_delta_flow, rel_54_new_flow);
rel_54_new_flow->purge();
[&](){
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_56_new_freevarsStm_op_ctxt,rel_56_new_freevarsStm->createContext());
for(const auto& env0 : *rel_56_new_freevarsStm) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_81_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt));
}
}
();std::swap(rel_25_delta_freevarsStm, rel_56_new_freevarsStm);
rel_56_new_freevarsStm->purge();
[&](){
CREATE_OP_CONTEXT(rel_71_VBool_op_ctxt,rel_71_VBool->createContext());
CREATE_OP_CONTEXT(rel_48_new_VBool_op_ctxt,rel_48_new_VBool->createContext());
for(const auto& env0 : *rel_48_new_VBool) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_71_VBool->insert(tuple,READ_OP_CONTEXT(rel_71_VBool_op_ctxt));
}
}
();std::swap(rel_17_delta_VBool, rel_48_new_VBool);
rel_48_new_VBool->purge();
[&](){
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_52_new_exit_var_op_ctxt,rel_52_new_exit_var->createContext());
for(const auto& env0 : *rel_52_new_exit_var) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env0[3])}};
rel_75_exit_var->insert(tuple,READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
}
}
();std::swap(rel_21_delta_exit_var, rel_52_new_exit_var);
rel_52_new_exit_var->purge();
[&](){
CREATE_OP_CONTEXT(rel_72_VNum_op_ctxt,rel_72_VNum->createContext());
CREATE_OP_CONTEXT(rel_49_new_VNum_op_ctxt,rel_49_new_VNum->createContext());
for(const auto& env0 : *rel_49_new_VNum) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_72_VNum->insert(tuple,READ_OP_CONTEXT(rel_72_VNum_op_ctxt));
}
}
();std::swap(rel_18_delta_VNum, rel_49_new_VNum);
rel_49_new_VNum->purge();
[&](){
CREATE_OP_CONTEXT(rel_122_un_VBool_op_ctxt,rel_122_un_VBool->createContext());
CREATE_OP_CONTEXT(rel_69_new_un_VBool_op_ctxt,rel_69_new_un_VBool->createContext());
for(const auto& env0 : *rel_69_new_un_VBool) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_122_un_VBool->insert(tuple,READ_OP_CONTEXT(rel_122_un_VBool_op_ctxt));
}
}
();std::swap(rel_38_delta_un_VBool, rel_69_new_un_VBool);
rel_69_new_un_VBool->purge();
[&](){
CREATE_OP_CONTEXT(rel_102_input_freevars_op_ctxt,rel_102_input_freevars->createContext());
CREATE_OP_CONTEXT(rel_66_new_input_freevars_op_ctxt,rel_66_new_input_freevars->createContext());
for(const auto& env0 : *rel_66_new_input_freevars) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_102_input_freevars->insert(tuple,READ_OP_CONTEXT(rel_102_input_freevars_op_ctxt));
}
}
();std::swap(rel_35_delta_input_freevars, rel_66_new_input_freevars);
rel_66_new_input_freevars->purge();
[&](){
CREATE_OP_CONTEXT(rel_82_greaterThan_op_ctxt,rel_82_greaterThan->createContext());
CREATE_OP_CONTEXT(rel_57_new_greaterThan_op_ctxt,rel_57_new_greaterThan->createContext());
for(const auto& env0 : *rel_57_new_greaterThan) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_82_greaterThan->insert(tuple,READ_OP_CONTEXT(rel_82_greaterThan_op_ctxt));
}
}
();std::swap(rel_26_delta_greaterThan, rel_57_new_greaterThan);
rel_57_new_greaterThan->purge();
[&](){
CREATE_OP_CONTEXT(rel_123_un_VNum_op_ctxt,rel_123_un_VNum->createContext());
CREATE_OP_CONTEXT(rel_70_new_un_VNum_op_ctxt,rel_70_new_un_VNum->createContext());
for(const auto& env0 : *rel_70_new_un_VNum) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1])}};
rel_123_un_VNum->insert(tuple,READ_OP_CONTEXT(rel_123_un_VNum_op_ctxt));
}
}
();std::swap(rel_39_delta_un_VNum, rel_70_new_un_VNum);
rel_70_new_un_VNum->purge();
[&](){
CREATE_OP_CONTEXT(rel_101_input_flow_op_ctxt,rel_101_input_flow->createContext());
CREATE_OP_CONTEXT(rel_65_new_input_flow_op_ctxt,rel_65_new_input_flow->createContext());
for(const auto& env0 : *rel_65_new_input_flow) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_101_input_flow->insert(tuple,READ_OP_CONTEXT(rel_101_input_flow_op_ctxt));
}
}
();std::swap(rel_34_delta_input_flow, rel_65_new_input_flow);
rel_65_new_input_flow->purge();
[&](){
CREATE_OP_CONTEXT(rel_74_aeval_op_ctxt,rel_74_aeval->createContext());
CREATE_OP_CONTEXT(rel_51_new_aeval_op_ctxt,rel_51_new_aeval->createContext());
for(const auto& env0 : *rel_51_new_aeval) {
Tuple<RamDomain,4> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2]),ramBitCast(env0[3])}};
rel_74_aeval->insert(tuple,READ_OP_CONTEXT(rel_74_aeval_op_ctxt));
}
}
();std::swap(rel_20_delta_aeval, rel_51_new_aeval);
rel_51_new_aeval->purge();
[&](){
CREATE_OP_CONTEXT(rel_95_input_VBool_op_ctxt,rel_95_input_VBool->createContext());
CREATE_OP_CONTEXT(rel_59_new_input_VBool_op_ctxt,rel_59_new_input_VBool->createContext());
for(const auto& env0 : *rel_59_new_input_VBool) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_95_input_VBool->insert(tuple,READ_OP_CONTEXT(rel_95_input_VBool_op_ctxt));
}
}
();std::swap(rel_28_delta_input_VBool, rel_59_new_input_VBool);
rel_59_new_input_VBool->purge();
[&](){
CREATE_OP_CONTEXT(rel_104_input_init_op_ctxt,rel_104_input_init->createContext());
CREATE_OP_CONTEXT(rel_68_new_input_init_op_ctxt,rel_68_new_input_init->createContext());
for(const auto& env0 : *rel_68_new_input_init) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_104_input_init->insert(tuple,READ_OP_CONTEXT(rel_104_input_init_op_ctxt));
}
}
();std::swap(rel_37_delta_input_init, rel_68_new_input_init);
rel_68_new_input_init->purge();
[&](){
CREATE_OP_CONTEXT(rel_73_add_op_ctxt,rel_73_add->createContext());
CREATE_OP_CONTEXT(rel_50_new_add_op_ctxt,rel_50_new_add->createContext());
for(const auto& env0 : *rel_50_new_add) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_73_add->insert(tuple,READ_OP_CONTEXT(rel_73_add_op_ctxt));
}
}
();std::swap(rel_19_delta_add, rel_50_new_add);
rel_50_new_add->purge();
[&](){
CREATE_OP_CONTEXT(rel_100_input_final_op_ctxt,rel_100_input_final->createContext());
CREATE_OP_CONTEXT(rel_64_new_input_final_op_ctxt,rel_64_new_input_final->createContext());
for(const auto& env0 : *rel_64_new_input_final) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_100_input_final->insert(tuple,READ_OP_CONTEXT(rel_100_input_final_op_ctxt));
}
}
();std::swap(rel_33_delta_input_final, rel_64_new_input_final);
rel_64_new_input_final->purge();
[&](){
CREATE_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt,rel_103_input_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_67_new_input_freevarsStm_op_ctxt,rel_67_new_input_freevarsStm->createContext());
for(const auto& env0 : *rel_67_new_input_freevarsStm) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_103_input_freevarsStm->insert(tuple,READ_OP_CONTEXT(rel_103_input_freevarsStm_op_ctxt));
}
}
();std::swap(rel_36_delta_input_freevarsStm, rel_67_new_input_freevarsStm);
rel_67_new_input_freevarsStm->purge();
[&](){
CREATE_OP_CONTEXT(rel_96_input_VNum_op_ctxt,rel_96_input_VNum->createContext());
CREATE_OP_CONTEXT(rel_60_new_input_VNum_op_ctxt,rel_60_new_input_VNum->createContext());
for(const auto& env0 : *rel_60_new_input_VNum) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_96_input_VNum->insert(tuple,READ_OP_CONTEXT(rel_96_input_VNum_op_ctxt));
}
}
();std::swap(rel_29_delta_input_VNum, rel_60_new_input_VNum);
rel_60_new_input_VNum->purge();
[&](){
CREATE_OP_CONTEXT(rel_97_input_aeval_op_ctxt,rel_97_input_aeval->createContext());
CREATE_OP_CONTEXT(rel_61_new_input_aeval_op_ctxt,rel_61_new_input_aeval->createContext());
for(const auto& env0 : *rel_61_new_input_aeval) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_97_input_aeval->insert(tuple,READ_OP_CONTEXT(rel_97_input_aeval_op_ctxt));
}
}
();std::swap(rel_30_delta_input_aeval, rel_61_new_input_aeval);
rel_61_new_input_aeval->purge();
[&](){
CREATE_OP_CONTEXT(rel_98_input_entry_var_op_ctxt,rel_98_input_entry_var->createContext());
CREATE_OP_CONTEXT(rel_62_new_input_entry_var_op_ctxt,rel_62_new_input_entry_var->createContext());
for(const auto& env0 : *rel_62_new_input_entry_var) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_98_input_entry_var->insert(tuple,READ_OP_CONTEXT(rel_98_input_entry_var_op_ctxt));
}
}
();std::swap(rel_31_delta_input_entry_var, rel_62_new_input_entry_var);
rel_62_new_input_entry_var->purge();
[&](){
CREATE_OP_CONTEXT(rel_99_input_exit_var_op_ctxt,rel_99_input_exit_var->createContext());
CREATE_OP_CONTEXT(rel_63_new_input_exit_var_op_ctxt,rel_63_new_input_exit_var->createContext());
for(const auto& env0 : *rel_63_new_input_exit_var) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env0[1]),ramBitCast(env0[2])}};
rel_99_input_exit_var->insert(tuple,READ_OP_CONTEXT(rel_99_input_exit_var_op_ctxt));
}
}
();std::swap(rel_32_delta_input_exit_var, rel_63_new_input_exit_var);
rel_63_new_input_exit_var->purge();
if(!(rel_45_new_disconnected5->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_6_disconnected5_op_ctxt,rel_6_disconnected5->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_6_disconnected5->insert(tuple,READ_OP_CONTEXT(rel_6_disconnected5_op_ctxt));
}
();}
std::swap(rel_14_delta_disconnected5, rel_45_new_disconnected5);
rel_45_new_disconnected5->purge();
if(!(rel_40_new_disconnected0->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_1_disconnected0_op_ctxt,rel_1_disconnected0->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_1_disconnected0->insert(tuple,READ_OP_CONTEXT(rel_1_disconnected0_op_ctxt));
}
();}
std::swap(rel_9_delta_disconnected0, rel_40_new_disconnected0);
rel_40_new_disconnected0->purge();
if(!(rel_41_new_disconnected1->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_2_disconnected1_op_ctxt,rel_2_disconnected1->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_2_disconnected1->insert(tuple,READ_OP_CONTEXT(rel_2_disconnected1_op_ctxt));
}
();}
std::swap(rel_10_delta_disconnected1, rel_41_new_disconnected1);
rel_41_new_disconnected1->purge();
if(!(rel_42_new_disconnected2->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_3_disconnected2_op_ctxt,rel_3_disconnected2->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_3_disconnected2->insert(tuple,READ_OP_CONTEXT(rel_3_disconnected2_op_ctxt));
}
();}
std::swap(rel_11_delta_disconnected2, rel_42_new_disconnected2);
rel_42_new_disconnected2->purge();
if(!(rel_43_new_disconnected3->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_4_disconnected3_op_ctxt,rel_4_disconnected3->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_4_disconnected3->insert(tuple,READ_OP_CONTEXT(rel_4_disconnected3_op_ctxt));
}
();}
std::swap(rel_12_delta_disconnected3, rel_43_new_disconnected3);
rel_43_new_disconnected3->purge();
if(!(rel_44_new_disconnected4->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_5_disconnected4_op_ctxt,rel_5_disconnected4->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_5_disconnected4->insert(tuple,READ_OP_CONTEXT(rel_5_disconnected4_op_ctxt));
}
();}
std::swap(rel_13_delta_disconnected4, rel_44_new_disconnected4);
rel_44_new_disconnected4->purge();
if(!(rel_46_new_disconnected6->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_7_disconnected6_op_ctxt,rel_7_disconnected6->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_7_disconnected6->insert(tuple,READ_OP_CONTEXT(rel_7_disconnected6_op_ctxt));
}
();}
std::swap(rel_15_delta_disconnected6, rel_46_new_disconnected6);
rel_46_new_disconnected6->purge();
if(!(rel_47_new_disconnected7->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_8_disconnected7_op_ctxt,rel_8_disconnected7->createContext());
Tuple<RamDomain,0> tuple{{}};
rel_8_disconnected7->insert(tuple,READ_OP_CONTEXT(rel_8_disconnected7_op_ctxt));
}
();}
std::swap(rel_16_delta_disconnected7, rel_47_new_disconnected7);
rel_47_new_disconnected7->purge();
iter++;
}
iter = 0;
rel_22_delta_final->purge();
rel_53_new_final->purge();
rel_27_delta_init->purge();
rel_58_new_init->purge();
rel_24_delta_freevars->purge();
rel_55_new_freevars->purge();
rel_23_delta_flow->purge();
rel_54_new_flow->purge();
rel_25_delta_freevarsStm->purge();
rel_56_new_freevarsStm->purge();
rel_17_delta_VBool->purge();
rel_48_new_VBool->purge();
rel_21_delta_exit_var->purge();
rel_52_new_exit_var->purge();
rel_18_delta_VNum->purge();
rel_49_new_VNum->purge();
rel_38_delta_un_VBool->purge();
rel_69_new_un_VBool->purge();
rel_35_delta_input_freevars->purge();
rel_66_new_input_freevars->purge();
rel_26_delta_greaterThan->purge();
rel_57_new_greaterThan->purge();
rel_39_delta_un_VNum->purge();
rel_70_new_un_VNum->purge();
rel_34_delta_input_flow->purge();
rel_65_new_input_flow->purge();
rel_20_delta_aeval->purge();
rel_51_new_aeval->purge();
rel_28_delta_input_VBool->purge();
rel_59_new_input_VBool->purge();
rel_37_delta_input_init->purge();
rel_68_new_input_init->purge();
rel_19_delta_add->purge();
rel_50_new_add->purge();
rel_33_delta_input_final->purge();
rel_64_new_input_final->purge();
rel_36_delta_input_freevarsStm->purge();
rel_67_new_input_freevarsStm->purge();
rel_29_delta_input_VNum->purge();
rel_60_new_input_VNum->purge();
rel_30_delta_input_aeval->purge();
rel_61_new_input_aeval->purge();
rel_31_delta_input_entry_var->purge();
rel_62_new_input_entry_var->purge();
rel_32_delta_input_exit_var->purge();
rel_63_new_input_exit_var->purge();
rel_14_delta_disconnected5->purge();
rel_45_new_disconnected5->purge();
rel_9_delta_disconnected0->purge();
rel_40_new_disconnected0->purge();
rel_10_delta_disconnected1->purge();
rel_41_new_disconnected1->purge();
rel_11_delta_disconnected2->purge();
rel_42_new_disconnected2->purge();
rel_12_delta_disconnected3->purge();
rel_43_new_disconnected3->purge();
rel_13_delta_disconnected4->purge();
rel_44_new_disconnected4->purge();
rel_15_delta_disconnected6->purge();
rel_46_new_disconnected6->purge();
rel_16_delta_disconnected7->purge();
rel_47_new_disconnected7->purge();
if (performIO) rel_94_init->purge();
if (performIO) rel_80_freevars->purge();
if (performIO) rel_79_flow->purge();
if (performIO) rel_71_VBool->purge();
if (performIO) rel_72_VNum->purge();
if (performIO) rel_122_un_VBool->purge();
if (performIO) rel_102_input_freevars->purge();
if (performIO) rel_82_greaterThan->purge();
if (performIO) rel_123_un_VNum->purge();
if (performIO) rel_101_input_flow->purge();
if (performIO) rel_74_aeval->purge();
if (performIO) rel_95_input_VBool->purge();
if (performIO) rel_104_input_init->purge();
if (performIO) rel_73_add->purge();
if (performIO) rel_100_input_final->purge();
if (performIO) rel_103_input_freevarsStm->purge();
if (performIO) rel_83_hasType_Add->purge();
if (performIO) rel_96_input_VNum->purge();
if (performIO) rel_97_input_aeval->purge();
if (performIO) rel_85_hasType_GreaterThan->purge();
if (performIO) rel_98_input_entry_var->purge();
if (performIO) rel_99_input_exit_var->purge();
if (performIO) rel_84_hasType_Assign->purge();
if (performIO) rel_90_hasType_VBool->purge();
if (performIO) rel_89_hasType_Skip->purge();
if (performIO) rel_88_hasType_Sequence->purge();
if (performIO) rel_92_hasType_Var->purge();
if (performIO) rel_116_path_Sequence_1->purge();
if (performIO) rel_86_hasType_If->purge();
if (performIO) rel_87_hasType_Num->purge();
if (performIO) rel_91_hasType_VNum->purge();
if (performIO) rel_93_hasType_While->purge();
if (performIO) rel_112_path_If_1->purge();
if (performIO) rel_111_path_If_0->purge();
if (performIO) rel_105_path_Add_0->purge();
if (performIO) rel_113_path_If_2->purge();
if (performIO) rel_108_path_Assign_1->purge();
if (performIO) rel_110_path_GreaterThan_1->purge();
if (performIO) rel_106_path_Add_1->purge();
if (performIO) rel_121_path_While_1->purge();
if (performIO) rel_109_path_GreaterThan_0->purge();
if (performIO) rel_115_path_Sequence_0->purge();
if (performIO) rel_120_path_While_0->purge();
if (performIO) rel_107_path_Assign_0->purge();
if (performIO) rel_118_path_VNum_0->purge();
if (performIO) rel_119_path_Var_0->purge();
if (performIO) rel_114_path_Num_0->purge();
if (performIO) rel_117_path_VBool_0->purge();
if (performIO) rel_6_disconnected5->purge();
if (performIO) rel_1_disconnected0->purge();
if (performIO) rel_2_disconnected1->purge();
if (performIO) rel_3_disconnected2->purge();
if (performIO) rel_4_disconnected3->purge();
if (performIO) rel_5_disconnected4->purge();
if (performIO) rel_7_disconnected6->purge();
if (performIO) rel_8_disconnected7->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_23(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__Skip"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_89_hasType_Skip);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_24(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
SignalHandler::instance()->setMsg(R"_(final_var(prog,out_0__0,out_1__0) :- 
   ext_input__final_var(prog),
   final(prog,s),
   freevarsStm(prog,out_0__0),
   exit_var(s,prog,out_0__0,out_1__0).
in file /Volumes/home/projects/inca/souffle-frontend/benchmark/generated/dataflow/ex1/analysis.dl [70:1-70:151])_");
if(!(rel_75_exit_var->empty()) && !(rel_81_freevarsStm->empty()) && !(rel_76_ext_input_final_var->empty()) && !(rel_77_final->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_76_ext_input_final_var_op_ctxt,rel_76_ext_input_final_var->createContext());
CREATE_OP_CONTEXT(rel_77_final_op_ctxt,rel_77_final->createContext());
CREATE_OP_CONTEXT(rel_81_freevarsStm_op_ctxt,rel_81_freevarsStm->createContext());
CREATE_OP_CONTEXT(rel_75_exit_var_op_ctxt,rel_75_exit_var->createContext());
CREATE_OP_CONTEXT(rel_78_final_var_op_ctxt,rel_78_final_var->createContext());
for(const auto& env0 : *rel_76_ext_input_final_var) {
auto range = rel_77_final->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_77_final_op_ctxt));
for(const auto& env1 : range) {
auto range = rel_81_freevarsStm->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_81_freevarsStm_op_ctxt));
for(const auto& env2 : range) {
auto range = rel_75_exit_var->lowerUpperRange_1110(Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[0]), ramBitCast(env2[1]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,4>{{ramBitCast(env1[1]), ramBitCast(env0[0]), ramBitCast(env2[1]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_75_exit_var_op_ctxt));
for(const auto& env3 : range) {
Tuple<RamDomain,3> tuple{{ramBitCast(env0[0]),ramBitCast(env2[1]),ramBitCast(env3[3])}};
rel_78_final_var->insert(tuple,READ_OP_CONTEXT(rel_78_final_var_op_ctxt));
}
}
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","prog\tout_0__0\tout_1__0"},{"name","final_var"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"params\": [\"prog\", \"out_0__0\", \"out_1__0\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"+:Stm\", \"s:symbol\", \"+:Val\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_78_final_var);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (performIO) rel_77_final->purge();
if (performIO) rel_81_freevarsStm->purge();
if (performIO) rel_75_exit_var->purge();
if (performIO) rel_76_ext_input_final_var->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_25(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__While"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_93_hasType_While);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_26(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","prog"},{"fact-dir","generated/dataflow/ex1"},{"name","ext_input__final_var"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"prog\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Stm\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_76_ext_input_final_var);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_27(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__Var"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Exp\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_92_hasType_Var);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_28(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out"},{"fact-dir","generated/dataflow/ex1"},{"name","hasType__Add"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"out\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"+:Exp\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_83_hasType_Add);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_29(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Add__0"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"+:Exp\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_105_path_Add_0);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_30(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","out\tfield"},{"fact-dir","generated/dataflow/ex1"},{"name","path__Add__1"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"out\", \"field\"]}}"},{"types","{\"ADTs\": {\"+:Exp\": {\"arity\": 4, \"branches\": [{\"name\": \"Add\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"GreaterThan\", \"types\": [\"+:Exp\", \"+:Exp\"]}, {\"name\": \"Num\", \"types\": [\"i:number\"]}, {\"name\": \"Var\", \"types\": [\"s:symbol\"]}]}, \"+:Stm\": {\"arity\": 5, \"branches\": [{\"name\": \"Assign\", \"types\": [\"s:symbol\", \"+:Exp\"]}, {\"name\": \"If\", \"types\": [\"+:Exp\", \"+:Stm\", \"+:Stm\"]}, {\"name\": \"Sequence\", \"types\": [\"+:Stm\", \"+:Stm\"]}, {\"name\": \"Skip\", \"types\": []}, {\"name\": \"While\", \"types\": [\"+:Exp\", \"+:Stm\"]}]}, \"+:Val\": {\"arity\": 2, \"branches\": [{\"name\": \"VBool\", \"types\": [\"u:unsigned\"]}, {\"name\": \"VNum\", \"types\": [\"i:number\"]}]}}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"+:Exp\", \"+:Exp\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_106_path_Add_1);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
};
SouffleProgram *newInstance_analysis(){return new Sf_analysis;}
SymbolTable *getST_analysis(SouffleProgram *p){return &reinterpret_cast<Sf_analysis*>(p)->symTable;}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_analysis: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_analysis();
};
public:
factory_Sf_analysis() : ProgramFactory("analysis"){}
};
extern "C" {
factory_Sf_analysis __factory_Sf_analysis_instance;
}
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(generated/dataflow/ex1/analysis.dl)",
R"()",
R"()",
false,
R"()",
1);
if (!opt.parse(argc,argv)) return 1;
souffle::Sf_analysis obj;
#if defined(_OPENMP) 
obj.setNumThreads(opt.getNumJobs());

#endif
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
