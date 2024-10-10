#include <stack>
#include <vector>

// Advanced Queue that calculates min, max, OR, and AND of numbers inside this queue in average O(1) 
// and GCD and LCM in average O(log x) where x is the maximum value inside the queue

template<typename T>
struct Que {
private:
    Que() {
    }

    bool G = false, L = false;

    struct {
        stack<T> s, mn, mx, OR, AND, gc, lc;
        vector<stack<T> *> members = {&s, &mn, &mx, &OR, &AND};
    } left;

    struct {
        stack<T> s, mn, mx, OR, AND, gc, lc;
        vector<stack<T> *> members = {&s, &mn, &mx, &OR, &AND};
    } right;

    T gcd(T a, T b) {
        return b ? gcd(b, a % b) : a;
    }

    T lcm(T a, T b) {
        return a / gcd(a, b) * b;
    }

    void trans() {
        auto top = right.s.top();
        for (auto mem: left.members)
            mem->push(top);
        for (auto mem: right.members)
            mem->pop();
        while (!right.s.empty()) {
            left.s.push(right.s.top());
            left.mn.push(min(right.s.top(), left.mn.top()));
            left.mx.push(max(right.s.top(), left.mx.top()));
            left.OR.push(right.s.top() | left.OR.top());
            left.AND.push(right.s.top() & left.AND.top());
            if (G) left.gc.push(gcd(right.s.top(), left.gc.top()));
            if (L) left.lc.push(lcm(right.s.top(), left.lc.top()));
            for (auto mem: right.members)
                mem->pop();
        }
    }

public:
    Que(bool support_GCD = false, bool support_LCM = false) {
        if (support_GCD) {
            G = true;
            right.members.push_back(&right.gc);
            left.members.push_back(&left.gc);
        }
        if (support_LCM) {
            L = true;
            left.members.push_back(&left.lc);
            right.members.push_back(&right.lc);
        }
    }

    bool empty() {
        return left.s.empty() && right.s.empty();
    }

    size_t size() {
        return right.s.size() + left.s.size();
    }

    void pop() {
        if (left.s.empty())
            trans();
        for (auto mem: left.members)
            mem->pop();
    }

    void push(T x) {
        if (right.s.empty()) {
            for (auto mem: right.members)
                mem->push(x);
        } else {
            right.s.push(x);
            right.mn.push(min(right.mn.top(), x));
            right.mx.push(max(right.mx.top(), x));
            right.OR.push(right.OR.top() | x);
            right.AND.push(right.AND.top() & x);
            if (G) right.gc.push(gcd(right.gc.top(), x));
            if (L) right.lc.push(lcm(right.lc.top(), x));
        }
    }

    T MIN() {
        if (right.mn.empty()) return left.mn.top();
        if (left.mn.empty()) return right.mn.top();
        return min(left.mn.top(), right.mn.top());
    }

    T MAX() {
        if (right.mx.empty()) return left.mx.top();
        if (left.mx.empty()) return right.mx.top();
        return max(left.mx.top(), right.mx.top());
    }

    T OR() {
        if (right.OR.empty()) return left.OR.top();
        if (left.OR.empty()) return right.OR.top();
        return left.OR.top() | right.OR.top();
    }

    T AND() {
        if (right.AND.empty()) return left.AND.top();
        if (left.AND.empty()) return right.AND.top();
        return left.AND.top() & right.AND.top();
    }

    T GCD() {
        if (right.gc.empty()) return left.gc.top();
        if (left.gc.empty()) return right.gc.top();
        return gcd(left.gc.top(), right.gc.top());
    }

    T LCM() {
        if (right.lc.empty()) return left.lc.top();
        if (left.lc.empty()) return right.lc.top();
        return lcm(left.lc.top(), right.lc.top());
    }
};
