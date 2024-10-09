#include <stack>

// Advanced Queue that calculates min, max, OR, and AND of numbers inside this queue in average O(1)
template<typename T>
struct Que {
private:
    struct {
        stack<T> s, mn, mx, OR, AND;
        stack<T> *members[5] = {&s, &mn, &mx, &OR, &AND};
    } left;

    struct {
        stack<T> s, mn, mx, OR, AND;
        stack<T> *members[5] = {&s, &mn, &mx, &OR, &AND};
    } right;

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
            for (auto mem: right.members)
                mem->pop();
        }
    }

public:
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
        }
    }

    T mn() {
        if (right.mn.empty()) return left.mn.top();
        if (left.mn.empty()) return right.mn.top();
        return min(left.mn.top(), right.mn.top());
    }

    T mx() {
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
};
