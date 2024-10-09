template<typename T>
class rational {
private:
    T up;
    T down;

    T gcd(T &a, T &b) {
        return b ? gcd(b, a % b) : a;
    }

public:
    rational() {
        up = 0;
        down = 1;
    }

    rational(T a, T b = 1) {
        up = a;
        down = b;
    }

    rational operator+(rational &other) {
        rational res;
        res.up = up * other.down + other.up * down;
        res.down = down * other.down;
        T g = gcd(res.up, res.down);
        res.up /= g;
        res.down /= g;
        return res;
    }

    rational operator-(rational &other) {
        rational res;
        res.up = up * other.down - other.up * down;
        res.down = down * other.down;
        T g = gcd(res.up, res.down);
        res.up /= g;
        res.down /= g;
        return res;
    }

    rational operator*(rational &other) {
        rational res;
        res.up = up * other.up;
        res.down = down * other.down;
        T g = gcd(res.up, res.down);
        res.up /= g;
        res.down /= g;
        return res;
    }

    rational operator/(rational &other) {
        rational res;
        res.up = up * other.down;
        res.down = down * other.up;
        T g = gcd(res.up, res.down);
        res.up /= g;
        res.down /= g;
        return res;
    }

    string str_ratio() {
        return to_string(up) + "/" + to_string(down);
    }
};
