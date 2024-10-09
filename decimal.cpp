class deci {
private:
    long long i64;
    double f64;

public:
    deci() {
        i64 = 0;
        f64 = 0;
    }

    deci(long long i, double f = 0) {
        i64 = i;
        f64 = f;
    }

    bool operator==(deci &other) const {
        if (i64 == other.i64 && abs(f64 - other.f64) < 1e-15)
            return true;
        return false;
    }

    bool operator!=(deci &other) const {
        return !(*this == other);
    }

    bool operator<(deci &other) const {
        if (*this != other && i64 < other.i64 && f64 < other.f64)
            return true;
        return false;
    }

    bool operator<=(deci &other) const {
        if (*this == other || *this < other)
            return true;
        return false;
    }

    bool operator>(deci &other) const {
        return !(*this <= other);
    }

    bool operator>=(deci &other) const {
        return !(*this < other);
    }

    deci operator+(deci &other) const {
        deci res;
        res.i64 = (*this).i64 + other.i64;
        res.f64 = (*this).f64 + other.f64;
        if (res.f64 >= 1) res.i64 += 1, res.f64 -= 1;
        return res;
    }

    deci operator-(deci &other) const {
        deci res;
        res.i64 = (*this).i64 - other.i64;
        res.f64 = (*this).f64 - other.f64;
        if (res.f64 < 0) res.i64 -= 1, res.f64 += 1;
        return res;
    }

    deci operator*(deci &other) const {
        deci res;
        res.i64 = (*this).i64 * other.i64;
        res.f64 = (*this).f64 * other.f64;

        res.i64 += (long long) ((*this).i64 * other.f64);
        res.f64 += ((*this).i64 * other.f64) - (long long) ((*this).i64 * other.f64);

        res.i64 += (long long) ((*this).f64 * other.i64);
        res.f64 += ((*this).f64 * other.i64) - (long long) ((*this).f64 * other.i64);

        res.i64 += (long long) res.f64;
        res.f64 -= (long long) res.f64;
        return res;
    }

    deci reciprocal() {
        if (*this == *deci(1) || *this == *deci(-1))
            return *this;
        deci res;
        long long l, h;
        if ((*this).i64) {
        } else {
        }
    }

    deci operator/(deci &other) {
        deci res;
        res.i64 = i64 * other.i64;
        res.f64 = f64 * other.f64;
        return res;
    }
};
