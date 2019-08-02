#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector add(const NumericVector &x, const NumericVector &y)
{
    R_xlen_t size = x.size();
    NumericVector r(size);
    for (int i = 0; i < size; i++)
    {
        r(i) = x(i) + y(i);
    }
    return r;
}
// [[Rcpp::export]]
NumericVector sub(const NumericVector &x, const NumericVector &y)
{
    R_xlen_t size = x.size();
    NumericVector r(size);
    for (int i = 0; i < size; i++)
    {
        r(i) = x(i) - y(i);
    }
    return r;
}
// [[Rcpp::export]]
NumericVector mul(const NumericVector &x, const NumericVector &y)
{
    R_xlen_t size = x.size();
    NumericVector r(size);
    for (int i = 0; i < size; i++)
    {
        r(i) = x(i) * y(i);
    }
    return r;
}
// [[Rcpp::export]]
NumericVector div(const NumericVector &x, const NumericVector &y)
{
    R_xlen_t size = x.size();
    NumericVector r(size);
    for (int i = 0; i < size; i++)
    {
        r(i) = x(i) / y(i);
    }
    return r;
}
// [[Rcpp::export]]
NumericVector abs(const NumericVector &x, const NumericVector &y)
{
    R_xlen_t size = x.size();
    NumericVector r(size);
    for (int i = 0; i < size; i++)
    {
        r(i) = abs(x(i));
    }
    return r;
}
// [[Rcpp::export]]
NumericVector sqrt(const NumericVector &x, const NumericVector &y)
{
    R_xlen_t size = x.size();
    NumericVector r(size);
    for (int i = 0; i < size; i++)
    {
        r(i) = sqrt(x(i));
    }
    return r;
}
// [[Rcpp::export]]
NumericVector log(const NumericVector &x, const NumericVector &y)
{
    R_xlen_t size = x.size();
    NumericVector r(size);
    for (int i = 0; i < size; i++)
    {
        r(i) = log(x(i));
    }
    return r;
}
// [[Rcpp::export]]
NumericVector inv(const NumericVector &x, const NumericVector &y)
{
    R_xlen_t size = x.size();
    NumericVector r(size);
    for (int i = 0; i < size; i++)
    {
        r(i) = 1 / x(i);
    }
    return r;
}

// [[Rcpp::export]]
NumericVector argmin(const NumericVector &x, const NumericVector &y,
                     const int window = 16, const bool partial = true)
{
    R_xlen_t ind = 0;
    double value = x[0];
    R_xlen_t size = x.size();
    NumericVector r(size);
    for (R_xlen_t i = 0; i < window; i++)
    {
        if (x[i] < value)
        {
            ind = i;
            value = x[i];
        }
    }
    r[window] = ind + 1;
    for (R_xlen_t j = window + 1; j < size; j++)
    {
        if (r[j - 1] == 1)
        {
            ind = j - window;
            value = x[ind];
            for (R_xlen_t i = j - window; i < j; i++)
            {
                if (x[i] < value)
                {
                    ind = i;
                    value = x[i];
                }
            }
            r[j] = ind - j + window + 1;
        }
        else
        {
            r[j] = (x[r[j - 1] + j - window - 2] < x[j - 1]) ? (r[j - 1] - 1) : 16;
        }
    }
    if (partial)
    {
        r[0] = 1;
        for (R_xlen_t i = 1; i < window; i++)
        {
            if (x[i] < x[r[i - 1]-1])
            {
                r[i] = i + 1;
            }
            else
            {
                r[i] = r[i - 1];
            }
        }
    }
    return r;
}
// [[Rcpp::export]]
NumericVector argmax(const NumericVector &x, const NumericVector &y,
                     const int window = 16, const bool partial = true)
{
    R_xlen_t ind = 0;
    double value = x[0];
    R_xlen_t size = x.size();
    NumericVector r(size);
    for (R_xlen_t i = 0; i < window; i++)
    {
        if (x[i] > value)
        {
            ind = i;
            value = x[i];
        }
    }
    r[window] = ind + 1;
    for (R_xlen_t j = window + 1; j < size; j++)
    {
        if (r[j - 1] == 1)
        {
            ind = j - window;
            value = x[ind];
            for (R_xlen_t i = j - window; i < j; i++)
            {
                if (x[i] > value)
                {
                    ind = i;
                    value = x[i];
                }
            }
            r[j] = ind - j + window + 1;
        }
        else
        {
            r[j] = (x[r[j - 1] + j - window - 2] > x[j - 1]) ? (r[j - 1] - 1) : 16;
        }
    }
    if (partial)
    {
        r[0] = 1;
        for (R_xlen_t i = 1; i < window; i++)
        {
            if (x[i] > x[r[i - 1]-1])
            {
                r[i] = i + 1;
            }
            else
            {
                r[i] = r[i - 1];
            }
        }
    }
    return r;
}
// [[Rcpp::export]]
NumericVector delay(const NumericVector &x, const NumericVector &y,
                     const int window = 16)
{
    R_xlen_t size = x.size();
    NumericVector r(size);
    for (R_xlen_t i = window; i < size;i++){
        r[i] = x[i - window];
    }
        return r;
}
// [[Rcpp::export]]
NumericVector delta(const NumericVector &x, const NumericVector &y,
                     const int window = 16)
{
    R_xlen_t size = x.size();
    NumericVector r(size);
    for (R_xlen_t i = window; i < size;i++){
        r[i] = x[i] - x[i - window];
    }
        return r;
}