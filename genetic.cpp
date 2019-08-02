#include <Rcpp.h>
#include <iostream>
#include <stdio.h>
#include <string>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export]]
Function test(Function fun1, Function fun2, NumericVector &a)
{
	return fun1(fun2(a,a), a);
}

// [[Rcpp::export]]
List generate_fml(List& func_vec, List& data_prc)
{
	List r(7);
  R_xlen_t s1 = func_vec.size();
  R_xlen_t s2 = data_prc.size();
	NumericVector ind1;
	for (int i = 0; i < 3; i++)
	{
		ind1.push_back(rand() % s1);
	}
	NumericVector ind2;
	for (int i = 0; i < 4; i++)
	{
		ind2.push_back(rand() % s2);
	}
	for (int i = 0; i < 3; i++)
	{
		r[i] = func_vec[ind1[i]];
	}
	for (int i = 3; i < 7; i++)
	{
		r[i] = data_prc[ind2[i - 3]];
	}
	return r;
}



// typedef struct func // struct of operator we will use for node
// {
// 	func() = default;

// 	virtual ~func() = default;
// };

// typedef struct BiTNode // Binary tree node
// {
// 	func *f;
// 	struct BiTNode *lchild, *rchild;
// };

// int
// score(NumericVector x) // 函数：求个体的适应度；
// {
// }

// void *crossover(chrom popnext[4]) // 函数：交叉操作；
// {

// 	int random;
// 	int i;

// 	return (0);
// }

// void *mutation(chrom popnext[4]) // 函数：变异操作；
// {

// 	int random;
// 	int row, col, value;
// 	random = rand() % 50;
// 	if (random == 25)
// 	{
// 	}

// 	return (0);
// }

// int main()
// {

// 	return 0;
// }