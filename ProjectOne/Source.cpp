#include <iostream>
#include <vector>
#include <memory>
#include <algorithm> 

using namespace std;

typedef std::vector<std::vector<double>>   matrTwo;
typedef std::vector<double>   matrOne;

class matrix {
public:
	matrix(int width, int height);
	/*matrix inverse();
	matrix transpose();
	double  det(matrix matA, int n);*/

	void   multiply(const matrix &firstSecond, const matrix &matrSecond);
	void multiply(const matrOne &massFirst, matrOne &massSecond);
	void   add(const matrix &firstSecond, const matrix &matrSecond);
	void   sub(const matrix &matrFirst, const matrix &matrSecond);
	double determin();
	void  transport();
	void cofactor();
	void setElement(int i = 0, int j = 0, double value = 0) {
		this->data[i][j] = value;
	};
	double getElement(int i = 0, int j = 0) {
		return this->data[i][j];
	};
	int getSize() {
		return this->width;
	};

	friend std::ostream& operator<<(std::ostream& str, matrix & output) {
		std::cout << "-----   Matrix  -----" << std::endl;

		for (matrTwo::iterator i = output.data.begin(); i != output.data.end(); ++i)
		{
			for (matrOne::iterator j = (*i).begin(); j != (*i).end(); ++j)
			{
				std::cout << (*j) << "  ";
			}
			std::cout << std::endl;
		}
		return str;
	};
	friend std::istream& operator>>(std::istream& str, matrix& input) {

		std::cout << "Input   -----   Matrix  -----" << std::endl;
		auto i_(1), j_(1);

		for (matrTwo::iterator i = input.data.begin(); i != input.data.end(); ++i, i_++)
		{
			for (matrOne::iterator j = (*i).begin(); j != (*i).end(); ++j, j_++)
			{
				std::cout << "Input matrix [" << i_ << "][" << j_ << "]   -  ";
				std::cin >> (*j);
			}
			j_ = 1;
		}
		return str;
	};



private:

	double det(matrix& a, int n);
	void minor(matrix& b, matrix a, int i, int n);
	void transpose(matrix& matA, matrix& matrix_cofactor);
	int width, height;
	bool square;
	matrTwo data;


};

//++++++++++++++++++++++++++++++++++++++++++++++++++++
matrix::matrix(int height = 1, int width = 1) : height(height), width(width) {

	if (width == height)
	{
		square = true;
	}
	else
	{
		square = false;
	}
	data.resize(this->height);
	for (matrTwo::iterator itr = data.begin(); itr != data.end(); ++itr)
	{
		itr->resize(width);
	}

	for (matrTwo::iterator i = data.begin(); i != data.end(); ++i)
	{
		for (matrOne::iterator j = (*i).begin(); j != (*i).end(); ++j)
		{
			(*j) = 0;
		}
	}


}

void matrix::multiply(const matrix &matrFirst, const matrix &matrSecond)
{
	if (!((matrFirst.width == matrSecond.height) && (matrFirst.height == matrSecond.width))) {
		throw 1;
		return;
	}
	matrix matrThird(matrFirst.width, matrSecond.height);
	for (int row = 0; row < matrFirst.width; row++) {
		for (int col = 0; col < matrFirst.width; col++) {
			for (int inner = 0; inner < matrFirst.height; inner++) {
				matrThird.data[row][col] += matrSecond.data[row][inner] * matrFirst.data[inner][col];
			}
		}
	}
	*this = matrThird;
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++
void matrix::multiply(const matrOne &massFirst, matrOne &massSecond)
{
	for (int row = 0; row < this->width; row++) {
		for (int inner = 0; inner < this->height; inner++) {
			massSecond[row] += massFirst[inner] * this->data[row][inner];
		}
	}
}

//++++++++++++++++++++++++++++++++++++++++++++++++++++
void matrix::add(const matrix &matrFirst, const matrix &matrSecond)
{
	if (!((matrFirst.width == matrSecond.width) && (matrFirst.height == matrSecond.height))) {
		throw 1;
		return;
	}
	matrix matrThird(matrFirst.width, matrFirst.height);
	for (int row = 0; row < matrFirst.width; row++) {
		for (int col = 0; col < matrFirst.height; col++) {
			matrThird.data[row][col] = matrFirst.data[row][col] + matrSecond.data[row][col];
		}
	}
	*this = matrThird;
}

//++++++++++++++++++++++++++++++++++++++++++++++++++++
void matrix::sub(const matrix &matrFirst, const matrix &matrSecond)
{
	if (!((matrFirst.width == matrSecond.width) && (matrFirst.height == matrSecond.height))) {
		throw 1;
		return;
	}
	matrix matrThird(matrFirst.width, matrFirst.height);
	for (int row = 0; row < matrFirst.width; row++) {
		for (int col = 0; col < matrFirst.height; col++) {
			matrThird.data[row][col] = matrFirst.data[row][col] - matrSecond.data[row][col];
		}
	}
	*this = matrThird;
}

//++++++++++++++++++++++++++++++++++++++++++++++++++++
void matrix::minor(matrix& b, matrix a, int i, int n) {
	int j, l, h = 0, k = 0;
	for (l = 1; l < n; l++) {
		for (j = 0; j < n; j++) {
			if (j == i)
				continue;
			b.data[h][k] = a.data[l][j];
			k++;
			if (k == (n - 1)) {
				h++;
				k = 0;
			}
		}
	}
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++

double matrix::det(matrix& a, int n) {
	int i;
	matrix b(n, n);

	double sum = 0;
	if (n == 1)
		return a.data[0][0];
	else if (n == 2)
		return (a.data[0][0] * a.data[1][1] - a.data[0][1] * a.data[1][0]);
	else
		for (i = 0; i < n; i++) {
			minor(b, a, i, n);
			sum = (double)(sum + a.data[0][i] * pow(-1, i)*det(b, (n - 1)));
		}
	return sum;
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++
double matrix::determin() {
	if (!(this->square)) {
		throw 1;
		return 0;
	}
	return det(*this, this->height);
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++
void  matrix::transport() {

	if (!this->determin()) {
		throw 1;
		return;
	}
	matrix matA(this->height, this->width);
	int i, j;

	for (int i = 0; i < this->height; i++) {
		for (int j = 0; j < this->width; j++) {
			matA.data[j][i] = this->data[i][j];
		}
	}
	*this = matA;
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++
void matrix::transpose(matrix& matA, matrix& matrix_cofactor) {
	auto size = this->width;
	matrix m_transpose(size, size);
	matrix m_inverse(size, size);

	int i, j;

	for (i = 0; i < size; i++)
	{
		for (j = 0; j < size; j++)
		{
			m_transpose.data[i][j] = matrix_cofactor.data[j][i];
		}
	}
	auto det = matA.determin();
	for (i = 0; i < size; i++)
	{
		for (j = 0; j < size; j++)
		{
			m_inverse.data[i][j] = m_transpose.data[i][j] / det;
		}
	}
	matA = m_inverse;
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++
void matrix::cofactor()
{

	if (!this->determin()) {
		throw 1;
		return;
	}

	auto size = this->height;
	matrix m_cofactor(size, size);
	matrix matrix_cofactor(size, size);
	int p, q, m, n, i, j;
	for (q = 0; q < size; q++)
	{
		for (p = 0; p < size; p++)
		{
			m = 0;
			n = 0;
			for (i = 0; i < size; i++)
			{
				for (j = 0; j < size; j++)
				{
					if (i != q && j != p)
					{
						m_cofactor.data[m][n] = this->data[i][j];
						if (n < (size - 2))
							n++;
						else
						{
							n = 0;
							m++;
						}
					}
				}
			}
			matrix_cofactor.data[q][p] = pow(-1, q + p) * this->det(m_cofactor, size - 1);
		}
	}
	this->transpose(*this, matrix_cofactor);
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++



void metodCramar() {
	int size;
	std::cout << "Input quantity - ";
	std::cin >> size;
	std::cout << "Input matric value\n";
	matrix matrA(size, size);
	std::cin >> matrA;
	matrOne value;
	matrOne  result;
	matrix mass(size, size);
	std::cout << "Input value result\n";
	value.resize(size);

	auto mainDeter = matrA.determin();

	auto i_ = 1, count = 1;
	for (matrOne::iterator j = value.begin(); j != value.end(); ++j, i_++)
	{
		std::cout << "Input value [" << i_ << "] -  ";
		std::cin >> (*j);
	}

	for (auto i_ = 0; i_ < size; i_++) {
		mass = matrA;
		for (auto j_ = 0; j_ < size; j_++) {
			mass.setElement(j_, i_, value[j_]);
		}
		result.push_back(mass.determin() / mainDeter);
	}
	for (matrOne::iterator j = result.begin(); j != result.end(); ++j, count++)
	{
		std::cout << "Value X" << count << "  " << (*j) << std::endl;
	}
};
void metodInvert() {
	int size;
	std::cout << "Input quantity - ";
	std::cin >> size;
	std::cout << "Input matric value\n";
	matrix matrA(size, size);
	std::cin >> matrA;
	matrOne value;
	matrOne  result;
	std::cout << "Input value result\n";
	value.resize(size);
	result.resize(size);

	auto mainDeter = matrA.determin();

	auto i_ = 1, count = 1;
	for (matrOne::iterator j = value.begin(); j != value.end(); ++j, i_++)
	{
		std::cout << "Input value [" << i_ << "] -  ";
		std::cin >> (*j);
	}
	matrA.cofactor();
	std::cout << "Invert\n" << matrA;
	matrA.multiply(value, result);

	for (matrOne::iterator j = result.begin(); j != result.end(); ++j, count++)
	{
		std::cout << "Value X" << count << "  " << (*j) << std::endl;
	}
};
/*void metodLU() {
	int size;
	std::cout << "Input quantity - ";
	std::cin >> size;
	std::cout << "Input matric value\n";
	matrix matrA(size, size);
	matrix matrL(size, size);
	matrix matrU(size, size);
	std::cin >> matrA;
	matrOne value;
	matrOne  result;
	std::cout << "Input value result\n";
	value.resize(size);
	result.resize(size);

	auto i_ = 1, count = 1;
	for (matrOne::iterator j = value.begin(); j != value.end(); ++j, i_++)
	{
		std::cout << "Input value [" << i_ << "] -  ";
		std::cin >> (*j);
	}
	/*
	for (auto i_ = 0; i_ < size; i_++) {
		for (auto j_ = 0; j_ < size; j_++) {
			if (i_>j_)
			{
				matrL.setElement(i_, j_, matrA.getElement(i_, j_));
			}
			else
			{
				matrL.setElement(i_, j_, 0.0);
			}
		}
	}

	for (auto i_ = 0; i_ < size; i_++) {
		for (auto j_ = 0; j_ < size; j_++) {
			if (i_<j_)
			{
				if (i_ == j_) {
					matrU.setElement(i_, j_, 0);
				}
				else
				{
					matrU.setElement(i_, j_, 1);
				}
			}
			else
			{
				matrU.setElement(i_, j_, matrA.getElement(i_, j_));
			}
		}
	}

	double S;
	for (auto i_ = 0; i_ < size; i_++) {
		for (auto j_ = 0; j_ < size; j_++) {
			S = 0;
			if (i_>=j_)
			{
				for (auto k_ = 0; k_ < i_-1; k_++) {
					S += matrL.getElement(i_, k_)*matrU.getElement(k_, j_);
				}
				matrU.setElement((matrA.getElement(i_,j_)-S)/(matrL.getElement(i_,i_)));

			}
			else
			{
				for (auto k_ = 0; k_ < j_ - 1; k_++) {
					S += matrL.getElement(i_, k_)*matrU.getElement(k_, j_);
				}
				matrL.setElement((matrA.getElement(i_, j_) - S));

			}

		}
	}


	for (auto i_ = 0; i_ < size; i_++) {
		S = 0;
		for (auto k_ = 0; k_ < i_ - 1; k_++) {

		S = S+matrL.getElement(i_,k_) *

		}

	}

	*/

	/*
		for (matrOne::iterator j = result.begin(); j != result.end(); ++j, count++)
		{
			std::cout << "Value X" << count << "  " << (*j) << std::endl;
		}
	};
	*/



void LU(matrix& matrA, matrix& matrL, matrix& matrU)
{
	matrU = matrA;
	auto n = matrA.getSize();
	for (int i = 0; i < n; i++)
		for (int j = i; j < n; j++)
			matrL.setElement(j, i, (matrU.getElement(j, i) / matrU.getElement(i, i)));

	for (int k = 1; k < n; k++)
	{
		for (int i = k - 1; i < n; i++)
			for (int j = i; j < n; j++)
				matrL.setElement(j, i, (matrU.getElement(j, i) / matrU.getElement(i, i)));

		for (int i = k; i < n; i++)
			for (int j = k - 1; j < n; j++)
				matrU.setElement(i, j, matrU.getElement(i, j) - matrL.getElement(i, (k - 1)) * matrU.getElement((k - 1), j));
	}

};

void proisv(matrTwo A, matrTwo B, matrTwo &R, int n)
{
	for (int i = 0; i < n; i++)
		for (int j = 0; j < n; j++)
			for (int k = 0; k < n; k++)
				R[i][j] += A[i][k] * B[k][j];
}




void metodLU()
{
	int size;
	int n, i, k, j, p;
	std::cout << "Input quantity - ";
	std::cin >> size;
	std::cout << "Input matric value\n";
	matrix matrA(size, size);
	std::cin >> matrA;
	matrix matrL(size, size), matrU(size, size), matrR(size, size);
	matrOne z, x;
	matrOne value;
	value.resize(size);
	z.resize(size);
	x.resize(size);
	std::cout << "Input value result\n";
	int  v_ = 1;
	for (matrOne::iterator df = value.begin(); df != value.end(); ++df, v_++)
	{
		std::cout << "Input value [" << v_ << "] -  ";
		std::cin >> (*df);
	}

	double sum;
	n = size;
	for (k = 0; k < n; k++)
	{
		matrU.setElement(k, k, 1);
		for (i = k; i < n; i++)
		{
			sum = 0;
			for (p = 1; p <= k - 1; p++)
				sum += matrL.getElement(i, p) * matrU.getElement(p, k);
			matrL.setElement(i, k, (matrA.getElement(i, k) - sum));
		}

		for (j = k + 1; j < n; j++)
		{
			sum = 0;
			for (p = 1; p <= k - 1; p++)
				sum += matrL.getElement(k, p) * matrU.getElement(p, j);
			matrU.setElement(k, j, ((matrA.getElement(k, j) - sum) / matrL.getElement(k, k)));
		}
	}
	//******** Displaying LU matrix**********//
	std::cout << std::endl << std::endl << "LU matrix is " << std::endl;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < n; j++)
			std::cout << matrL.getElement(i, j) << "  ";
		std::cout << std::endl;
	}
	std::cout << std::endl;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < n; j++)
			std::cout << matrU.getElement(i, j) << "  ";
		std::cout << std::endl;
	}

	//***** FINDING Z; LZ=b*********//

	for (i = 0; i < n; i++)
	{                                        //forward subtitution method
		sum = 0;
		for (p = 0; p < i - 1; p++)
			sum += matrL.getElement(i, p) *  z[p];
		z[i] = (value[i] - sum) / matrL.getElement(i, i);
	}
	//********** FINDING X; UX=Z***********//

	for (i = n; i > 0; i--)
	{
		sum = 0;
		for (p = n; p > i; p--)
			sum += matrU.getElement(i, p) * x[p];
		x[i] = (z[i] - sum) / matrU.getElement(i, i);
	}
	//*********** DISPLAYING SOLUTION**************//
	std::cout << std::endl << "Set of solution is" << std::endl;
	for (i = 0; i < n; i++)
		std::cout << std::endl << x[i];


};



int main() {
	try
	{

		metodLU();


		//metodInvert();
		/*matrix  bb(3, 3);
		matrix  aa(3, 3);
		matrix  cc;
		std::cin >> bb;
		std::cout << bb;
		std::cin >> aa;
		std::cout << aa;
		cc.sub(aa,bb);
		std::cout << cc;*/
	}
	catch (int e)
	{
		std::cout << "Determin null matrix non-degenerate - " << e << '\n';
	}
	system("pause");
	return 0;
}
