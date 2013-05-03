//DUNE
//====
/* written by Tilak Raj Singh

        This is a simple implementation of inserters for a matrix class. Here a minute change has been applied from
        as described in the paper. Here on inserting all the values for a matrix in the destructor I have decreased 
        the size of each column to the max limit it is filled so as to save memory. Like row 1 contains 5 elements 
        and row 2 contains 3 elements and the slotsize for inserter is 7, then in the destructor I have decreased 
        the size of row 1 to 5 and row 2 to 3. On declaring a inserter again for the same matrix it is checked it 
        slotsize is greater than that of the row with max elements. If greater then each row is resized to the slot 
        size given else each row is resized to the size of the row with max number of elements.

For inserters an optional update parameter is also provided to change value of a index already inserted.
        0(default) -> to replace existing value
        1          -> to add value to existing value
        2          -> to subtract value from existing value
        3          -> to multiply value into existing value
        4          -> to store quotient on dividing existing value from given value

All the martix operations used use inserters.

Note :- compile with -std=c++0x

*/


#include<iostream>
#include<vector>
#include<map>
#include<utility>
#include<typeinfo>
#include <type_traits>
using namespace std;

//***********************************************************************************************************
/*Sample inserter class
        @var description->
                type    : to know the type of matrix called
                spare   : to store spare values
                max     : to store number of values to be stored in each row
                update  : to store update status
*/

//*********************************************************************************************************
template<class X>
class Inserter
{
        int slot,i,j;
        X* ref;
        typedef typename X::type type;
        map<pair<int,int>,type> spare;
        vector<int> max;
        int update;
        pair<int,int> p;
        pair<int,type> temp1,temp2;
        public:
                Inserter(X &mat,int s=5, int up=0)
                {
                        update=up;
                        ref=&mat;
                        max.resize(ref->nr);
                        for(i=0;i<max.size();i++)
                                max[i]=0;
                        if(ref->assigned==false)
                        {
                                if(ref->build==false)
                                {       
                                        slot=s;
                                        for(i=0;i<ref->nr;i++)
                                        {
                                                ref->colval[i].resize(s);
                                        }
                                }
                                else
                                {
                                        if(s>maxslot())//increase slot size of colval
                                                slot=s;
                                        else
                                                slot=maxslot();
                                        for(i=0;i<ref->nr;i++)
                                                ref->colval[i].resize(slot);
                                }
                                ref->build=true;
                                ref->assigned=true;
                        }
                        else
                                cout<<"Inserter already assigned to matrix\n";
                }
                int maxslot()//to return size of row having max entries
                {
                        int max=0;
                        for(i=0;i<ref->nr;i++)
                                if(ref->row[i]>max)
                                        max=ref->row[i];
                        return max;
                }       
                void insertval(int r, int c,type value)
                {
                        if(r>ref->nr-1||c>ref->nc-1)
                        {
                                cout<<"Index out of bounds\n";
                                return;
                        }
                        if(value==0)
                        //{
                        //      cout<<"Cant insert a zero value in sparse matrix\n";
                                return;
                        //}
                        else
                        {       
                                for(i=0;i<ref->row[r];i++)
                                {
                                        if(ref->colval[r][i].first==c)//if index already in the matrix
                                        {
                                                switch(update)
                                                {
                                                        case 0:ref->colval[r][i].second=value;return;
                                                        case 1:ref->colval[r][i].second+=value;return;
                                                        case 2:ref->colval[r][i].second-=value;return;
                                                        case 3:ref->colval[r][i].second*=value;return;
                                                        case 4:ref->colval[r][i].second/=value;return;
                                                }
                                        }
                                        else
                                        {
                                                if(ref->row[r]==slot)//if row full insertion in map
                                                {
                                                        p.first=r;p.second=c;
                                                        spare.insert(pair<pair<int,int>,type>(p,value));
                                                        max[r]++;return;
                                                }
                                                else
                                                {
                                                        if(ref->colval[r][i].first>c)//find correct index to be inserted and shift to maintain increasing order for columns
                                                        {
                                                                temp1.first=c;temp1.second=value;
                                                                ref->row[r]++;
                                                                for(;i<ref->row[r];i++)
                                                                {
                                                                        temp2=ref->colval[r][i];
                                                                        ref->colval[r][i]=temp1;
                                                                        temp1=temp2;
                                                                }
                                                                return;
                                                        }               
                                                }
                                        }
                                }
                                //if none condition true insert at end of row
                                ref->row[r]++;
                                ref->colval[r][i].first=c;
                                ref->colval[r][i].second=value;
                        }
                }
                ~Inserter()
                {
                        if(spare.begin()!=spare.end())//to insert values of spare back into matrix
                        {
                                int incre=0;
                                for(i=0;i<max.size();i++)//calculate new slot size
                                        if(max[i]>incre)
                                                incre=max[i];
                                slot+=incre;
                                for(i=0;i<ref->nr;i++)//increase slotsize of each column
                                        ref->colval[i].resize(slot);
                                typename map<pair<int,int>,type>::iterator itr=spare.begin();
                                type val;
                                for(;itr!=spare.end();itr++)//insertion
                                {
                                        p=itr->first;
                                        val=itr->second;
                                        insertval(p.first,p.second,val);
                                }
                        }
                        for(i=0;i<ref->nr;i++)//resizing of columns to their max limit
                                ref->colval[i].resize(ref->row[i]);
                        ref->assigned=false;
                }
};
//**********************************************************************************************************

/* Sample Matrix class for implementing inserters
        @var description->
                row             : to store the index where next value is to be inserted for each row
                colval          : to store column index and value pair for each row 
                build           : to know whether matrix build before
                assigned        : to know if inserter assigned to a matrix
                nc,nr           : no of rows and columns
*/

//**********************************************************************************************************
template<class Y>
class Matrix
{
        public:
                typedef Y type;
                vector<int> row;
                vector<vector<pair<int,Y> > >colval;
                bool build,assigned;
                int nr,nc;
                Matrix()
                {
                        build=assigned=false;
                }
                Matrix(int r, int c)
                {
                        build=assigned=false;
                        initialise(r,c);
                }
                void initialise(int r, int c)
                {
                        nr=r;nc=c;
                        row.resize(r);
                        colval.resize(r);
                        for(int i=0;i<nr;i++)//initialise each each row index
                                row[i]=0;
                }
                void print()//to print matrices
                {
                        cout<<"Row : rownum ->\tcol:value\n";
                        for(int i=0;i<nr;i++)
                        {
                                cout<<"Row : "<<i<<"->\t";
                                for(int j=0;j<row[i];j++)
                                        cout<<colval[i][j].first<<":"<<colval[i][j].second<<"\t";
                                cout<<endl;
                        }
                }
                Y getval(int r,int c)
                {
                        if(r<0||r>=nr)
                        {
                                cout<<"Row index not in matrix\n";
                                return 0;
                        }
                        int l=0,u=row[r]-1;
                        while(l<u)
                        {
                                int mid=(l+u)/2;
                                if(c<=colval[r][mid].first)
                                        u=mid;
                                else
                                        l=mid+1;
                        }
                        if(colval[r][l].first==c)
                                return colval[r][l].second;
                        else
                                return 0;
                }

//********************************************MATRIX OPERATIONS******************************************
                

//--------------------------------------Matrix with Scaler operations------------------------------------
                
                //Matrix A = const
                template<typename U>
                void operator = (U const x)
                {
                        if(typeid(int)==typeid(U)||typeid(char)==typeid(U)||typeid(float)==typeid(U)||typeid(double)==typeid(U))
                        {
                                for(int i=0;i<nr;i++)
                                {
                                        for(int j=0;j<nc;j++)
                                        {
                                                {
                                                        Inserter<Matrix<Y>  > ins(*this);
                                                        ins.insertval(i,j,x);
                                                }
                                        }
                                }
                        }
                }
                
                //Matrix A+=const
                template<typename U>
                Matrix<Y> operator += (U const x)
                {
                        if(typeid(int)==typeid(U)||typeid(char)==typeid(U)||typeid(float)==typeid(U)||typeid(double)==typeid(U))
                        {
                                for(int i=0;i<nr;i++)
                                {
                                        for(int j=0;j<row[i];j++)
                                        {
                                                Inserter<Matrix <Y> > ins(*this,5,1);
                                                ins.insertval(i,j,x);
                                        }
                                }
                        }
                        return(*this);
                }
                                
                //Matrix A + const
                template<typename U>
                Matrix<Y> operator + (U const x)
                {
                        Matrix<Y> res=*this;
                        res+=x;
                        return res;
                }

                //Matrix A-=const
                template<typename U>
                Matrix<Y> operator -= (U const x)
                {
                        if(typeid(int)==typeid(U)||typeid(char)==typeid(U)||typeid(float)==typeid(U)||typeid(double)==typeid(U))
                        {
                                for(int i=0;i<nr;i++)
                                {
                                        for(int j=0;j<row[i];j++)
                                        {
                                                Inserter<Matrix <Y> > ins(*this,5,2);
                                                ins.insertval(i,j,x);
                                        }
                                }
                        }
                        return(*this);
                }
                                
                //Matrix A - const               
                template<typename U>
                Matrix<Y> operator - (U const x)
                {
                        Matrix<Y> res=*this;
                        res-=x;
                        return res;
                }
                
                //Matrix A*=const
                template<typename U>
                Matrix<Y> operator *= (U const x)
                {
                        if(typeid(int)==typeid(U)||typeid(char)==typeid(U)||typeid(float)==typeid(U)||typeid(double)==typeid(U))
                        {
                                for(int i=0;i<nr;i++)
                                {
                                        for(int j=0;j<row[i];j++)
                                        {
                                                Inserter<Matrix <Y> > ins(*this,5,3);
                                                ins.insertval(i,j,x);
                                        }
                                }
                        }
                        return(*this);
                }
                                
                //Matrix A * const               
                template<typename U>
                Matrix<Y> operator * (U const x)
                {
                        Matrix<Y> res=*this;
                        res*=x;
                        return res;
                }
                
                //Matrix A/=const
                template<typename U>
                Matrix<Y> operator /= (U const x)
                {
                        if(typeid(int)==typeid(U)||typeid(char)==typeid(U)||typeid(float)==typeid(U)||typeid(double)==typeid(U))
                        {
                                for(int i=0;i<nr;i++)
                                {
                                        for(int j=0;j<row[i];j++)
                                        {
                                                Inserter<Matrix <Y> > ins(*this,5,4);
                                                ins.insertval(i,j,x);
                                        }
                                }
                        }
                        return(*this);
                }
                                
                //Matrix A / const               
                template<typename U>
                Matrix<Y> operator / (U const x)
                {
                        Matrix<Y> res=*this;
                        res/=x;
                        return res;
                }

//---------------------------------------------------------------------------------------------------------------

//----------------------------------------------Matrix with Matrices operations----------------------------------
                
                //Matrix A = Matrix B
                template <typename U>    
                /*Matrix<typename std::common_type<Y, U>::type>*/void operator = (Matrix<U> const & rhs)
                {
                        typedef typename std::common_type<Y, U>::type R;
                        Matrix<R> res(nr,nc);
                        if(nr!=rhs.nr||nc!=rhs.nc)
                                cout<<"Matrix sizes do not match to equate\n";
                        else
                        {
                                //nr=rhs.nr;nc=rhs.nc;
                                //row.resize(rhs.row.size());
                                for(int i=0;i<nr;i++)
                                {
                                        row[i]=rhs.row[i];
                                        for(int j=0;j<row[i];j++)
                                        {
                                                colval[i].resize(rhs.colval[i].size());
                                                colval[i][j]=rhs.colval[i][j];
                                        }
                                }
                        }
                }

                //Matrix A += Matrix B
                template <typename U>    
                Matrix<typename std::common_type<Y, U>::type>& operator+=(Matrix<U> & rhs)
                {
                        typedef typename std::common_type<Y, U>::type R;
                        Matrix<R> res(nr,nc);
                        if(nr!=rhs.nr||nc!=rhs.nc)
                                cout<<"Matrix sizes do not match to perform addition\n";
                        else
                        {
                                Inserter<Matrix<R> > ins(res);
                                for(int i=0;i<nr;i++)
                                        for(int j=0;j<nc;j++)
                                                ins.insertval(i,j,getval(i,j)+rhs.getval(i,j));
                        }       
                        *this=res;
                        return *this;           
                }

                //Matrix A + Matrix B
                template <typename U>    
                Matrix<typename std::common_type<Y, U>::type> operator+(Matrix<U> & rhs)
                {
                        Matrix<typename std::common_type<Y, U>::type> res=*this;
                        return(res+=rhs);
                }

                //Matrix A -= Matrix B
                template <typename U>    
                Matrix<typename std::common_type<Y, U>::type>& operator-=(Matrix<U> & rhs)
                {
                        typedef typename std::common_type<Y, U>::type R;
                        Matrix<R> res(nr,nc);
                        if(nr!=rhs.nr||nc!=rhs.nc)
                                cout<<"Matrix sizes do not match to perform subtraction\n";
                        else
                        {
                                Inserter<Matrix<R> > ins(res);
                                for(int i=0;i<nr;i++)
                                        for(int j=0;j<nc;j++)
                                                ins.insertval(i,j,getval(i,j)+rhs.getval(i,j));
                        }       
                        *this=res;
                        return *this;           
                }
                
                //Matrix A - Matrix B
                template <typename U>    
                Matrix<typename std::common_type<Y, U>::type> operator-(Matrix<U> & rhs)
                {
                        Matrix<typename std::common_type<Y, U>::type> res=*this;
                        return(res-=rhs);
                }

                //Matrix A * Matrix B
                template <typename U>
                Matrix<typename std::common_type<Y, U>::type> operator*(Matrix<U> & rhs)
                {
                        typedef typename std::common_type<Y, U>::type R;
                        Matrix<R> res(nr,nc);
                        if(nc!=rhs.nr)
                                cout<<"Matrix sizes do not match to perform multiplication\n";
                        else
                        {
                                //rhs.print();
                                Inserter<Matrix<R> > ins(res);
                                for(int i=0;i<nr;i++)
                                {
                                        for(int j=0;j<rhs.nc;j++)
                                        {
                                                R sum=0;
                                                for(int k=0;k<nr;k++)
                                                {
                                                        sum+=getval(i,k)*(rhs.getval(k,j));
                                                }
                                                //cout<<"i="<<i<<"\tj="<<j<<"sum="<<sum<<endl;
                                                ins.insertval(i,j,sum);
                                        }
                                }
                        }
                        return res;
                }               
//---------------------------------------------------------------------------------------------------------------
};

//*****************************************TESTING WITH MAIN FUNCTION********************************************
int main()
{
        Matrix<double> A(3,4),C(3,4);
        {
                Inserter<Matrix<double> > ins(A,4);
                ins.insertval(0,2,3);
                ins.insertval(1,2,5);
                //ins.insertval(1,1,4);
                ins.insertval(0,1,2);                   
                ins.insertval(0,0,6);
                ins.insertval(2,3,7);
                ins.insertval(2,0,8);
                ins.insertval(2,2,9);
        }
        //A.print();cout<<endl;
        {
                Inserter<Matrix<double> > ins1(A,3);
                ins1.insertval(2,3,9);
        }
        //A.print();cout<<endl;
        {
                Inserter<Matrix<double> > ins1(A,2);
                ins1.insertval(0,3,2);
        }
        //A.print();cout<<endl;
        Matrix<double> B(3,4);
        {
                Inserter<Matrix<double> > ins1(B,2);
                ins1.insertval(1,0,2);
                ins1.insertval(0,2,5);
        }
        //B.print();cout<<endl;
        //Matrix<double> C(3,4);
        A.print();cout<<endl;
        B.print();cout<<endl<<endl;
        for(int i=0;i<3;i++)
        {
                for(int j=0;j<4;j++)
                        cout<<B.getval(i,j)<<"\t";
                cout<<endl;
        }
        C=A+B;
        C.print();cout<<endl;
        A=B;
        A.print();cout<<endl;
        A=5;
        A.print();cout<<endl;
        A+=5;
        A.print();cout<<endl;
        A-=5;
        A.print();cout<<endl;
        A*=5;
        A.print();cout<<endl;
        A/=5;
        A.print();cout<<endl;
        //B.print();cout<<endl;
        //C.print();cout<<endl;
        A-=B;
        //C=A-B;
        A.print();cout<<endl;
        //C=5;
        //C.print();cout<<endl;
        Matrix<int> D(4,3);
        {
                Inserter<Matrix<int> > ins1(D,2);
                ins1.insertval(0,0,2);
                ins1.insertval(0,2,7);
                ins1.insertval(1,2,5);
                ins1.insertval(2,2,6);
        }       
        D.print();
        C=A*D;
        C.print();cout<<endl;
        return 0;       
}
