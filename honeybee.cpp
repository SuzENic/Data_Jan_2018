#include <iostream>
#include <math.h>
#include <limits>
using namespace std;
int iteration = 50000000;
double denom =  50000000.0;// same as iteration
double dist[50000000];// same as iteration
double running_dist = 0.0;
int n,m, k, steps,i, j, thresh, target;
double a = 0.0;
double b = 0.0;
typedef std::numeric_limits< double > dbl;


int jump() //jumps to new position and maps that to cart coords. they start from A at 12 and go clockwise. 
{
  int r1 = rand()% 6;
  double step1 = .5;
  double step2 = cos(30.0/180.0*M_PI);
  if (r1 == 0)  //to step to position A  
    {
      //a = a;
      b = b+1;
    }
  if (r1 == 1)  //to step to position B  
    {
      a = a+step2;
      b = b+step1;
    }
  if (r1 == 2)  //to step to position C  
    {
      a = a+step2;
      b = b-step1;
    }
  if (r1 == 3)  //to step to position D  
    {
      //a = a;
      b = b-1;
    }
  if (r1 == 4)  //to step to position E  
    {
      a = a-step2;
      b = b-step1;
    }
  if (r1 == 5)  //to step to position F  
    {
      a = a-step2;
      b = b+step1;
    }
  
  return 0;
}


int main() 
{
  cout.precision(dbl::max_digits10);// forcing the print to full digits
  srand (time(NULL));
  int totalsteps;
  int modnum;
  double prob_o_thresh;
  double prob_o_target;
  for (j =0; j<2; ++j)// stepping over each case
    {
      double mean = 0.0;
      double variance = 0.0;
      double prob_sum = 0.0;
      double stdev = 0.0;
      if (j <1)
	{
	  totalsteps= 16;
	  thresh = 6;
	  target = 8;
	}
      else
	{
	  totalsteps= 64;
	  thresh = 20;
	  target = 24;
	}
      running_dist = 0.0;
      for (i = 0; i <iteration;++i) //stepping through the given number of steps
	{
	  a = 0.0;
          b = 0.0;
	  for ( steps = 0; steps < totalsteps; ++steps ) 
	    {
	      jump();
	      dist[i] = sqrt(a*a+b*b); //composing the sum
	    }
	  running_dist = running_dist + dist[i];// to find the mean
	}
      mean = running_dist/denom;// running sum used here
      prob_o_thresh = 0.0;
      prob_o_target = 0.0;
      for (i = 0; i <iteration;++i) 
	{
	  if (dist[i] >= thresh) {
	    prob_o_thresh = prob_o_thresh+1.0;
	    if (dist[i] >= target) {
	       prob_o_target = prob_o_target+1.0;
	    }
	  }
	}
      for (i = 0; i <iteration;++i) 
	{
	  variance = variance + (dist[i]-mean)*(dist[i]-mean);// to find the stdev
	  dist[i] = 0;
	}
      stdev = sqrt(variance/denom);// variance used here
      if (j == 0)// printing out results
        {
	  cout << "For 16 Steps:"<< endl;
	}
      else
	{
	  cout << "For 64 Steps:"<< endl;
	}
      cout << "Mean:        "<< mean<<"   Stdev: "<<stdev<<endl;
      cout <<"Prob over target:" << prob_o_thresh<<","<<prob_o_target<<","<< prob_o_target/prob_o_thresh<<endl;
    }
  return 0;
}
