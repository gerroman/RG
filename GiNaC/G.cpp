#include "wstp.h"
#include <ginac/ginac.h>

#ifdef DEBUG
#include <iostream>
#endif // DEBUG

void EvalG(double* reZs, long nReZs, double* imZs, long nImZs, double yValue)
{
  double gvalue[2];
  const int k = 2;
  gvalue[0] = 0.;
  gvalue[1] = 0.;

  GiNaC::lst zs;

#ifdef DEBUG
  if (nReZs != nImZs) {
    std::cerr << "[error]: unequal re and im lists" << std::endl;
  }
#endif // DEBUG
  

  for (int i = 0; i < nReZs; ++i) {
    GiNaC::ex z = (*reZs) + GiNaC::I * (*imZs);
#ifdef DEBUG
    std::cerr << "[info]: z[" << i << "] = " << z << std::endl;
#endif // DEBUG
    zs.append(z);
    reZs++;
    imZs++;
  }

  GiNaC::numeric y(yValue);
#ifdef DEBUG
  std::cerr << "[info]: y = " << y << std::endl;
#endif // DEBUG

  GiNaC::ex GValue = GiNaC::G(zs, y);
#ifdef DEBUG
  std::cerr << "[info]: G(zs, y) = " << GValue << std::endl;
#endif // DEBUG

#ifdef DEBUG
  if (GiNaC::is_a<GiNaC::numeric>(GValue)) {
#endif // DEBUG
    gvalue[0] = GiNaC::ex_to<GiNaC::numeric>(GiNaC::real_part(GValue)).to_double();
    gvalue[1] = GiNaC::ex_to<GiNaC::numeric>(GiNaC::imag_part(GValue)).to_double();
#ifdef DEBUG
  } else {
    std::cerr << "[error]: G(zs, y) returned not a numeric result" << std::endl;
  }
#endif // DEBUG

  if (!WSPutReal64List(stdlink, gvalue, k)) {
#ifdef DEBUG
    std::cerr << "[error]: unable to send a result" << std::endl;
#endif // DEBUG
  };
}

int main(int argc, char* argv[])
{
	return WSMain(argc, argv);
}
