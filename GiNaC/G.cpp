#include "wstp.h"
#include <ginac/ginac.h>
#include <iostream>
#include <cassert>

void EvalG(double* reZs, long nReZs, double* imZs, long nImZs, double yValue)
{
  double gvalue[2];
  const int k = 2;
  gvalue[0] = 0.;
  gvalue[1] = 0.;

  GiNaC::lst zs;

  assert(nReZs == nImZs);

  for (int i = 0; i < nReZs; ++i) {
    GiNaC::ex z = (*reZs) + GiNaC::I * (*imZs);
    std::cerr << "[info]: z[" << i << "] = " << z << std::endl;
    zs.append(z);
    reZs++;
    imZs++;
  }

  GiNaC::numeric y(yValue);
  std::cerr << "[info]: y = " << y << std::endl;

  GiNaC::ex GValue = GiNaC::G(zs, y);
  std::cerr << "[info]: G(zs, y) = " << GValue << std::endl;

  if (GiNaC::is_a<GiNaC::numeric>(GValue)) {
    gvalue[0] = GiNaC::ex_to<GiNaC::numeric>(GiNaC::real_part(GValue)).to_double();
    gvalue[1] = GiNaC::ex_to<GiNaC::numeric>(GiNaC::imag_part(GValue)).to_double();
  } else {
    std::cerr << "[error]: G(zs, y) returned not a numeric result" << std::endl;
  }

  assert(WSPutReal64List(stdlink, gvalue, k) > 0);
}

int main(int argc, char* argv[])
{
	return WSMain(argc, argv);
}
