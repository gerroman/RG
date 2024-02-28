#include "wstp.h"
#include <ginac/ginac.h>
#include <cassert>

void EvalG(double* reZs, long nReZs, double* imZs, long nImZs, double yValue)
{
  double gvalue[2];
  const int k = 2;
  gvalue[0] = 0.;
  gvalue[1] = 0.;

  GiNaC::lst zs;

  assert(nReZs == nImZs && nReZs >= 0);

  for (int i = 0; i < nReZs; ++i) {
    GiNaC::ex z = (*reZs) + GiNaC::I * (*imZs);
    zs.append(z);
    reZs++;
    imZs++;
  }
  GiNaC::numeric y(yValue);

  GiNaC::ex GValue = GiNaC::G(zs, y);
  assert(GiNaC::is_a<GiNaC::numeric>(GValue));

  gvalue[0] = GiNaC::ex_to<GiNaC::numeric>(GiNaC::real_part(GValue)).to_double();
  gvalue[1] = GiNaC::ex_to<GiNaC::numeric>(GiNaC::imag_part(GValue)).to_double();

  assert(WSPutReal64List(stdlink, gvalue, k) > 0);
}

int main(int argc, char* argv[])
{
	return WSMain(argc, argv);
}
