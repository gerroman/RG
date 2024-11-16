#include "wstp.h"
#include <ginac/ginac.h>
#include <cassert>
#include <limits>


void EvalG(double* reZs, long nReZs, double* imZs, long nImZs, double yValue)
{
  double gvalue[2];
  gvalue[0] = std::numeric_limits<double>::quiet_NaN();
  gvalue[1] = std::numeric_limits<double>::quiet_NaN();

  assert(nReZs == nImZs);
  if (nReZs != nImZs) {
    WSPutReal64List(stdlink, gvalue, 2);
    return;
  }

  GiNaC::lst zs;
  for (int i = 0; i < nReZs; ++i) {
    GiNaC::ex z = (*reZs) + GiNaC::I * (*imZs);
    zs.append(z);
    reZs++;
    imZs++;
  }
  GiNaC::numeric y(yValue);

  GiNaC::ex GValue = GiNaC::G(zs, y);

  assert(GiNaC::is_a<GiNaC::numeric>(GValue));
  if (!GiNaC::is_a<GiNaC::numeric>(GValue)) {
    WSPutReal64List(stdlink, gvalue, 2);
    return;
  }

  gvalue[0] = GiNaC::ex_to<GiNaC::numeric>(GiNaC::real_part(GValue)).to_double();
  gvalue[1] = GiNaC::ex_to<GiNaC::numeric>(GiNaC::imag_part(GValue)).to_double();
  WSPutReal64List(stdlink, gvalue, 2);
}


int main(int argc, char* argv[])
{
  return WSMain(argc, argv);
}
