#include "wstp.h"
#include <ginac/ginac.h>
#include <iostream>

double G(double zValue, double yValue)
{
  GiNaC::ex GValue = GiNaC::G(GiNaC::lst{zValue}, yValue);
  if (!(GiNaC::is_a<GiNaC::numeric>(GValue))) {
    std::cerr << "[error]: call G(" << zValue << ", " << yValue << ") returned not a numeric result" << std::endl;
    return 0.0;
  }
  return GiNaC::ex_to<GiNaC::numeric>(GValue).to_double();
}

int main(int argc, char* argv[])
{
	return WSMain(argc, argv);
}
