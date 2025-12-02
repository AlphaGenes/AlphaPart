#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List AlphaPartDrop(double c1, double c2, int nI, int nP, int nT, int nGP,
                   NumericMatrix ped, IntegerVector P, IntegerVector Px) {
  // --- Temp ---

  int i, j, t, p, pt=0, mt=0, k;

  // --- Outputs ---

  NumericMatrix pa(nI+1, nT*nGP);    // Parent average
  NumericMatrix ms(nI+1, nT*nGP);    // Mendelian sampling
  NumericMatrix xa(nI+1, nP*nT*nGP); // Partitions
  // NOTE: Rcpp::NumericMatrix is filled by 0s by default

  // TODO: Maybe we want an algorithm that works on one trait at a time to save on memory?
  //       https://github.com/AlphaGenes/AlphaPart/issues/15

  // TODO: Pass pedigree by reference to improve memory use #13
  //       https://github.com/AlphaGenes/AlphaPart/issues/13

  // --- Compute ---
  for(i = 1; i < nI+1; i++) {
    for(t = 0; t < nT; t++) {
      // Parent average (PA)
      pa(i, t) = c1 * ped(ped(i, 1), 3+t) +
                 c2 * ped(ped(i, 2), 3+t);

      // Mendelian sampling (MS)
      ms(i, t) = ped(i, 3+t) - pa(i, t);
      
      // Gametic partition
      if (nGP ==3) {
        pt = t + nT; // paternal trait index
        mt = t + 2*nT; // maternal trait index
        pa(i, pt) = c1 * ped(ped(i,1), 3+pt) + c1 * ped(ped(i,1), 3+mt);
        pa(i, mt) = c2 * ped(ped(i,2), 3+pt) + c2 * ped(ped(i,2), 3+mt);
        ms(i, mt) = ped(i, 3+mt) - pa(i, mt);
        ms(i, pt) = ped(i, 3+pt) - pa(i, pt);
      }

      // Parts

      // ... for the MS part
      j = nGP*Px[t] + P[i];
      xa(i, j) = ms(i, t);
      
      if (nGP == 3) {
        j = nGP*Px[t] + P[i] + nP;
        xa(i, j) = ms(i, pt);
        j = nGP*Px[t] + P[i] + nP*2;
        xa(i, j) = ms(i, mt);
      }

      // ... for the PA parts
      for(p = 0; p < nP; p++) {
        j = nGP*Px[t] + p;
        xa(i, j) += c1 * xa(ped(i, 1), j) +
                    c2 * xa(ped(i, 2), j);
        if (nGP == 3) {
          j = nGP*Px[t] + p + nP;
          k = nGP*Px[t] + p + nP*2;
          xa(i, j) += c1 * xa(ped(i, 1), j) + c1 * xa(ped(i, 1), k);
          xa(i, k) += c2 * xa(ped(i, 2), j) + c2 * xa(ped(i, 2), k);
        }
      }
    }
  }

  // --- Return ---

  return List::create(Named("pa", pa),
                      Named("ms", ms),
                      Named("xa", xa));
}
