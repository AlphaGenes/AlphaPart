#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP AlphaPartDrop(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP AlphaPartDropGroup(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"AlphaPartDrop",      (DL_FUNC) &AlphaPartDrop,       9},
    {"AlphaPartDropGroup", (DL_FUNC) &AlphaPartDropGroup, 10},
    {NULL, NULL, 0}
};

void R_init_AlphaPart(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
