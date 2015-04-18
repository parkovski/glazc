#ifndef GLAZ_H
#define GLAZ_H

#include <assert.h>
#include <stdio.h>

#ifdef _MSC_VER
# define EXPORTFUNC __declspec(dllexport)
#elif defined(__GNUC__)
# define EXPORTFUNC __attribute__((visibility("default")))
#endif


#endif        //  #ifndef GLAZ_H

