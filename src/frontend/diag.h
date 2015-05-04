#ifndef DIAG_H
#define DIAG_H

#ifdef __cplusplus

#include <string>

// this stuff is confusing b/c we expose only the C API to C,
// but also have to support it from C++ (to implement it).
namespace glaz {
    class Diagnostics;
}
typedef struct CDiagnostics {
    glaz::Diagnostics *diag;
} CDiagnostics;

namespace glaz {

struct SourceLocation;
class Diagnostics {
    int maxErrors;
    int errorCount;

    CDiagnostics c_diagnostics;
    
public:
    // default to using stdout
    explicit Diagnostics(int maxErrors = 10)
        : maxErrors(maxErrors),
          errorCount(0) {
        c_diagnostics.diag = this;
    }
    
    bool shouldExit() const { return maxErrors > 0 && errorCount >= maxErrors; }
    bool hadError() const { return errorCount > 0; }
        
    // returns whether we should continue (have max errors been reached?)
    bool error(const std::string &filename,
               const std::string &message,
               const std::string &sourceText,
               const SourceLocation &sourceLocation);
    
    void warning(const std::string &filename,
                 const std::string &message,
                 const std::string &sourceText,
                 const SourceLocation &sourceLocation);

    CDiagnostics *getCDiagnostics() { return &c_diagnostics; }

private:
    void emitMessage(const std::string &messageType,
                     const std::string &filename,
                     const std::string &message,
                     const std::string &sourceText,
                     const SourceLocation &sourceLocation);
};

} // namespace glaz

#define NS_GLAZ_CPP glaz::
extern "C" {
#else // __cplusplus
struct CDiagnostics;
struct SourceLocation;
#define NS_GLAZ_CPP
#endif // __cplusplus

// C API for scanner

int diagnostics_error(struct CDiagnostics *d,
                      const char *filename,
                      const char *message,
                      const char *sourceText,
                      const struct NS_GLAZ_CPP SourceLocation *sourceLocation);

int diagnostics_error_range(struct CDiagnostics *d,
                            const char *filename,
                            const char *message,
                            const char *sourceTextStart,
                            const char *sourceTextEnd,
                            const struct NS_GLAZ_CPP SourceLocation *sourceLocation);

void diagnostics_warning(struct CDiagnostics *d,
                         const char *filename,
                         const char *message,
                         const char *sourceText,
                         const struct NS_GLAZ_CPP SourceLocation *sourceLocation);

void diagnostics_warning_range(struct CDiagnostics *d,
                              const char *filename,
                              const char *message,
                              const char *sourceTextStart,
                              const char *sourceTextEnd,
                              const struct NS_GLAZ_CPP SourceLocation *sourceLocation);

int diagnostics_should_exit(struct CDiagnostics *d);

int diagnostics_had_error(struct CDiagnostics *d);

#ifdef __cplusplus
} // extern "C"
#endif

#endif        //  #ifndef DIAG_H

