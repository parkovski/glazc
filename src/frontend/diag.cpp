#include "diag.h"
#include "scanner.h"

#include <string>
#include <iostream>

using namespace glaz;

bool Diagnostics::error(const std::string &filename,
                        const std::string &message,
                        const std::string &sourceText,
                        const SourceLocation &sourceLocation)
{
    emitMessage("error", filename, message, sourceText, sourceLocation);
    ++errorCount;
    return shouldExit();
}

void Diagnostics::warning(const std::string &filename,
                          const std::string &message,
                          const std::string &sourceText,
                          const SourceLocation &sourceLocation)
{
    emitMessage("warning", filename, message, sourceText, sourceLocation);
}

void Diagnostics::emitMessage(const std::string &messageType,
                              const std::string &filename,
                              const std::string &message,
                              const std::string &sourceText,
                              const SourceLocation &sourceLocation)
{
    std::cout
        << filename << ":"
        << sourceLocation.first_line << ":" << sourceLocation.first_column << ": "
        << messageType << ": " << message
        << " (found `" << sourceText << "')" << std::endl;
}

extern "C" {
    // C API for the scanner
    int diagnostics_error(CDiagnostics *d,
        const char *filename,
        const char *message,
        const char *sourceText,
        const SourceLocation *sourceLocation) {

        return d->diag->error(filename, message, sourceText, *sourceLocation);
    }

    int diagnostics_error_range(CDiagnostics *d,
        const char *filename,
        const char *message,
        const char *sourceTextStart,
        const char *sourceTextEnd,
        const SourceLocation *sourceLocation) {

        return d->diag->error(filename, message, std::string(sourceTextStart, sourceTextEnd), *sourceLocation);
    }

    void diagnostics_warning(CDiagnostics *d,
        const char *filename,
        const char *message,
        const char *sourceText,
        const SourceLocation *sourceLocation) {

        d->diag->warning(filename, message, sourceText, *sourceLocation);
    }

    void diagnostics_warning_range(CDiagnostics *d,
        const char *filename,
        const char *message,
        const char *sourceTextStart,
        const char *sourceTextEnd,
        const SourceLocation *sourceLocation) {

        d->diag->warning(filename, message, std::string(sourceTextStart, sourceTextEnd), *sourceLocation);
    }

    int diagnostics_should_exit(CDiagnostics *d) {
        return d->diag->shouldExit();
    }

    int diagnostics_had_error(CDiagnostics *d) {
        return d->diag->hadError();
    }
}