#ifndef INCLUDE_DEBUG_LOGGER_H
#define INCLUDE_DEBUG_LOGGER_H

#include <iostream>
#include <ostream>
#include <map>
#include <string>
#include <string.h>
#include <stdarg.h>
#include <sstream>
#include <math.h>
#include <cmath>

#include "Timer.h"

#if defined(WIN32) | defined(__WIN32) || defined (_WIN32)
#define SPRINTF(buffer, format, value) sprintf_s(buffer, 128, format, value)
#elif defined(__unix__) || defined(__APPLE__)
#define SPRINTF(buffer, format, value) sprintf(buffer, format, value)
#endif

constexpr int OUTPUTFORMAT_DECIMAL = 0;
constexpr int OUTPUTFORMAT_HEX = 1;
constexpr int OUTPUTFORMAT_UPPERHEX = 2;
constexpr int OUTPUTFORMAT_BIN = 3;
constexpr int CAPITALIZEDFORMAT_NONE = 0;
constexpr int CAPITALIZEDFORMAT_CAPS = 1;
constexpr int CAPITALIZEDFORMAT_LOWER = 2;

/**
 * Levels for debugging
 * NONE: nothing is output
 * TRACE: basic trace information
 * WARNING: small non-critical errors (code can continue to run without problems)
 * ERROR: large potentially problematic issues (code can probably run, but might crash)
 * CRITICALERROR: errors that break the program. It should close
 * */
enum class Level {
    NONE,
    LEVEL_TRACE,
    LEVEL_WARNING,
    LEVEL_ERROR,
    CRITICAL_ERROR,
    LEVEL_COUNT
};

/**
 * all the valid types for debug variables
 * INTEGER32: a 32 bit integer
 * INTEGER64: a 64 bit integer
 * FLOAT32: a 32 bit floating point value
 * FLOAT64: a 64 bit floating point value
 * STRING: an instance of std::string
 * @author Bryce Young 5/19/2021
 * */
enum class DebugVarType {
    CHAR,
    INTEGER32,
    INTEGER64,
    FLOAT32,
    FLOAT64,
    STRING,
    DEBUGVAR_TYPE_COUNT
};

/**
 * Class to interface with the logger
 * CFG doc:
 * 
 * Capitalize any output by prefixing with ^
 * 
 * @author Bryce Young 5/20/2021
 * */
class DebugLogger {
    public:
        DebugLogger(const std::string& loggerName = "Debug", Level level = Level::LEVEL_TRACE) 
            :level(level),
            targetStream(&std::cout)
        {
            this->loggerName = loggerName;
            this->level = level;
            totalNanoseconds = 0;

            for(int i = 0; i <= (int)Level::LEVEL_COUNT; ++i) {
                messageCount[i] = 0;
            }

            levelNames[(int)Level::LEVEL_TRACE] = "TCE";
            levelNames[(int)Level::LEVEL_WARNING] = "WNG";
            levelNames[(int)Level::LEVEL_ERROR] = "ERR";
            levelNames[(int)Level::CRITICAL_ERROR] = "CRT";
            levelNames[(int)Level::LEVEL_COUNT] = levelNames[(int)Level::LEVEL_TRACE];

            //add default variables
            //th = time hours
            //tm = time minutes
            //ts = time seconds
            //tl = time milliseconds
            //ti = time microseconds
            addInternalVariable("th", timeVars + 0, DebugVarType::FLOAT64);
            addInternalVariable("tm", timeVars + 1, DebugVarType::FLOAT64);
            addInternalVariable("ts", timeVars + 2, DebugVarType::FLOAT64);
            addInternalVariable("tl", timeVars + 3, DebugVarType::FLOAT64);
            addInternalVariable("ti", timeVars + 4, DebugVarType::FLOAT64);

            //eth = elapsed time hours
            //etm = elapsed time minutes
            //ets = elapsed time seconds
            //etl = elapsed time milliseconds
            //eti = elapsed time microseconds
            addInternalVariable("eth", elapsedTimeVars + 0, DebugVarType::FLOAT64);
            addInternalVariable("etm", elapsedTimeVars + 1, DebugVarType::FLOAT64);
            addInternalVariable("ets", elapsedTimeVars + 2, DebugVarType::FLOAT64);
            addInternalVariable("etl", elapsedTimeVars + 3, DebugVarType::FLOAT64);
            addInternalVariable("eti", elapsedTimeVars + 4, DebugVarType::FLOAT64);

            //the name of the logger program
            addInternalVariable("pn", &this->loggerName, DebugVarType::STRING);

            //the name of the levels
            //tn = trace name
            //wn = warning name
            //en = error name
            //cn = critical name
            //ln = current level name
            addInternalVariable("tn", &this->levelNames[(int)Level::LEVEL_TRACE], DebugVarType::STRING);
            addInternalVariable("wn", &this->levelNames[(int)Level::LEVEL_WARNING], DebugVarType::STRING);
            addInternalVariable("en", &this->levelNames[(int)Level::LEVEL_ERROR], DebugVarType::STRING);
            addInternalVariable("cn", &this->levelNames[(int)Level::CRITICAL_ERROR], DebugVarType::STRING);
            addInternalVariable("ln", &this->levelNames[(int)Level::LEVEL_COUNT], DebugVarType::STRING);

            //variables for message count
            //dmc stands for debug message count
            addInternalVariable("dmc", &messageCount[(int)Level::LEVEL_COUNT], DebugVarType::INTEGER64);

            //tmc stands for trace message count
            //wmc warning message count
            //emc error message count
            //cmc critical messageCount
            addInternalVariable("tmc", &messageCount[(int)Level::LEVEL_TRACE], DebugVarType::INTEGER64);
            addInternalVariable("wmc", &messageCount[(int)Level::LEVEL_WARNING], DebugVarType::INTEGER64);
            addInternalVariable("emc", &messageCount[(int)Level::LEVEL_ERROR], DebugVarType::INTEGER64);
            addInternalVariable("cmc", &messageCount[(int)Level::CRITICAL_ERROR], DebugVarType::INTEGER64);

            //level message count
            addInternalVariable("lmc", &currentMessageCount, DebugVarType::INTEGER64);

            //helpful characters
            addInternalVariable("lbc", &specialCharacters[0], DebugVarType::CHAR);
            addInternalVariable("rbc", &specialCharacters[1], DebugVarType::CHAR);
            addInternalVariable("lbk", &specialCharacters[2], DebugVarType::CHAR);
            addInternalVariable("rbk", &specialCharacters[3], DebugVarType::CHAR);
            addInternalVariable("bks", &specialCharacters[4], DebugVarType::CHAR);

            //char pnemonics
            reserves["char"] = Token::TokenType::SIGNED_CHAR;
            reserves["ch"] = Token::TokenType::SIGNED_CHAR;
            reserves["c"] = Token::TokenType::SIGNED_CHAR;

            //int pnemonics
            reserves["int"] = Token::TokenType::SIGNED_INT;
            reserves["i"] = Token::TokenType::SIGNED_INT;
            reserves["d"] = Token::TokenType::SIGNED_INT;
            reserves["uint"] = Token::TokenType::SIGNED_INT;
            reserves["ui"] = Token::TokenType::SIGNED_INT;
            reserves["u"] = Token::TokenType::SIGNED_INT;

            //long pneumonics
            reserves["long"] = Token::TokenType::SIGNED_LONG;
            reserves["llu"] = Token::TokenType::SIGNED_LONG;
            reserves["ulong"] = Token::TokenType::SIGNED_LONG;
            reserves["ul"] = Token::TokenType::SIGNED_LONG;

            //float pnemoinics
            reserves["float"] = Token::TokenType::FLOAT;
            reserves["flt"] = Token::TokenType::FLOAT;
            reserves["f"] = Token::TokenType::FLOAT;

            //str pneumonics
            reserves["string"] = Token::TokenType::STRING;
            reserves["str"] = Token::TokenType::STRING;
            reserves["s"] = Token::TokenType::STRING;

            setPrefix("[3ln]~[.2etl] \\[[>05lmc]\\]: ");
            timer.reset();
        }

        ~DebugLogger() {
        }

        void setTargetOutput(std::ostream* outputStream) {
            this->targetStream = outputStream;
        }

        /**
         * Returns the level of the debugger
         * */
        Level getLevel() const {
            return level;
        }

        /**
         * Updates level
         * @param newLevel: the new level
         * */
        void setLevel(Level newLevel) {
            this->level = newLevel;
        }

        void setColorTrace(std::ostream& outputStream) {
            if(enableColor) {
                previousColor = "\033[1m\033[32m"; //dark green
                //outputStream << "\033[34m"; //blue
                //outputStream << "\033[32m"; //green
                //outputStream << "\033[1m\033[34m"; //dark blue
                outputStream << previousColor;
            }
        }

        void setColorWarning(std::ostream& outputStream) {
            if(enableColor) {
                //outputStream << "\033[33m"; //yellow
                previousColor = "\033[1m\033[33m"; //dark yellow
                outputStream << previousColor;
            }
        }

        void setColorError(std::ostream& outputStream) {
            if(enableColor) {
                //outputStream << "\033[31m"; //red
                previousColor = "\033[1m\033[31m"; //dark red
                outputStream << previousColor;
            }
        }

        void setColorCritical(std::ostream& outputStream) {
            if(enableColor) {
                previousColor = "\033[1m\033[31m"; //dark red
                outputStream << previousColor;
            }
        }

        void resetColor(std::ostream& outputStream) {
			if (enableColor) {
				previousColor = "\033[0m";
				outputStream << previousColor;
            }
        }

        void setColorEnabled() {
            this->enableColor = true;
        }

        void setColorDisabled() {
            this->enableColor = false;
        }

        bool getColorEnabled() {
            return this->enableColor;
        }
        
        /**
         * There exist various variables which the prefix uses
         * To use a variable, use two braces and write the name of the variable inside; constrast from the {} used for printing parameters passed into the function
         * [ var_name ] -> with or without the spaces, it doesn't matter
         * Basic formatting options within the braces apply
         * Applies the prefix to all levels if @param targetLevel is omitted
         * */
        void setPrefix(const std::string& prefix, Level targetLevel = Level::LEVEL_COUNT) {
            if(targetLevel == Level::LEVEL_COUNT) {
                this->prefixFormat[(int)Level::LEVEL_TRACE] = prefix;
                this->prefixFormat[(int)Level::LEVEL_WARNING] = prefix;
                this->prefixFormat[(int)Level::LEVEL_ERROR] = prefix;
                this->prefixFormat[(int)Level::CRITICAL_ERROR] = prefix;
            }
            else if(targetLevel < Level::LEVEL_COUNT && targetLevel >= Level::LEVEL_TRACE){
                this->prefixFormat[(int)targetLevel] = prefix;
            }
        }

        int trace(const char* format, ...) {
            int ret = 0;
            va_list args;
            va_start(args, format);

            //set trace vars
            if(updateLogger(Level::LEVEL_TRACE)) {
                setTrace(*this->targetStream);
                ret = logInternal(*this->targetStream, format, args);
            }

            resetColor(*this->targetStream);

            va_end(args);
            return ret;
        }

        int traceToStream(std::ostream& output, const char* format, ...) {
            int ret = 0;
            va_list args;
            va_start(args, format);

            //set trace vars
            if(updateLogger(Level::LEVEL_TRACE)) {
                setTrace(output);
                ret = logInternal(output, format, args);
            }

            resetColor(output);

            va_end(args);
            return ret;
        }

        int warning(const char* format, ...) {
            int ret = 0;
            va_list args;
            va_start(args, format);

            if(updateLogger(Level::LEVEL_WARNING)) {
                setWarning(*this->targetStream);
                ret = logInternal(*this->targetStream, format, args);
            }

            resetColor(*this->targetStream);

            va_end(args);
            return ret;
        }

        int warningToStream(std::ostream& output, const char* format, ...) {
            int ret = 0;
            va_list args;
            va_start(args, format);

            if(updateLogger(Level::LEVEL_WARNING)) {
                setWarning(output);
                ret = logInternal(output, format, args);
            }

            resetColor(output);

            va_end(args);
            return ret;
        }

        int error(const char* format, ...) {
            int ret = 0;
            va_list args;
            va_start(args, format);

            if(updateLogger(Level::LEVEL_ERROR)) {
                setError(*this->targetStream);
                ret = logInternal(*this->targetStream, format, args);
            }

            resetColor(*this->targetStream);

            va_end(args);
            return ret;
        }

        int errorToStream(std::ostream& output, const char* format, ...) {
            int ret = 0;
            va_list args;
            va_start(args, format);

            if(updateLogger(Level::LEVEL_ERROR)) {
                setError(output);
                ret = logInternal(output, format, args);
            }

            resetColor(output);

            va_end(args);
            return ret;
        }

        int critical(const char* format, ...) {
            int ret = 0;
            va_list(args);
            va_start(args, format);

            if(updateLogger(Level::CRITICAL_ERROR)) {
                setCritical(*this->targetStream);
                ret = logInternal(*this->targetStream, format, args);
            }

            resetColor(*this->targetStream);

            va_end(args);
            return ret;
        }

        int criticalToStream(std::ostream& output, const char* format, ...) {
            int ret = 0;
            va_list(args);
            va_start(args, format);

            if(updateLogger(Level::CRITICAL_ERROR)) {
                setCritical(output);
                ret = logInternal(output, format, args);
            }

            resetColor(output);

            va_end(args);
            return ret;
        }

        /**
         * Updates times and message counts
         * */
        inline bool updateLogger(Level lev) {
            if(this->level <= lev) {
                messageCount[(int)lev]++;
                messageCount[(int)Level::LEVEL_COUNT]++;

                //update timers (hrs, mins, seconds, millis, microseconds)
                uint64_t elapsedNanos = timer.nanoseconds();
                totalNanoseconds += elapsedNanos;

                timeVars[0] = (double)totalNanoseconds / 3.6e12;
                timeVars[1] = (double)totalNanoseconds / 6e10;
                timeVars[2] = (double)totalNanoseconds / 1e9;
                timeVars[3] = (double)totalNanoseconds / 1e6;
                timeVars[4] = (double)totalNanoseconds / 1000;

                elapsedTimeVars[0] = (double)elapsedNanos / 3.6e12;
                elapsedTimeVars[1] = (double)elapsedNanos / 6e10;
                elapsedTimeVars[2] = (double)elapsedNanos / 1e9;
                elapsedTimeVars[3] = (double)elapsedNanos / 1e6;
                elapsedTimeVars[4] = (double)elapsedNanos / 1000;

                timer.reset();
                return true;
            }

            return false;
        }

        inline void setTrace(std::ostream& output) {
            //set trace vars
            setColorTrace(output);
            levelNames[(int)Level::LEVEL_COUNT] = levelNames[(int)Level::LEVEL_TRACE];
            currentMessageCount = messageCount[(int)Level::LEVEL_TRACE];
        }

        inline void setWarning(std::ostream& output) {
            setColorWarning(output);
            levelNames[(int)Level::LEVEL_COUNT] = levelNames[(int)Level::LEVEL_WARNING];
            currentMessageCount = messageCount[(int)Level::LEVEL_WARNING];
        }

        inline void setError(std::ostream& output) {
            setColorError(output);
            levelNames[(int)Level::LEVEL_COUNT] = levelNames[(int)Level::LEVEL_ERROR];
            currentMessageCount = messageCount[(int)Level::LEVEL_ERROR];
        }

        inline void setCritical(std::ostream& output) {
            setColorCritical(output);
            levelNames[(int)Level::LEVEL_COUNT] = levelNames[(int)Level::CRITICAL_ERROR];
            currentMessageCount = messageCount[(int)Level::CRITICAL_ERROR];
        }

        /**
         * adds a variable to the debugger
         * @param name the name to which the variable will be referred
         * @param variable the variable value as a void*
         * @param type the variable type, one of the section from the enum
         * @return true if the variable was added successfully, false if the variable is conflicting with other variables
         * */
        bool addVariable(const std::string& name, void* variable, DebugVarType type) {
            //validate variable
            const char* varName = name.c_str();
            int index = 0;
            if(isAlpha(varName, index) || varName[0] == '_') {
                index++;

                while(varName[index] && (isAlpha(varName, index) || varName[index] == '_' || isNum(varName, index))) {
                    index++;
                }
                
                if(index == name.size()) {
                    if(variables.find(name) == variables.end()) {
                        variables.emplace(std::pair<std::string, DebugVar>({name, DebugVar(type, variable)}));
                        return true;
                    }
                }
            }

            //improper var name or something
            return false;
        } 

        /**
         * Removes a variable from the list
         * @param name the name of the variable to remove
         * @return true if the variable existed and is now removed, false if the variable never existed
         * */
        bool removeVariable(const std::string& name) {
            std::map<std::string, DebugVar>::iterator v = variables.find(name);

            if(v != variables.end() && !v->second.getReadonly()) {
                variables.erase(v);
                return true;
            }

            return false;
        }

    private:
        /**
         * Adds a variable that cannot be removed
         * */
        bool addInternalVariable(const std::string& name, void* variable, DebugVarType type) {
            if(variables.find(name) == variables.end()) {
                variables.emplace(std::pair<std::string, DebugVar>({name, DebugVar(type, variable, true)}));
                return true;
            }

            return false;
        }

        /**
         * Internal method to handle logging
         * @param output the output stream to write to
         * @param format the print format
         * @param args the va arguments as a reference
         * @param level the current log level
         * */
        inline int logInternal(std::ostream& output, const char* format, va_list& args, bool recursive = false) {
            std::stringstream outputLine;

            //print prefix to message using only internal variables
            if(!recursive) {
                printPrefix(outputLine, level, args);
            }

            //set color (trace color)
            int len = (int)strlen(format);
            int formatIndex = 0;
            int previousFormatIndex = -1;

            //process and print arguments
            while(printNext(outputLine, format, formatIndex, args) && formatIndex < len && formatIndex != previousFormatIndex) {
                previousFormatIndex = formatIndex;
            }

            if(!recursive) {
                outputLine << "\n";
            }

            output << outputLine.str();
            return (int)outputLine.str().size();
        }

        /**
         * contains information representing a token
         * */
        struct Token {
            enum class TokenType {
                CAPITAL,
                LOWER,
                RIGHT,
                FORMATTED_STRING,
                FILL_ZERO,
                ZERO_DECIMALS,
                VARIABLE_NAME,
                FLOAT,
                UNSIGNED_MARK,
                SIGNED_CHAR,
                SIGNED_SHORT,
                SIGNED_INT,
                SIGNED_LONG,
                STRING,
                NUMBER,
                DECIMAL,
                HEX_MODIFIER,
                CAPITAL_HEX_MODIFIER,
                BINARY_MODIFIER
            };

            const char* lexemeStart, *lexemeEnd;
            TokenType type;
        };

        Token currentToken;
        
        /**
         * Loads the next identifier
         * */
        bool getIdentifier(const char* format, int& index) {
            if(isAlpha(format, index) || format[index] == '_') {
            }
            else {
                return false;
            }

            currentToken.type = Token::TokenType::VARIABLE_NAME;
            currentToken.lexemeStart = format + index;

            while(format[index] && (isAlpha(format, index) || format[index] == '_' || isNum(format, index))) {
                index++;
            }

            currentToken.lexemeEnd = format + index;
            return true;
        }

        /**
         * Returns if the character can be part of an identifier or not
         * */
        bool isPartOfIdentifier(char c) {
            return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || (c >= '0' && c <= '9');
        }

        /**
         * Returns the next one or two character token
         * */
        bool getNextToken(const char* format, int& index, char end) {
            skipWhitespace(format, index);

            if(format[index] == '^') {
                currentToken.type = Token::TokenType::CAPITAL;
                currentToken.lexemeStart = format + index;
                currentToken.lexemeEnd = format + index + 1;
                index++;
                return true;
            }
            if(format[index] == '0') {
                currentToken.type = Token::TokenType::FILL_ZERO;
                currentToken.lexemeStart = format + index;
                currentToken.lexemeEnd = format + index + 1;
                index++;
                return true;
            }
            if(format[index] == '$') {
                currentToken.type = Token::TokenType::LOWER;
                currentToken.lexemeStart = format + index;
                currentToken.lexemeEnd = format + index + 1;
                index++;
                return true;
            }
            if(format[index] == '>') {
                currentToken.type = Token::TokenType::RIGHT;
                currentToken.lexemeStart = format + index;
                currentToken.lexemeEnd = format + index + 1;
                index++;
                return true;
            }
            else if(format[index] == '+') {
                currentToken.type = Token::TokenType::UNSIGNED_MARK;
                currentToken.lexemeStart = format + index;
                currentToken.lexemeEnd = format + index + 1;
                index++;
                return true;
            }
            else if(format[index] == '.') {
                currentToken.type = Token::TokenType::DECIMAL;
                currentToken.lexemeStart = format + index;
                currentToken.lexemeEnd = format + index + 1;
                index++;

                //if its .0, then it counts as a single token
                if(format[index] == '0') {
                    currentToken.type = Token::TokenType::ZERO_DECIMALS;
                    currentToken.lexemeEnd++;
                    index++;
                }

                return true;
            }
            else if(format[index] == 'x' && !isPartOfIdentifier(format[index + 1])) {
                currentToken.type = Token::TokenType::HEX_MODIFIER;
                currentToken.lexemeStart = format + index;
                currentToken.lexemeEnd = format + index + 1;
                index++;
                return true;
            }
            else if(format[index] == 'X' && !isPartOfIdentifier(format[index + 1])) {
                currentToken.type = Token::TokenType::CAPITAL_HEX_MODIFIER;
                currentToken.lexemeStart = format + index;
                currentToken.lexemeEnd = format + index + 1;
                index++;
                return true;
            }
            else if(format[index] == 'b' && !isPartOfIdentifier(format[index + 1])) {
                currentToken.type = Token::TokenType::BINARY_MODIFIER;
                currentToken.lexemeStart = format + index;
                currentToken.lexemeEnd = format + index + 1;
                index++;
                return true;
            }
            else if(isNum(format, index)) {
                getNumber(format, index);
                return true;
            }
            else if(format[index] == '\'') {
                char start = (end == ']')? '[' : '{';
                //increment index because the start of the string doesn't include the quote
                index++;
                currentToken.type = Token::TokenType::FORMATTED_STRING;
                currentToken.lexemeStart = format + index;
                currentToken.lexemeEnd = currentToken.lexemeStart;

                //load the string until the closing brace is found
                int depth = 1;

                while(depth > 0 && format[index]) {
                    if(format[index] == '\\') {
                        if(format[index + 1]) {
                            index++;
                        }
                    }
                    else if(format[index] == end) {
                        depth--;
                    }
                    else if(format[index] == start) {
                        depth++;
                    }

                    index++;
                }

                //we have to move to the previous close because the program is looking for a close brace
                index--;
                currentToken.lexemeEnd = format + index;

                return true;
            }
            else if(getIdentifier(format, index)) {
                //check to see if the identifier was actually a reserve
                std::string v(currentToken.lexemeStart, currentToken.lexemeEnd);

                return true;
            }
            else {
                //error of some kind
                return false;
            }
        }

        /**
         * Returns the next number
         * */
        void getNumber(const char* format, int& index) {
            currentToken.type = Token::TokenType::NUMBER;
            currentToken.lexemeStart = format + index;

            while(isNum(format, index)) {
                index++;
            }

            currentToken.lexemeEnd = format + index;
        }

        /**
         * Enumerates all formatting options supplied by the user
         * */
        void collectFormattingOptions(const char* format, int& index, int& capitalized, bool& rightAligned, bool& unsignedValue, std::string& v, int& spaceCount, int& spaceCount_dec, bool& fillZero, int& outputFormat, std::string& formattedString, char end) { 
            bool foundDecimal = false;

            //implement variable grammar here
            index++;
            while(format[index] != end && format[index]) {
                if(!getNextToken(format, index, end)) {
                    break;
                }

                if(currentToken.type == Token::TokenType::CAPITAL) {
                    capitalized = CAPITALIZEDFORMAT_CAPS;
                }
                else if(currentToken.type == Token::TokenType::LOWER) {
                    capitalized = CAPITALIZEDFORMAT_LOWER;
                }
                else if(currentToken.type == Token::TokenType::RIGHT) {
                    rightAligned = true;
                }
                else if(currentToken.type == Token::TokenType::FILL_ZERO) {
                    fillZero = true;
                }
                else if(currentToken.type == Token::TokenType::NUMBER) {
                    int value = 0;
                    std::string lexeme(currentToken.lexemeStart, currentToken.lexemeEnd);
                    value = std::stoi(lexeme);

                    if(foundDecimal) {
                        foundDecimal = false;
                        spaceCount_dec = value;
                    }
                    else {
                        spaceCount = value;
                    }
                }
                else if(currentToken.type == Token::TokenType::ZERO_DECIMALS) {
                    spaceCount_dec = 0;
                }
                else if(currentToken.type == Token::TokenType::DECIMAL) {
                    foundDecimal = true;
                }
                else if(currentToken.type == Token::TokenType::UNSIGNED_MARK) {
                    unsignedValue = true;
                }
                else if(currentToken.type == Token::TokenType::HEX_MODIFIER) {
                    outputFormat = OUTPUTFORMAT_HEX;
                }
                else if(currentToken.type == Token::TokenType::CAPITAL_HEX_MODIFIER) {
                    outputFormat = OUTPUTFORMAT_UPPERHEX;
                }
                else if(currentToken.type == Token::TokenType::BINARY_MODIFIER) {
                    outputFormat = OUTPUTFORMAT_BIN;
                }
                else if(currentToken.type == Token::TokenType::FORMATTED_STRING) {
                    formattedString = std::string(currentToken.lexemeStart, currentToken.lexemeEnd);
                    if(formattedString.size() == 0) {
                        formattedString = " ";
                    }
                }
                else {
                    v = std::string(currentToken.lexemeStart, currentToken.lexemeEnd);
                }

                skipWhitespace(format, index);
            }
        }

        /**
         * Prints the variable
         * */
        void printVariable(std::ostream& output, const char* format, int& index, va_list& args) {
            //0 for no change, 1 for upper, 2 for lower
            int capitalized = CAPITALIZEDFORMAT_NONE;
            bool rightAligned = false;
            bool unsignedValue = false;
            std::string variableName;
            std::string formattedString;
            int setSpaceCount = -1;
            int setSpaceCount_dec = -1;
            bool fillZero = false;
            int outputFormat = OUTPUTFORMAT_DECIMAL;

            collectFormattingOptions(format, index, capitalized, rightAligned, unsignedValue, variableName, setSpaceCount, setSpaceCount_dec, fillZero, outputFormat, formattedString, ']');

            //now that we have reached the end, we can go ahead and print the variable
            //lets check if it exists first
            std::map<std::string, DebugVar>::iterator var = variables.find(variableName);

            //if the formatted string specifier is set, it overrides the argument specifier
            if(formattedString.size() > 0) {
                std::stringstream nextOutput;

                logInternal(nextOutput, formattedString.c_str(), args, true);
                printFormattedString(output, nextOutput.str().c_str(), capitalized, rightAligned, setSpaceCount);
            }
            else if(var != variables.end()) {
                switch(var->second.getType()) {
                    case DebugVarType::CHAR:
                        {
                            char value = var->second.getChar();
                            printFormattedChar(output, value, capitalized, rightAligned, setSpaceCount);
                        }
                        break;
                    case DebugVarType::INTEGER32:
                        {
                            uint32_t value = var->second.getInt32();
                            printFormattedInteger(output, value, rightAligned, setSpaceCount, outputFormat, unsignedValue, fillZero, false);
                        }
                        break;
                    case DebugVarType::INTEGER64:
                        {
                            uint64_t value = var->second.getInt64();
                            printFormattedInteger(output, value, rightAligned, setSpaceCount, outputFormat, unsignedValue, fillZero, true);
                        }
                        break;
                    case DebugVarType::FLOAT32:
                        {
                            float value = var->second.getFloat32();
                            printFormattedFloat(output, value, rightAligned, setSpaceCount, setSpaceCount_dec, fillZero);
                        }
                        break;
                    case DebugVarType::FLOAT64:
                        {
                            double value = var->second.getFloat64();
                            printFormattedFloat(output, value, rightAligned, setSpaceCount, setSpaceCount_dec, fillZero);
                        }
                        break;
                    case DebugVarType::STRING:
                        {
                            const char* value = var->second.getString();
                            printFormattedString(output, value, capitalized, rightAligned, setSpaceCount);
                        }
                        break;
                }
            }
        }

        /**
         * Reverses a string
         * @param buffer the buffer to reverse
         * @param len the length of the contents of the string
         * */
        void reverseString(char* buffer, int len) {
            //reverse the string in place
            char* begin = buffer, *end = buffer + len - 1;

            while(begin < end) {
                char tmp = *begin;
                *begin = *end;
                *end = tmp;
                begin++;
                end--;
            }
        }

        /**
         * Prints hex to a buffer
         * @param spaces indicates the min number of digits
         * @param fill the character to fill the empty spaces with
         * @return the length of the string
         * */
        int printHexToBuffer(char* buffer, uint64_t value, bool upperFlag) {
            char hexChars[] = "0123456789abcdef";
            char capHexChars[] = "0123456789ABCDEF";
            char* charset = upperFlag? capHexChars : hexChars;
            int index = 0;

            do{
                int digit = value % 16;
                buffer[index++] = charset[digit];
                value /= 16;
            } while(value);

            buffer[index] = 0;
            reverseString(buffer, index);
            return index;
        }

        int printBinToBuffer(char* buffer, uint64_t value) {
            char charset[3] = "01";
            int index = 0;

            do {
                int digit = value % 2;
                buffer[index++] = charset[digit];
                value /= 2;
            } while(value);
            
            buffer[index] = 0;
            reverseString(buffer, index);
            return index;
        }

        double powers10[10] = { 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 10000000000 };

        void printFormattedFloat(std::ostream& output, double value, bool right, int spaces, int decSpaces, bool fillZero) {
            decSpaces = (decSpaces == -1)? 10 : decSpaces;
            int tmpDecSpaces = decSpaces;
            decSpaces = std::max(0, decSpaces);
            decSpaces = std::min(10, decSpaces);

            if(decSpaces > 0) {
                double power = powers10[decSpaces - 1];

                //truncate the multiplication by the power
                value = (value * power);
                value = std::trunc((value + .5));
                value /= power;
            }
            else if(decSpaces == 0) {
                value = std::trunc((value + .5));
            }

            decSpaces = tmpDecSpaces;

            std::string toPrint(std::to_string(value));
            int decLoc = (int)toPrint.find('.');
            int len = 0;

            if(decLoc != -1) {
                len = decLoc + decSpaces + (decSpaces != 0);
            }
            else {
                //if the decimal point doesn't exist in the string, there are no decimal places (not relying on std::to_string to add trailing decimals)
                len = (int)toPrint.size();

                if(decSpaces != 0) {
                    len += decSpaces + 1;
                    toPrint += '.';
                }
            }

            //format it right if necessary
            if(right) {
                for(int i = 0; i < spaces - len; ++i) {
                    if(fillZero) {
                        output << '0';
                    }
                    else {
                        output << ' ';
                    }
                }

                for(int i = 0; i < len; ++i) {
                    if(i < (int)toPrint.size()) {
                        output << toPrint[i];
                    }
                    else {
                        output << '0';
                    }
                }
            }
            else {
                for(int i = 0; i < len; ++i) {
                    if(i < (int)toPrint.size()) {
                        output << toPrint[i];
                    }
                    else {
                        output << '0';
                    }
                }

                for(int i = 0; i < spaces - len; ++i) {
                    if(fillZero) {
                        output << '0';
                    }
                    else {
                        output << ' ';
                    }
                }
            }
        }

        void printFormattedStringRaw(std::ostream& output, const char* toPrint, int cap, int len) {
            std::string buffer(toPrint, toPrint + len);

            for(int i = 0; i < len; ++i) {
                char next = toPrint[i];

                if(cap == CAPITALIZEDFORMAT_CAPS) {
                    next = (char)std::toupper(next);
                }
                else if(cap == CAPITALIZEDFORMAT_LOWER) {
                    next = (char)std::tolower(next);
                }

                buffer[i] = next;
            }

            output << buffer;
        }

        void printFormattedString(std::ostream& output, const char* toPrint, int cap, bool right, int space) {
            int len = (int)strlen(toPrint);

            if(right) {
                for(int i = 0; i < space - len; ++i) {
                    output << ' ';
                }

                printFormattedStringRaw(output, toPrint, cap, len);
            }
            else {
                printFormattedStringRaw(output, toPrint, cap, len);

                for(int i = 0; i < space - len; ++i) {
                    output << ' ';
                }
            }
        }

        void printFormattedInteger(std::ostream& output, uint64_t value, bool right, int space, int outputFormat, bool unsignedMark, bool fillZero, bool longlong) {
            char buffer[129];
            buffer[128] = 0;
            int len = 0;

            //print 32 bit integer
            if(outputFormat == OUTPUTFORMAT_DECIMAL) {
                if(unsignedMark) {
                    if(longlong) {
                        len = SPRINTF(buffer, "%llu", (unsigned long long int)value);
                    }
                    else {
                        len = SPRINTF(buffer, "%u", (int)value);
                    }
                }
                else {
                    if(longlong) {
                        len = SPRINTF(buffer, "%lld", (long long int)value);
                    }
                    else {
                        len = SPRINTF(buffer, "%d", (int)value);
                    }
                }
            }
            else if(outputFormat == OUTPUTFORMAT_HEX) {
                len = printHexToBuffer(buffer, value, false);
            }
            else if(outputFormat == OUTPUTFORMAT_UPPERHEX) {
                len = printHexToBuffer(buffer, value, true);
            }
            else if(outputFormat == OUTPUTFORMAT_BIN) {
                len = printBinToBuffer(buffer, value);
            }

            if(right) {
                for(int i = 0; i < space - len; ++i) {
                    if(fillZero) {
                        output << "0";
                    }
                    else {
                        output << " ";
                    }
                }

                output << buffer;
            }
            else {
                output << buffer;

                for(int i = 0; i < space - len; ++i) {
                    if(fillZero) {
                        output << "0";
                    }
                    else {
                        output << " ";
                    }
                }
            }
        }

        void printFormattedChar(std::ostream& output, char value, int cap, bool right, int space) {
            if(cap == 1) {
                value = (char)std::toupper(value);
            }
            else if(cap == 2) {
                value = (char)std::tolower(value);
            }

            if(right) {
                for(int i = 0; i < space - 1; ++i) {
                    output << " ";
                }

                output << value;
            }
            else {
                output << value;

                for(int i = 0; i < space - 1; ++i) {
                    output << " ";
                }
            }
        }

        void printArgument(std::ostream& output, const char* format, int& index, va_list& args) {
            int capitalized = CAPITALIZEDFORMAT_NONE;
            bool rightAligned = false;
            bool unsignedValue = false;
            std::string type;
            std::string formattedString;
            int setSpaceCount = -1;
            int setSpaceCount_dec = -1;
            bool fillZero = false;
            int outputFormat = OUTPUTFORMAT_DECIMAL;

            collectFormattingOptions(format, index, capitalized, rightAligned, unsignedValue, type, setSpaceCount, setSpaceCount_dec, fillZero, outputFormat, formattedString, '}');

            //lookup table to determine the variable type
            std::map<std::string, Token::TokenType>::iterator t = reserves.find(type);
            Token::TokenType argumentType;

            if(t != reserves.end()) {
                //its actually a reserve word
                argumentType = t->second;

                if(argumentType == Token::TokenType::SIGNED_CHAR) {
                    //collect char from VA args and print
                    char ch = (char)va_arg(args, int);
                    printFormattedChar(output, ch, capitalized, rightAligned, setSpaceCount);
                }
                else if(argumentType == Token::TokenType::SIGNED_INT) {
                    uint32_t val = va_arg(args, uint32_t);
                    printFormattedInteger(output, val, rightAligned, setSpaceCount, outputFormat, unsignedValue || type[0] == 'u', fillZero, false);
                }
                else if(argumentType == Token::TokenType::SIGNED_LONG) {
                    uint64_t val = va_arg(args, uint64_t);
                    printFormattedInteger(output, val, rightAligned, setSpaceCount, outputFormat, unsignedValue || type[0] == 'u', fillZero, true);
                }
                else if(argumentType == Token::TokenType::FLOAT) {
                    double val = va_arg(args, double);
                    printFormattedFloat(output, val, rightAligned, setSpaceCount, setSpaceCount_dec, fillZero);
                }
                else if(argumentType == Token::TokenType::STRING) {
                    const char* strValue = (const char*)va_arg(args, void*);
                    printFormattedString(output, strValue, capitalized, rightAligned, setSpaceCount);
                }
                else {
                    //unrecognized type, ignore it
                    //an unspecified type is fine, just means we won't have to pull out a parameter, it could always be a formatted string
                }
            }
            //otherwise the type was not recognized, ignore it
        }

        /**
         * Prints an argument and returns whether the entire string has been finished or not
         * Prints each character until a special one is found:
         * [] indicates a variable
         * {} indicates an ordered parameter in the va_list
         * @param format the format for printing
         * @param index the current index in the string
         * @param args the argument list
         * @return true if there is more to the string
         * */
        bool printNext(std::ostream& outputStream, const char* format, int& index, va_list& args) {
            int startIndex = index;

            switch(format[index]) {
                case '\\':
                    if(format[index + 1]) {
                        index++;
                        outputStream << format[index];
                    }
                    break;
                case '[':
                    printVariable(outputStream, format, index, args);
                    break;
                case '{':
                    printArgument(outputStream, format, index, args);
                    break;
                default:
                    while(format[index] != '[' && format[index] != '{' && format[index] != '}' && format[index] != ']' && format[index] != '\\' && format[index]) {
                        index++;
                    }
                    outputStream << std::string(format + startIndex, format + index);
                    index--;
                    break;
            }

            index++;
            return format[index];
        }
            
        /**
         * Prints the prefix
         * Pretty much the same thing as printNext, but it will only accept variables
         * */
        bool printNextPrefix(std::ostream& outputStream, const char* format, int& index, va_list& args) {
            int startIndex = index;

            switch (format[index]) {
                case '\\':
                    if(format[index + 1]) {
                        index++;
                        outputStream << format[index];
                    }
                    break;
                case '[':
                    printVariable(outputStream, format, index, args);
                    break;
                default:
                    while(format[index] != '[' && format[index] != ']' && format[index] != '\\' && format[index]) {
                        index++;
                    }

                    outputStream << std::string(format + startIndex, format + index);
                    index--;
                    break;
            }

            index++;
            return format[index];
        }

        //raw values for total time
        double timeVars[5] = { 0 };
        long long totalNanoseconds = 0;

        //raw values for total time
        double elapsedTimeVars[5] = { 0 };

        /**
         * Stores the number of messages at each level
         * messageCount[LEVEL_COUNT] is the total number of messages sent to the debugger
         * */
        long long messageCount[(int)Level::LEVEL_COUNT + 1];
        long long currentMessageCount = 0;

        /*
        * prints to the output stream the debug format
        */
        void printPrefix(std::ostream& output, Level level, va_list& args) {
            const char* format = prefixFormat[(int)level].c_str();
            int len = (int)prefixFormat[(int)level].size();
            int formatIndex = 0;
            int previousFormatIndex = -1;

            //process and print arguments
            while(printNextPrefix(output, format, formatIndex, args) && formatIndex < len && formatIndex != previousFormatIndex) {
                previousFormatIndex = formatIndex;
            }
        }

        /**
         * Struct containing information for a debug var
         * @author Bryce Young
         * */
        struct DebugVar {
            public:

                DebugVar(DebugVarType type, void* value, bool readOnly = false) 
                    :type(type),
                    value(value),
                    readonly(readOnly)
                {
                }

                DebugVar(const DebugVar& var)
                    :type(var.type),
                    value(var.value)
                {
                }

                DebugVar& operator=(const DebugVar& var) {
                    this->type = var.type;
                    this->value = var.value;
                    return *this;
                }

                char getChar() {
                    return *(char*)value;
                }

                float getFloat32() {
                    return *(float*)value;
                }

                double getFloat64() {
                    return *(double*)value;
                }

                int32_t getInt32() {
                    return *(uint32_t*)value;
                }

                int64_t getInt64() {
                    return *(uint64_t*)value;
                }

                const char* getString() {
                    return &(*(std::string*)value)[0];
                }

                DebugVarType getType() const {
                    return type;
                }

                bool getReadonly() {
                    return readonly;
                }

            private:
                DebugVarType type;
                void* value;
                bool readonly = false;
        };

        bool isNum(const char* format, int& index) {
            return (format[index] >= '0' && format[index] <= '9');
        }

        bool isAlpha(const char* format, int& index) {
            return ((format[index] >= 'a' && format[index] <= 'z') || (format[index] >= 'A' && format[index] <= 'Z'));
        }

        bool isWhiteSpace(const char* format, int& index) {
            return format[index] == ' ' || format[index] == '\t';
        }

        void skipWhitespace(const char* format, int& index) {
            while(isWhiteSpace(format, index)) {
                index++;
            }
        }

        //current output level
        Level level;

        //the logger's name
        std::string loggerName;

        //special characters as internal variables
        char specialCharacters[6] = "{}[]\\";

        //list of every usable variable
        std::map<std::string, DebugVar> variables;

        //list of reserves
        std::map<std::string, Token::TokenType> reserves;

        //an array of level names
        std::string levelNames[(int)Level::LEVEL_COUNT + 1];

        /**
         * A string representing the prefix of each debug
         * Can access internal variables and is updated on each print
         * You can use a different format for each debug level if you want, but you have to specify it with specific function calls
         * Calling the funciton to set the prefix format globally will overwrite it for all level counts!
         * */
        std::string prefixFormat[(int)Level::LEVEL_COUNT];

        /**
         * Timer to keep track of time and changes in it
         * */
        Timer timer;

        /**
         * Whether it prints colors to the scren
         * */
        bool enableColor = true;
        bool valueOnlyColor = false;
        std::string previousColor = "";

        /**
         * The target output stream
         * */
        std::ostream* targetStream;
};

#endif
