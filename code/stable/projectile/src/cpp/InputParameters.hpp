/** \file InputParameters.hpp
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides the structure for holding input values
*/
#ifndef InputParameters_h
#define InputParameters_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

/** \brief Structure for holding the input values
*/
class InputParameters {
    public:
        double v_launch;
        double theta;
        double p_target;
        
        /** \brief Initializes input object by reading inputs and checking physical constraints and software constraints on the input
            \param filename name of the input file
        */
        InputParameters(string filename);
    
    private:
        /** \brief Reads input from a file with the given file name
            \param filename name of the input file
        */
        void get_input(string filename);
        /** \brief Verifies that input values satisfy the physical constraints and software constraints
        */
        void input_constraints();
};

#endif
