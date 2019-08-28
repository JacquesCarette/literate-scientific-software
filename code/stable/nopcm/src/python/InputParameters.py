## \file InputParameters.py
# \author Thulasi Jegatheesan
# \brief Provides the function for reading inputs and the function for checking the physical constraints and software constraints on the input
from __future__ import print_function
import sys
import math

import Constants

## \brief Reads input from a file with the given file name
# \param filename name of the input file
# \return heating coil surface area: area covered by the outermost layer of the coil (m^2)
# \return specific heat capacity of water: the amount of energy required to raise the temperature of a given unit mass of water by a given amount (J/(kg degreeC))
# \return convective heat transfer coefficient between coil and water: the convective heat transfer coefficient that models the thermal flux from the coil to the surrounding water (W/(m^2 degreeC))
# \return initial temperature: the temperature at the beginning of the simulation (degreeC)
# \return final time: the amount of time elapsed from the beginning of the simulation to its conclusion (s)
# \return length of tank: the length of the tank (m)
# \return temperature of the heating coil: the average kinetic energy of the particles within the coil (degreeC)
# \return time step for simulation: the finite discretization of time used in the numerical method for solving the computational model (s)
# \return density of water: nass per unit volume of water (kg/m^3)
# \return diameter of tank: the diameter of the tank (m)
# \return absolute tolerance
# \return relative tolerance
# \return temperature of the water: the average kinetic energy of the particles within the water (degreeC)
# \return change in heat energy in the water: change in thermal energy within the water (J)
def get_input(filename):
    infile = open(filename, "r")
    infile.readline()
    A_C = float(infile.readline())
    infile.readline()
    C_W = float(infile.readline())
    infile.readline()
    h_C = float(infile.readline())
    infile.readline()
    T_init = float(infile.readline())
    infile.readline()
    t_final = float(infile.readline())
    infile.readline()
    L = float(infile.readline())
    infile.readline()
    T_C = float(infile.readline())
    infile.readline()
    t_step = float(infile.readline())
    infile.readline()
    rho_W = float(infile.readline())
    infile.readline()
    D = float(infile.readline())
    infile.readline()
    A_tol = float(infile.readline())
    infile.readline()
    R_tol = float(infile.readline())
    infile.readline()
    T_W = float(infile.readline())
    infile.readline()
    E_W = float(infile.readline())
    infile.close()
    
    return A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, A_tol, R_tol, T_W, E_W

## \brief Verifies that input values satisfy the physical constraints and software constraints
# \param A_C heating coil surface area: area covered by the outermost layer of the coil (m^2)
# \param C_W specific heat capacity of water: the amount of energy required to raise the temperature of a given unit mass of water by a given amount (J/(kg degreeC))
# \param h_C convective heat transfer coefficient between coil and water: the convective heat transfer coefficient that models the thermal flux from the coil to the surrounding water (W/(m^2 degreeC))
# \param T_init initial temperature: the temperature at the beginning of the simulation (degreeC)
# \param t_final final time: the amount of time elapsed from the beginning of the simulation to its conclusion (s)
# \param L length of tank: the length of the tank (m)
# \param T_C temperature of the heating coil: the average kinetic energy of the particles within the coil (degreeC)
# \param t_step time step for simulation: the finite discretization of time used in the numerical method for solving the computational model (s)
# \param rho_W density of water: nass per unit volume of water (kg/m^3)
# \param D diameter of tank: the diameter of the tank (m)
# \param T_W temperature of the water: the average kinetic energy of the particles within the water (degreeC)
# \param E_W change in heat energy in the water: change in thermal energy within the water (J)
# \param consts structure holding the constant values
def input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W, consts):
    if (not(A_C <= consts.A_C_max)) :
        print("Warning: ", end='')
        print("A_C has value ", end='')
        print(A_C, end='')
        print(" but suggested to be ", end='')
        print("below ", end='')
        print(consts.A_C_max, end='')
        print(" (A_C_max)", end='')
        print(".")
    if (not(consts.C_W_min < C_W and C_W < consts.C_W_max)) :
        print("Warning: ", end='')
        print("C_W has value ", end='')
        print(C_W, end='')
        print(" but suggested to be ", end='')
        print("between ", end='')
        print(consts.C_W_min, end='')
        print(" (C_W_min)", end='')
        print(" and ", end='')
        print(consts.C_W_max, end='')
        print(" (C_W_max)", end='')
        print(".")
    if (not(consts.h_C_min <= h_C and h_C <= consts.h_C_max)) :
        print("Warning: ", end='')
        print("h_C has value ", end='')
        print(h_C, end='')
        print(" but suggested to be ", end='')
        print("between ", end='')
        print(consts.h_C_min, end='')
        print(" (h_C_min)", end='')
        print(" and ", end='')
        print(consts.h_C_max, end='')
        print(" (h_C_max)", end='')
        print(".")
    if (not(t_final < consts.t_final_max)) :
        print("Warning: ", end='')
        print("t_final has value ", end='')
        print(t_final, end='')
        print(" but suggested to be ", end='')
        print("below ", end='')
        print(consts.t_final_max, end='')
        print(" (t_final_max)", end='')
        print(".")
    if (not(consts.L_min <= L and L <= consts.L_max)) :
        print("Warning: ", end='')
        print("L has value ", end='')
        print(L, end='')
        print(" but suggested to be ", end='')
        print("between ", end='')
        print(consts.L_min, end='')
        print(" (L_min)", end='')
        print(" and ", end='')
        print(consts.L_max, end='')
        print(" (L_max)", end='')
        print(".")
    if (not(consts.rho_W_min < rho_W and rho_W <= consts.rho_W_max)) :
        print("Warning: ", end='')
        print("rho_W has value ", end='')
        print(rho_W, end='')
        print(" but suggested to be ", end='')
        print("between ", end='')
        print(consts.rho_W_min, end='')
        print(" (rho_W_min)", end='')
        print(" and ", end='')
        print(consts.rho_W_max, end='')
        print(" (rho_W_max)", end='')
        print(".")
    if (not(consts.AR_min <= D and D <= consts.AR_max)) :
        print("Warning: ", end='')
        print("D has value ", end='')
        print(D, end='')
        print(" but suggested to be ", end='')
        print("between ", end='')
        print(consts.AR_min, end='')
        print(" (AR_min)", end='')
        print(" and ", end='')
        print(consts.AR_max, end='')
        print(" (AR_max)", end='')
        print(".")
    
    if (not(A_C > 0)) :
        print("Warning: ", end='')
        print("A_C has value ", end='')
        print(A_C, end='')
        print(" but suggested to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")
    if (not(C_W > 0)) :
        print("Warning: ", end='')
        print("C_W has value ", end='')
        print(C_W, end='')
        print(" but suggested to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")
    if (not(h_C > 0)) :
        print("Warning: ", end='')
        print("h_C has value ", end='')
        print(h_C, end='')
        print(" but suggested to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")
    if (not(0 < T_init and T_init < 100)) :
        print("Warning: ", end='')
        print("T_init has value ", end='')
        print(T_init, end='')
        print(" but suggested to be ", end='')
        print("between ", end='')
        print(0, end='')
        print(" and ", end='')
        print(100, end='')
        print(".")
    if (not(t_final > 0)) :
        print("Warning: ", end='')
        print("t_final has value ", end='')
        print(t_final, end='')
        print(" but suggested to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")
    if (not(L > 0)) :
        print("Warning: ", end='')
        print("L has value ", end='')
        print(L, end='')
        print(" but suggested to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")
    if (not(0 < T_C and T_C < 100)) :
        print("Warning: ", end='')
        print("T_C has value ", end='')
        print(T_C, end='')
        print(" but suggested to be ", end='')
        print("between ", end='')
        print(0, end='')
        print(" and ", end='')
        print(100, end='')
        print(".")
    if (not(0 < t_step and t_step < t_final)) :
        print("Warning: ", end='')
        print("t_step has value ", end='')
        print(t_step, end='')
        print(" but suggested to be ", end='')
        print("between ", end='')
        print(0, end='')
        print(" and ", end='')
        print(t_final, end='')
        print(" (t_final)", end='')
        print(".")
    if (not(rho_W > 0)) :
        print("Warning: ", end='')
        print("rho_W has value ", end='')
        print(rho_W, end='')
        print(" but suggested to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")
    if (not(D > 0)) :
        print("Warning: ", end='')
        print("D has value ", end='')
        print(D, end='')
        print(" but suggested to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")
    if (not(T_init <= T_W and T_W <= T_C)) :
        print("Warning: ", end='')
        print("T_W has value ", end='')
        print(T_W, end='')
        print(" but suggested to be ", end='')
        print("between ", end='')
        print(T_init, end='')
        print(" (T_init)", end='')
        print(" and ", end='')
        print(T_C, end='')
        print(" (T_C)", end='')
        print(".")
    if (not(E_W >= 0)) :
        print("Warning: ", end='')
        print("E_W has value ", end='')
        print(E_W, end='')
        print(" but suggested to be ", end='')
        print("above ", end='')
        print(0, end='')
        print(".")


