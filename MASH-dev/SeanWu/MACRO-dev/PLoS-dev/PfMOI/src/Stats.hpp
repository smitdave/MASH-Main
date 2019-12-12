/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  on-line summary statistics calculations
 *  code source: https://www.johndcook.com/blog/standard_deviation/
 *
 *  Sean Wu (slwu89@berkeley.edu)
 *  August 2019
*/

#ifndef STATS_HPP
#define STATS_HPP

#include <cmath>


/* ################################################################################
 * class definition
################################################################################ */

class RunningStat {
public:

  /* constructor & destructor */
  RunningStat() : m_n(0) {};
  ~RunningStat() = default;

  void Clear() { m_n = 0; };

  // add a new data point
  void Push(double x){
    m_n++;

    // See Knuth TAOCP vol 2, 3rd edition, page 232
    if (m_n == 1)
    {
        m_oldM = m_newM = x;
        m_oldS = 0.0;
    }
    else
    {
        m_newM = m_oldM + (x - m_oldM)/m_n;
        m_newS = m_oldS + (x - m_oldM)*(x - m_newM);

        // set up for next iteration
        m_oldM = m_newM;
        m_oldS = m_newS;
    }
  };

  int NumDataValues() const { return m_n; };

  double Mean() const { return (m_n > 0) ? m_newM : 0.0; };

  double Variance() const { return ( (m_n > 1) ? m_newS/(m_n - 1) : 0.0 ); };

  double StandardDeviation() const { return std::sqrt(Variance()); };

private:

  int m_n;

  double m_oldM, m_newM, m_oldS, m_newS;
};

#endif
